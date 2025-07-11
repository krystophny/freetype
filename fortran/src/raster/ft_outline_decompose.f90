module ft_outline_decompose_mod
  use ft_types
  use ft_geometry, only: FT_Vector
  use ft_outline_mod
  use ft_raster_types
  use ft_bezier
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  private
  
  ! Public functions
  public :: ft_outline_decompose
  public :: ft_outline_decompose_simple
  
  ! Constants for fixed-point conversion
  integer, parameter :: UPSCALE_SHIFT = 8  ! Convert to 8.8 subpixel precision
  integer, parameter :: DOWNSCALE_SHIFT = 8
  
contains

  ! Decompose outline into move/line/curve operations
  function ft_outline_decompose(outline, funcs, user_data, error) result(success)
    type(FT_Outline), intent(in) :: outline
    type(FT_Outline_Funcs), intent(in) :: funcs
    type(c_ptr), intent(in) :: user_data
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: n, first, last, tag
    type(FT_Vector) :: v_start, v_last, v_control, v_middle
    type(FT_Vector) :: vec1, vec2
    logical :: needs_closing
    
    success = .false.
    error = FT_Err_Ok
    
    ! Check outline validity
    if (.not. ft_outline_check(outline, error)) then
      return
    end if
    
    ! Process each contour
    first = 1
    do n = 1, outline%n_contours
      last = outline%contours(n) + 1  ! Convert to 1-based
      
      ! Get first point
      v_start = outline%points(first)
      v_last = outline%points(last)
      
      ! Check if we need to close the contour
      tag = iand(int(outline%tags(last)), 3)
      
      ! If last point is off-curve and first point is on-curve, 
      ! start from last point
      if (tag == FT_CURVE_TAG_CUBIC .or. &
          (tag == FT_CURVE_TAG_CONIC .and. &
           iand(int(outline%tags(first)), 3) == FT_CURVE_TAG_ON)) then
        ! Start from the last point
        v_start = v_last
        last = last - 1
      else
        ! Skip first point, we'll use it as v_start
        first = first + 1
      end if
      
      ! Move to start point
      if (associated(funcs%move_to)) then
        error = funcs%move_to(v_start, user_data)
        if (error /= 0) return
      end if
      
      needs_closing = .true.
      
      ! Process points in contour
      do while (first <= last)
        tag = iand(int(outline%tags(first)), 3)
        
        select case (tag)
        case (FT_CURVE_TAG_ON)
          ! On-curve point - line to
          if (associated(funcs%line_to)) then
            error = funcs%line_to(outline%points(first), user_data)
            if (error /= 0) return
          end if
          
        case (FT_CURVE_TAG_CONIC)
          ! Conic Bezier
          v_control = outline%points(first)
          
          ! Check next point
          if (first < last) then
            first = first + 1
            tag = iand(int(outline%tags(first)), 3)
            
            if (tag == FT_CURVE_TAG_ON) then
              ! Single conic with explicit on-point
              if (associated(funcs%conic_to)) then
                error = funcs%conic_to(v_control, outline%points(first), user_data)
                if (error /= 0) return
              end if
            else if (tag == FT_CURVE_TAG_CONIC) then
              ! Two consecutive conics - create implicit on-point
              v_middle%x = (v_control%x + outline%points(first)%x) / 2
              v_middle%y = (v_control%y + outline%points(first)%y) / 2
              
              if (associated(funcs%conic_to)) then
                error = funcs%conic_to(v_control, v_middle, user_data)
                if (error /= 0) return
              end if
              
              ! Process next conic
              cycle
            end if
          else
            ! Close with conic to start
            if (associated(funcs%conic_to)) then
              error = funcs%conic_to(v_control, v_start, user_data)
              if (error /= 0) return
            end if
            needs_closing = .false.
          end if
          
        case (FT_CURVE_TAG_CUBIC)
          ! Cubic Bezier - need two control points
          if (first + 1 < last) then
            vec1 = outline%points(first)
            vec2 = outline%points(first + 1)
            first = first + 2
            
            if (iand(int(outline%tags(first)), 3) == FT_CURVE_TAG_ON) then
              if (associated(funcs%cubic_to)) then
                error = funcs%cubic_to(vec1, vec2, outline%points(first), user_data)
                if (error /= 0) return
              end if
            else
              ! Invalid cubic
              error = FT_Err_Invalid_Outline
              return
            end if
          else
            error = FT_Err_Invalid_Outline
            return
          end if
          
        end select
        
        first = first + 1
      end do
      
      ! Close contour if needed
      if (needs_closing .and. associated(funcs%line_to)) then
        error = funcs%line_to(v_start, user_data)
        if (error /= 0) return
      end if
      
      ! Move to next contour
      first = outline%contours(n) + 2  ! Next point after contour end
    end do
    
    success = .true.
    
  end function ft_outline_decompose
  
  ! Simple decomposition for rasterizer (lines only)
  function ft_outline_decompose_simple(outline, raster, error) result(success)
    type(FT_Outline), intent(in) :: outline
    type(FT_Raster_State), intent(inout) :: raster
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: n, first, last, i
    type(FT_Vector) :: v_start, v_current, v_next
    
    success = .false.
    error = FT_Err_Ok
    
    ! Process each contour
    first = 1
    do n = 1, outline%n_contours
      last = outline%contours(n) + 1  ! Convert to 1-based
      
      ! Get start point
      v_start = outline%points(first)
      v_current = v_start
      
      ! Process all points in contour
      do i = first + 1, last
        v_next = outline%points(i)
        
        ! Draw line from current to next
        call raster_line_to(raster, v_current, v_next)
        
        v_current = v_next
      end do
      
      ! Close contour
      if (v_current%x /= v_start%x .or. v_current%y /= v_start%y) then
        call raster_line_to(raster, v_current, v_start)
      end if
      
      ! Move to next contour
      first = last + 1
    end do
    
    success = .true.
    
  end function ft_outline_decompose_simple
  
  ! Internal line drawing for rasterizer
  subroutine raster_line_to(raster, from, to)
    type(FT_Raster_State), intent(inout) :: raster
    type(FT_Vector), intent(in) :: from, to
    
    integer :: x0, y0, x1, y1
    integer :: dx, dy, sx, sy, err, e2
    
    ! Convert to pixel coordinates (8.8 to integer)
    x0 = int(from%x / 256)
    y0 = int(from%y / 256)
    x1 = int(to%x / 256)
    y1 = int(to%y / 256)
    
    ! Bresenham's line algorithm
    dx = abs(x1 - x0)
    dy = abs(y1 - y0)
    
    if (x0 < x1) then
      sx = 1
    else
      sx = -1
    end if
    
    if (y0 < y1) then
      sy = 1
    else
      sy = -1
    end if
    
    err = dx - dy
    
    do
      ! Set cell at current position
      call set_raster_cell(raster, x0, y0)
      
      if (x0 == x1 .and. y0 == y1) exit
      
      e2 = 2 * err
      
      if (e2 > -dy) then
        err = err - dy
        x0 = x0 + sx
      end if
      
      if (e2 < dx) then
        err = err + dx
        y0 = y0 + sy
      end if
    end do
    
  end subroutine raster_line_to
  
  ! Set or update a cell at given position (internal version)
  subroutine set_raster_cell(raster, x, y)
    type(FT_Raster_State), intent(inout) :: raster
    integer, intent(in) :: x, y
    
    integer :: ey
    type(FT_Raster_Cell), pointer :: cell, prev_cell
    
    ! Check bounds
    if (y < raster%min_ey .or. y >= raster%max_ey) return
    if (x < raster%min_ex .or. x >= raster%max_ex) return
    
    ! Get y index
    ey = y - raster%min_ey + 1
    
    ! Find or create cell at this position
    cell => raster%ycells(ey)%next
    prev_cell => raster%ycells(ey)
    
    do while (associated(cell))
      if (cell%x == x) then
        ! Found existing cell
        raster%cell => cell
        cell%cover = cell%cover + 1
        return
      else if (cell%x > x) then
        ! Insert new cell before this one
        exit
      end if
      
      prev_cell => cell
      cell => cell%next
    end do
    
    ! Allocate new cell
    if (raster%cell_index <= raster%num_cells) then
      raster%cell => raster%cells(raster%cell_index)
      raster%cell_index = raster%cell_index + 1
      
      ! Initialize cell
      raster%cell%x = x
      raster%cell%cover = 1
      raster%cell%area = 0
      
      ! Insert into list
      raster%cell%next => cell
      prev_cell%next => raster%cell
    end if
    
  end subroutine set_raster_cell

end module ft_outline_decompose_mod