module ft_raster
  use ft_types
  use ft_raster_types
  use ft_geometry, only: FT_Vector, FT_BBox
  use ft_bitmap_mod
  use ft_outline_mod
  use ft_outline_decompose_mod
  use ft_scanline
  use ft_scanline_simple
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  private
  
  ! Public functions
  public :: ft_raster_new
  public :: ft_raster_done
  public :: ft_raster_reset
  public :: ft_raster_set_cell
  public :: ft_raster_render_outline
  public :: ft_raster_render_outline_scanline
  
  ! Constants
  integer, parameter :: FT_RASTER_CELL_POOL_SIZE = 1024
  integer, parameter :: FT_PIXEL_BITS = 8     ! 8 subpixels per pixel
  integer, parameter :: FT_ONE_PIXEL = ishft(1, FT_PIXEL_BITS)
  
contains

  ! Create a new rasterizer state
  function ft_raster_new(raster, error) result(success)
    type(FT_Raster_State), intent(out) :: raster
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    success = .false.
    error = FT_Err_Ok
    
    ! Initialize raster state
    raster%min_ex = 0
    raster%max_ex = 0
    raster%min_ey = 0
    raster%max_ey = 0
    raster%count_ey = 0
    raster%x = 0
    raster%y = 0
    raster%flags = 0
    raster%band_size = 0
    raster%band_shoot = 0
    raster%num_cells = 0
    raster%cell_index = 0
    
    ! Clear pointers
    raster%cell => null()
    raster%cells => null()
    raster%ycells => null()
    raster%outline => null()
    raster%target => null()
    
    ! Allocate cell pool
    allocate(raster%cells(FT_RASTER_CELL_POOL_SIZE), stat=error)
    if (error /= 0) then
      error = FT_Err_Out_Of_Memory
      return
    end if
    
    raster%num_cells = FT_RASTER_CELL_POOL_SIZE
    
    error = FT_Err_Ok
    success = .true.
    
  end function ft_raster_new
  
  ! Free rasterizer memory
  subroutine ft_raster_done(raster)
    type(FT_Raster_State), intent(inout) :: raster
    
    if (associated(raster%cells)) then
      deallocate(raster%cells)
      raster%cells => null()
    end if
    
    if (associated(raster%ycells)) then
      deallocate(raster%ycells)
      raster%ycells => null()
    end if
    
    raster%num_cells = 0
    raster%cell_index = 0
    
  end subroutine ft_raster_done
  
  ! Reset rasterizer for new outline
  function ft_raster_reset(raster, outline, target, error) result(success)
    type(FT_Raster_State), intent(inout) :: raster
    type(FT_Outline), target, intent(in) :: outline
    type(FT_Bitmap), target, intent(in) :: target
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: height
    
    success = .false.
    error = FT_Err_Ok
    
    ! Set outline and target
    raster%outline => outline
    raster%target => target
    
    ! Get outline bounding box
    if (.not. ft_outline_get_cbox(outline, raster%cbox, error)) then
      return
    end if
    
    ! Convert to pixel coordinates
    raster%min_ex = int(raster%cbox%xMin / FT_ONE_PIXEL)
    raster%min_ey = int(raster%cbox%yMin / FT_ONE_PIXEL)
    raster%max_ex = int((raster%cbox%xMax + FT_ONE_PIXEL - 1) / FT_ONE_PIXEL)
    raster%max_ey = int((raster%cbox%yMax + FT_ONE_PIXEL - 1) / FT_ONE_PIXEL)
    
    ! Clip to target bitmap
    if (raster%min_ex < 0) raster%min_ex = 0
    if (raster%min_ey < 0) raster%min_ey = 0
    if (raster%max_ex > target%width) raster%max_ex = target%width
    if (raster%max_ey > target%rows) raster%max_ey = target%rows
    
    ! Calculate height
    raster%count_ey = raster%max_ey - raster%min_ey
    
    if (raster%count_ey <= 0) then
      error = FT_Err_Invalid_Outline
      return
    end if
    
    ! Allocate y-sorted cell lists
    if (associated(raster%ycells)) then
      deallocate(raster%ycells)
    end if
    
    height = raster%count_ey
    allocate(raster%ycells(height), stat=error)
    if (error /= 0) then
      error = FT_Err_Out_Of_Memory
      return
    end if
    
    ! Initialize cell lists
    raster%ycells = FT_Raster_Cell(0, 0, 0, null())
    
    ! Reset cell pool
    raster%cell_index = 1
    raster%cell => null()
    
    error = FT_Err_Ok
    success = .true.
    
  end function ft_raster_reset
  
  ! Set or update a cell at given position
  subroutine ft_raster_set_cell(raster, x, y)
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
      raster%cell%cover = 0
      raster%cell%area = 0
      
      ! Insert into list
      raster%cell%next => cell
      prev_cell%next => raster%cell
    end if
    
  end subroutine ft_raster_set_cell
  
  ! Main outline rendering function (placeholder)
  function ft_raster_render_outline(raster, params, error) result(success)
    type(FT_Raster_State), intent(inout) :: raster
    type(FT_Raster_Params), intent(in) :: params
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    type(FT_Outline), pointer :: outline
    type(FT_Bitmap), pointer :: target
    
    success = .false.
    error = FT_Err_Ok
    
    ! Get outline from params (for now assume it's in raster state)
    outline => raster%outline
    target => params%target
    
    if (.not. associated(outline) .or. .not. associated(target)) then
      error = FT_Err_Invalid_Argument
      return
    end if
    
    ! Reset rasterizer
    if (.not. ft_raster_reset(raster, outline, target, error)) then
      return
    end if
    
    ! Clear the target bitmap
    call ft_bitmap_clear(target)
    
    ! Decompose outline and draw lines
    if (.not. ft_outline_decompose_simple(outline, raster, error)) then
      return
    end if
    
    ! Render cells to bitmap
    call render_cells_to_bitmap(raster)
    
    error = FT_Err_Ok
    success = .true.
    
  end function ft_raster_render_outline
  
  ! Render accumulated cells to bitmap
  subroutine render_cells_to_bitmap(raster)
    type(FT_Raster_State), intent(inout) :: raster
    
    integer :: y, x, ey
    type(FT_Raster_Cell), pointer :: cell
    logical :: pixel_on
    
    if (.not. associated(raster%target)) return
    if (.not. associated(raster%ycells)) return
    
    ! Process each scanline
    do y = raster%min_ey, raster%max_ey - 1
      ey = y - raster%min_ey + 1
      
      ! Process cells in this scanline
      cell => raster%ycells(ey)%next
      
      do while (associated(cell))
        x = cell%x
        
        ! Simple rule: if cell has any coverage, turn on pixel
        pixel_on = (cell%cover /= 0 .or. cell%area /= 0)
        
        ! Set pixel in bitmap
        if (pixel_on) then
          call ft_bitmap_set_pixel(raster%target, x, y, .true.)
        end if
        
        cell => cell%next
      end do
    end do
    
  end subroutine render_cells_to_bitmap
  
  ! Render outline using scanline conversion
  function ft_raster_render_outline_scanline(outline, bitmap, error) result(success)
    type(FT_Outline), target, intent(in) :: outline
    type(FT_Bitmap), target, intent(inout) :: bitmap
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    type(FT_Edge_Table) :: edge_table
    integer :: i, j, first, last
    integer :: x0, y0, x1, y1
    
    success = .false.
    error = FT_Err_Ok
    
    ! Clear bitmap
    call ft_bitmap_clear(bitmap)
    
    ! Create edge table
    if (.not. ft_edge_table_new(0, bitmap%rows - 1, edge_table, error)) then
      return
    end if
    
    ! Build edge table from outline
    first = 1
    do i = 1, outline%n_contours
      last = outline%contours(i) + 1  ! Convert to 1-based
      
      ! Add edges for this contour
      do j = first, last - 1
        x0 = int(outline%points(j)%x / 256)
        y0 = int(outline%points(j)%y / 256)
        x1 = int(outline%points(j + 1)%x / 256)
        y1 = int(outline%points(j + 1)%y / 256)
        
        call ft_edge_table_add_line(edge_table, x0, y0, x1, y1)
      end do
      
      ! Close contour
      x0 = int(outline%points(last)%x / 256)
      y0 = int(outline%points(last)%y / 256)
      x1 = int(outline%points(first)%x / 256)
      y1 = int(outline%points(first)%y / 256)
      
      call ft_edge_table_add_line(edge_table, x0, y0, x1, y1)
      
      first = last + 1
    end do
    
    ! Sort edges
    call ft_edge_table_sort(edge_table)
    
    ! Convert to bitmap
    call ft_scanline_convert(edge_table, bitmap)
    
    ! Cleanup
    call ft_edge_table_done(edge_table)
    
    error = FT_Err_Ok
    success = .true.
    
  end function ft_raster_render_outline_scanline

end module ft_raster