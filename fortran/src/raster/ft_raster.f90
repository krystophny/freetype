module ft_raster
  use ft_types
  use ft_raster_types
  use ft_geometry, only: FT_Vector, FT_BBox
  use ft_bitmap_mod, only: FT_Bitmap, ft_bitmap_set_pixel, ft_bitmap_set_pixel_gray, &
                           ft_bitmap_clear, FT_PIXEL_MODE_GRAY
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
  integer, parameter :: FT_ONE_PIXEL = ishft(1, FT_PIXEL_BITS)  ! 256
  
  ! Fill rule constants
  integer, parameter :: FT_FILL_EVEN_ODD = 256  ! 0x100
  integer, parameter :: FT_FILL_NON_ZERO = int(z'80000000')  ! INT_MIN
  
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
    
    ! Convert to pixel coordinates (26.6 format uses 64 units per pixel)
    raster%min_ex = int(raster%cbox%xMin / 64)
    raster%min_ey = int(raster%cbox%yMin / 64)
    raster%max_ex = int((raster%cbox%xMax + 63) / 64)
    raster%max_ey = int((raster%cbox%yMax + 63) / 64)
    
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
  
  ! Main outline rendering function
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
    
    ! Store flags for rendering mode
    raster%flags = params%flags
    
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
  
  ! Render accumulated cells to bitmap using FreeType sweep algorithm
  subroutine render_cells_to_bitmap(raster)
    type(FT_Raster_State), intent(inout) :: raster
    
    integer :: y, x, ey, fill_x
    type(FT_Raster_Cell), pointer :: cell
    integer :: area, coverage, cover
    
    if (.not. associated(raster%target)) return
    if (.not. associated(raster%ycells)) return
    
    ! Process each scanline with proper FreeType sweep algorithm
    do y = raster%min_ey, raster%max_ey - 1
      ey = y - raster%min_ey + 1
      
      ! Initialize running coverage accumulator and position
      cover = 0
      x = raster%min_ex
      
      ! Process cells in this scanline
      cell => raster%ycells(ey)%next
      
      ! print '("DEBUG sweep y=", I0, " initial cover=", I0, " x=", I0)', y, cover, x
      
      do while (associated(cell))
        ! print '("DEBUG cell: x=", I0, " cover=", I0, " area=", I0, " running_cover=", I0)', cell%x, cell%cover, cell%area, cover
        ! Check rendering mode
        if (iand(raster%flags, FT_RASTER_FLAG_AA) /= 0 .and. &
            raster%target%pixel_mode == FT_PIXEL_MODE_GRAY) then
          ! ANTIALIASED RENDERING with proper FreeType algorithm
          
          ! INTERIOR FILLING: Fill span between edges if inside shape
          if (cover /= 0 .and. cell%x > x) then
            ! For interior pixels, use the full winding number times pixel area
            ! FreeType uses (winding_number * ONE_PIXEL * ONE_PIXEL) for interior
            ! Our cover is already scaled by 4, so multiply by (256 * 256 / 4) = 16384
            call ft_fill_rule(coverage, cover * 16384, FT_FILL_NON_ZERO)
            
            ! DEBUG: Uncomment to debug interior filling
            ! print '("DEBUG: Interior fill from x=", I0, " to x=", I0, " coverage=", I0)', x, cell%x - 1, coverage
            
            ! Fill horizontal span from x to cell%x - 1
            do fill_x = x, cell%x - 1
              if (fill_x >= raster%min_ex .and. fill_x < raster%max_ex) then
                call ft_bitmap_set_pixel_gray(raster%target, fill_x, y, coverage)
              end if
            end do
          end if
          
          ! ACCUMULATE COVERAGE: Update running total BEFORE rendering edge
          ! Note: Our coordinates are in 26.6 format, need to scale to FreeType's 8.8 format
          ! Convert: 26.6 -> 8.8 by multiplying by 4 (64->256 scale factor)
          ! DEBUG: Uncomment to debug cover accumulation
          ! print '("DEBUG: y=", I0, " x=", I0, " old_cover=", I0, " cell_cover=", I0)', y, cell%x, cover, cell%cover
          cover = cover + cell%cover * 4
          ! print '("DEBUG: new_cover=", I0)', cover
          
          ! EDGE RENDERING: Render the edge pixel itself
          if (cell%area /= 0 .and. cell%x >= raster%min_ex .and. cell%x < raster%max_ex) then
            ! Apply FreeType FT_FILL_RULE to the edge area
            call ft_fill_rule(coverage, cell%area, FT_FILL_NON_ZERO)
            call ft_bitmap_set_pixel_gray(raster%target, cell%x, y, coverage)
          end if
          
        else
          ! MONOCHROME RENDERING (simple cell-based)
          if (cell%cover /= 0 .or. cell%area /= 0) then
            call ft_bitmap_set_pixel(raster%target, cell%x, y, .true.)
          end if
        end if
        
        ! Move to next position
        x = cell%x + 1
        cell => cell%next
      end do
      
      ! FINAL FILL: Fill remaining span if still inside shape (AA only)
      if (iand(raster%flags, FT_RASTER_FLAG_AA) /= 0 .and. &
          raster%target%pixel_mode == FT_PIXEL_MODE_GRAY .and. &
          cover /= 0) then
        ! Apply FreeType FT_FILL_RULE with proper scaling
        call ft_fill_rule(coverage, cover, FT_FILL_NON_ZERO)
        
        do fill_x = x, raster%max_ex - 1
          call ft_bitmap_set_pixel_gray(raster%target, fill_x, y, coverage)
        end do
      end if
    end do
    
  end subroutine render_cells_to_bitmap
  
  ! Apply FreeType FT_FILL_RULE - exact implementation of the C macro
  subroutine ft_fill_rule(coverage, area, fill)
    integer, intent(out) :: coverage
    integer, intent(in) :: area
    integer, intent(in) :: fill
    
    ! DEBUG: Print area value to understand what we're getting
    ! DEBUG: Uncomment to debug coverage calculation
    ! print '("DEBUG FT_FILL_RULE: area=", I0, " fill=", I0)', area, fill
    
    ! coverage = area >> (PIXEL_BITS * 2 + 1 - 8)
    ! With PIXEL_BITS = 8: area >> (8*2 + 1 - 8) = area >> 9
    coverage = ishft(area, -9)
    
    ! if (coverage & fill) coverage = ~coverage
    if (iand(coverage, fill) /= 0) then
      coverage = not(coverage)
    end if
    
    ! if (coverage > 255 && fill & INT_MIN) coverage = 255
    if (coverage > 255 .and. iand(fill, FT_FILL_NON_ZERO) /= 0) then
      coverage = 255
    end if
    
    ! Ensure coverage is in valid range [0, 255]
    coverage = max(0, min(255, coverage))
    
    ! DEBUG: Show final coverage
    ! print '("DEBUG: final coverage=", I0)', coverage
    
  end subroutine ft_fill_rule
  
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
      last = outline%contours(i) + 1  ! Convert from 0-based to 1-based
      
      ! Validate bounds
      if (last > outline%n_points) then
        error = FT_Err_Invalid_Outline
        call ft_edge_table_done(edge_table)
        return
      end if
      
      ! Add edges for this contour (but not the closing edge yet)
      do j = first, last - 1
        if (j + 1 <= last) then
          x0 = int(outline%points(j)%x / 64)
          y0 = int(outline%points(j)%y / 64)
          x1 = int(outline%points(j + 1)%x / 64)
          y1 = int(outline%points(j + 1)%y / 64)
          
          call ft_edge_table_add_line(edge_table, x0, y0, x1, y1)
        end if
      end do
      
      ! Close contour: connect last point back to first point of this contour
      if (first <= last) then
        x0 = int(outline%points(last)%x / 64)
        y0 = int(outline%points(last)%y / 64)
        x1 = int(outline%points(first)%x / 64)
        y1 = int(outline%points(first)%y / 64)
        
        call ft_edge_table_add_line(edge_table, x0, y0, x1, y1)
      end if
      
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