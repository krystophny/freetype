program debug_aa_cells
  use ft_types
  use ft_bitmap_mod
  use ft_outline_mod
  use ft_raster
  use ft_raster_types
  use ft_geometry, only: FT_Vector
  implicit none
  
  type(FT_Outline), target :: outline
  type(FT_Bitmap), target :: bitmap
  type(FT_Raster_State) :: raster
  type(FT_Raster_Params) :: params
  integer(FT_Error) :: error
  logical :: success
  integer :: i, j, y, ey, total_cells
  type(FT_Raster_Cell), pointer :: cell
  
  print '("Debugging AA cell accumulation...")'
  
  ! Create test outline (32x32 square)
  success = ft_outline_new(4, 1, outline, error)
  if (.not. success) stop 1
  
  outline%points(1)%x = 16 * 64
  outline%points(1)%y = 16 * 64
  outline%points(2)%x = 48 * 64
  outline%points(2)%y = 16 * 64
  outline%points(3)%x = 48 * 64
  outline%points(3)%y = 48 * 64
  outline%points(4)%x = 16 * 64
  outline%points(4)%y = 48 * 64
  
  outline%tags = FT_CURVE_TAG_ON
  outline%contours(1) = 3
  outline%n_points = 4
  outline%n_contours = 1
  
  ! Create bitmap and rasterizer
  success = ft_bitmap_new(64, 64, FT_PIXEL_MODE_GRAY, bitmap, error)
  if (.not. success) stop 1
  
  success = ft_raster_new(raster, error)
  if (.not. success) stop 1
  
  raster%outline => outline
  params%target => bitmap
  params%flags = FT_RASTER_FLAG_AA
  
  ! Render
  success = ft_raster_render_outline(raster, params, error)
  if (.not. success) then
    print '("ERROR: Render failed")'
    stop 1
  end if
  
  ! Check cell accumulation
  total_cells = 0
  if (associated(raster%ycells)) then
    do y = raster%min_ey, raster%max_ey - 1
      ey = y - raster%min_ey + 1
      
      ! Count cells in this scanline
      cell => raster%ycells(ey)%next
      do while (associated(cell))
        total_cells = total_cells + 1
        print '("Cell at (", I0, ",", I0, ") cover=", I0, " area=", I0)', &
              cell%x, y, cell%cover, cell%area
        cell => cell%next
      end do
    end do
  end if
  
  print '("Total cells accumulated: ", I0)', total_cells
  print '("Raster bounds: x=", I0, "-", I0, " y=", I0, "-", I0)', &
        raster%min_ex, raster%max_ex, raster%min_ey, raster%max_ey
  
  ! Cleanup
  call ft_raster_done(raster)
  call ft_bitmap_done(bitmap)
  call ft_outline_done(outline)
  
end program debug_aa_cells