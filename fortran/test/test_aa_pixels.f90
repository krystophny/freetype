program test_aa_pixels
  use ft_types
  use ft_bitmap_mod
  use ft_outline_mod
  use ft_raster
  use ft_raster_types
  use ft_geometry, only: FT_Vector
  use ft_bitmap_io
  implicit none
  
  type(FT_Outline), target :: outline
  type(FT_Bitmap), target :: bitmap
  type(FT_Raster_State) :: raster
  type(FT_Raster_Params) :: params
  integer(FT_Error) :: error
  logical :: success
  integer :: i, j, pixel_count
  
  print '("Testing AA pixel rendering...")'
  
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
  
  ! Count non-zero pixels
  pixel_count = 0
  do i = 1, bitmap%rows
    do j = 1, bitmap%width
      if (bitmap%buffer((i-1)*bitmap%width + j) /= 0) then
        pixel_count = pixel_count + 1
      end if
    end do
  end do
  
  print '("Pixels rendered: ", I0, " out of ", I0)', pixel_count, bitmap%width * bitmap%rows
  
  if (pixel_count > 0) then
    print '("✓ AA is rendering pixels")'
    ! Save output for inspection
    success = ft_bitmap_write_png(bitmap, "aa_test_output.png", error)
    if (success) then
      print '("Saved aa_test_output.png for inspection")'
    end if
  else
    print '("✗ AA is NOT rendering any pixels")'
  end if
  
  ! Cleanup
  call ft_raster_done(raster)
  call ft_bitmap_done(bitmap)
  call ft_outline_done(outline)
  
end program test_aa_pixels