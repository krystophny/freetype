program debug_aa_simple
  use ft_types
  use ft_bitmap_mod
  use ft_outline_mod
  use ft_raster
  use ft_raster_types
  use ft_geometry, only: FT_Vector
  use ft_bitmap_io
  use, intrinsic :: iso_fortran_env, only: int8
  implicit none
  
  type(FT_Outline), target :: outline
  type(FT_Bitmap), target :: bitmap
  type(FT_Raster_State) :: raster
  type(FT_Raster_Params) :: params
  integer(FT_Error) :: error
  logical :: success
  integer :: i, j, val
  
  print '("Debug Simple AA")'
  print '("===============")'
  
  ! Create simple 4x4 rectangle
  success = ft_outline_new(4, 1, outline, error)
  if (.not. success) stop 1
  
  ! Small rectangle at fractional position
  outline%points(1) = FT_Vector(1 * 64 + 32, 1 * 64 + 32)  ! 1.5, 1.5
  outline%points(2) = FT_Vector(3 * 64 + 32, 1 * 64 + 32)  ! 3.5, 1.5
  outline%points(3) = FT_Vector(3 * 64 + 32, 3 * 64 + 32)  ! 3.5, 3.5
  outline%points(4) = FT_Vector(1 * 64 + 32, 3 * 64 + 32)  ! 1.5, 3.5
  
  outline%tags = FT_CURVE_TAG_ON
  outline%contours(1) = 3
  outline%n_points = 4
  outline%n_contours = 1
  
  ! Create 8x8 bitmap
  success = ft_bitmap_new(8, 8, FT_PIXEL_MODE_GRAY, bitmap, error)
  if (.not. success) stop 1
  
  success = ft_raster_new(raster, error)
  if (.not. success) stop 1
  
  ! Render with AA
  raster%outline => outline
  params%target => bitmap
  params%flags = FT_RASTER_FLAG_AA
  
  success = ft_raster_render_outline(raster, params, error)
  if (.not. success) stop 1
  
  ! Print the bitmap values
  print '("Bitmap contents (8x8):")'
  do i = 0, 7
    write(*, '(8I4)', advance='no') (int(bitmap%buffer(i*8+j+1)), j=0,7)
    print '()'
  end do
  
  ! Force some pixels to be visible
  print '()'
  print '("Setting some pixels manually for comparison:")'
  bitmap%buffer(2*8+2+1) = 64_int8   ! Row 2, Col 2
  bitmap%buffer(2*8+3+1) = 100_int8  ! Row 2, Col 3
  bitmap%buffer(3*8+2+1) = 120_int8  ! Row 3, Col 2
  bitmap%buffer(3*8+3+1) = 127_int8  ! Row 3, Col 3
  
  ! Save
  success = ft_bitmap_write_png(bitmap, "debug_aa_simple.png", error)
  if (success) print '("Saved: debug_aa_simple.png")'
  
  ! Print again
  print '("After manual pixel setting:")'
  do i = 0, 7
    write(*, '(8I4)', advance='no') (int(bitmap%buffer(i*8+j+1)), j=0,7)
    print '()'
  end do
  
  ! Cleanup
  call ft_raster_done(raster)
  call ft_bitmap_done(bitmap)
  call ft_outline_done(outline)
  
end program debug_aa_simple