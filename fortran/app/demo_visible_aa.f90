program demo_visible_aa
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
  type(FT_Bitmap), target :: mono_bitmap, aa_bitmap
  type(FT_Raster_State) :: raster
  type(FT_Raster_Params) :: params
  integer(FT_Error) :: error
  logical :: success
  integer :: i, j, pixel_val
  
  print '("Creating Visible Antialiasing Demo")'
  print '("==================================")'
  
  ! Create a large rectangle with off-grid positioning to show AA
  success = ft_outline_new(4, 1, outline, error)
  if (.not. success) stop 1
  
  ! Large rectangle with fractional positioning (guaranteed to show AA)
  outline%points(1) = FT_Vector(10 * 64 + 32, 10 * 64 + 32)  ! 10.5, 10.5
  outline%points(2) = FT_Vector(54 * 64 + 32, 10 * 64 + 32)  ! 54.5, 10.5
  outline%points(3) = FT_Vector(54 * 64 + 32, 54 * 64 + 32)  ! 54.5, 54.5
  outline%points(4) = FT_Vector(10 * 64 + 32, 54 * 64 + 32)  ! 10.5, 54.5
  
  outline%tags = FT_CURVE_TAG_ON
  outline%contours(1) = 3
  outline%n_points = 4
  outline%n_contours = 1
  
  ! Create bitmaps
  success = ft_bitmap_new(64, 64, FT_PIXEL_MODE_MONO, mono_bitmap, error)
  if (.not. success) stop 1
  
  success = ft_bitmap_new(64, 64, FT_PIXEL_MODE_GRAY, aa_bitmap, error)
  if (.not. success) stop 1
  
  success = ft_raster_new(raster, error)
  if (.not. success) stop 1
  
  raster%outline => outline
  
  ! Render monochrome
  params%target => mono_bitmap
  params%flags = 0
  success = ft_raster_render_outline(raster, params, error)
  if (.not. success) stop 1
  
  ! Render antialiased
  params%target => aa_bitmap
  params%flags = FT_RASTER_FLAG_AA
  success = ft_raster_render_outline(raster, params, error)
  if (.not. success) stop 1
  
  ! Enhance visibility by amplifying grayscale values
  do i = 1, aa_bitmap%width * aa_bitmap%rows
    if (aa_bitmap%buffer(i) > 0) then
      ! Amplify low values to make them visible
      pixel_val = int(aa_bitmap%buffer(i)) * 8  ! 8x amplification
      if (pixel_val > 255) pixel_val = 255
      aa_bitmap%buffer(i) = int(pixel_val, kind=int8)
    end if
  end do
  
  ! Count and report pixels
  print '("Monochrome pixels: ", I0)', count_pixels(mono_bitmap)
  print '("Antialiased pixels: ", I0)', count_pixels(aa_bitmap)
  
  ! Save images
  success = ft_bitmap_write_png(mono_bitmap, "demo_mono.png", error)
  if (success) print '("Saved: demo_mono.png")'
  
  success = ft_bitmap_write_png(aa_bitmap, "demo_aa_enhanced.png", error)
  if (success) print '("Saved: demo_aa_enhanced.png (8x amplified)")'
  
  ! Create a high-contrast version
  do i = 1, aa_bitmap%width * aa_bitmap%rows
    if (aa_bitmap%buffer(i) > 0) then
      aa_bitmap%buffer(i) = 100_int8  ! Medium gray for all AA pixels
    end if
  end do
  
  success = ft_bitmap_write_png(aa_bitmap, "demo_aa_highcontrast.png", error)
  if (success) print '("Saved: demo_aa_highcontrast.png (high contrast)")'
  
  ! Print some pixel values for debugging
  print '()'
  print '("Sample pixel values around edges:")'
  do i = 10, 15
    do j = 10, 15
      pixel_val = aa_bitmap%buffer(i * aa_bitmap%width + j)
      if (pixel_val > 0) then
        print '("  Pixel (", I0, ",", I0, ") = ", I0)', j, i, pixel_val
      end if
    end do
  end do
  
  ! Cleanup
  call ft_raster_done(raster)
  call ft_bitmap_done(mono_bitmap)
  call ft_bitmap_done(aa_bitmap)
  call ft_outline_done(outline)
  
  print '()'
  print '("Demo complete! Check the generated PNG files.")'
  
contains

  function count_pixels(bitmap) result(count)
    type(FT_Bitmap), intent(in) :: bitmap
    integer :: count
    integer :: i
    
    count = 0
    select case (bitmap%pixel_mode)
    case (FT_PIXEL_MODE_MONO)
      do i = 1, bitmap%pitch * bitmap%rows
        count = count + popcnt(bitmap%buffer(i))
      end do
    case (FT_PIXEL_MODE_GRAY)
      do i = 1, bitmap%width * bitmap%rows
        if (bitmap%buffer(i) > 0) count = count + 1
      end do
    end select
    
  end function count_pixels

end program demo_visible_aa