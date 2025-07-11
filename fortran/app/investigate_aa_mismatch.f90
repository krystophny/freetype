program investigate_aa_mismatch
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
  integer :: i, j, mono_pixels, aa_pixels, pixel_count
  
  print '("Investigating AA vs Mono Mismatch")'
  print '("=================================")'
  
  ! Create simple, predictable test case
  success = ft_outline_new(4, 1, outline, error)
  if (.not. success) stop 1
  
  ! 20x20 rectangle centered in 32x32 bitmap
  outline%points(1) = FT_Vector(6 * 64, 6 * 64)    ! 6, 6
  outline%points(2) = FT_Vector(26 * 64, 6 * 64)   ! 26, 6
  outline%points(3) = FT_Vector(26 * 64, 26 * 64)  ! 26, 26
  outline%points(4) = FT_Vector(6 * 64, 26 * 64)   ! 6, 26
  
  outline%tags = FT_CURVE_TAG_ON
  outline%contours(1) = 3
  outline%n_points = 4
  outline%n_contours = 1
  
  ! Create 32x32 bitmaps
  success = ft_bitmap_new(32, 32, FT_PIXEL_MODE_MONO, mono_bitmap, error)
  if (.not. success) stop 1
  
  success = ft_bitmap_new(32, 32, FT_PIXEL_MODE_GRAY, aa_bitmap, error)
  if (.not. success) stop 1
  
  success = ft_raster_new(raster, error)
  if (.not. success) stop 1
  
  raster%outline => outline
  
  print '("Outline points:")'
  do i = 1, outline%n_points
    print '("  Point ", I0, ": (", I0, ",", I0, ") = (", F6.2, ",", F6.2, ")")', &
          i, outline%points(i)%x, outline%points(i)%y, &
          real(outline%points(i)%x)/64.0, real(outline%points(i)%y)/64.0
  end do
  
  ! Render monochrome
  print '()'
  print '("Rendering monochrome...")'
  params%target => mono_bitmap
  params%flags = 0
  success = ft_raster_render_outline(raster, params, error)
  if (.not. success) then
    print '("ERROR: Mono render failed")'
    stop 1
  end if
  
  ! Count mono pixels
  mono_pixels = 0
  do i = 1, mono_bitmap%pitch * mono_bitmap%rows
    mono_pixels = mono_pixels + popcnt(mono_bitmap%buffer(i))
  end do
  print '("Mono pixels set: ", I0)', mono_pixels
  
  ! Render antialiased
  print '()'
  print '("Rendering antialiased...")'
  params%target => aa_bitmap
  params%flags = FT_RASTER_FLAG_AA
  success = ft_raster_render_outline(raster, params, error)
  if (.not. success) then
    print '("ERROR: AA render failed")'
    stop 1
  end if
  
  ! Count AA pixels
  aa_pixels = 0
  do i = 1, aa_bitmap%width * aa_bitmap%rows
    if (aa_bitmap%buffer(i) > 0) aa_pixels = aa_pixels + 1
  end do
  print '("AA pixels set: ", I0)', aa_pixels
  
  ! Print first few AA pixel values
  print '("First 10 AA pixel values:")'
  pixel_count = 0
  do i = 1, aa_bitmap%width * aa_bitmap%rows
    if (aa_bitmap%buffer(i) > 0) then
      pixel_count = pixel_count + 1
      print '("  Pixel ", I0, " = ", I0)', i, int(aa_bitmap%buffer(i))
      if (pixel_count >= 10) exit
    end if
  end do
  
  ! Show bitmap patterns
  print '()'
  print '("Mono bitmap pattern (first 8 rows):")'
  do i = 1, 8
    write(*, '("Row ", I2, ": ")', advance='no') i
    do j = 1, min(32, mono_bitmap%width)
      if (is_mono_pixel_set(mono_bitmap, (i-1)*32 + j)) then
        write(*, '("█")', advance='no')
      else
        write(*, '("·")', advance='no')
      end if
    end do
    print '()'
  end do
  
  print '()'
  print '("AA bitmap pattern (first 8 rows, enhanced):")'
  do i = 1, 8
    write(*, '("Row ", I2, ": ")', advance='no') i
    do j = 1, min(32, aa_bitmap%width)
      if (aa_bitmap%buffer((i-1)*32 + j) > 0) then
        write(*, '("▓")', advance='no')
      else
        write(*, '("·")', advance='no')
      end if
    end do
    print '()'
  end do
  
  ! Save simple test images
  success = ft_bitmap_write_png(mono_bitmap, "investigate_mono.png", error)
  if (success) print '("Saved: investigate_mono.png")'
  
  success = ft_bitmap_write_png(aa_bitmap, "investigate_aa_raw.png", error)
  if (success) print '("Saved: investigate_aa_raw.png")'
  
  ! Cleanup
  call ft_raster_done(raster)
  call ft_bitmap_done(mono_bitmap)
  call ft_bitmap_done(aa_bitmap)
  call ft_outline_done(outline)
  
contains

  function is_mono_pixel_set(bitmap, pixel_index) result(is_set)
    type(FT_Bitmap), intent(in) :: bitmap
    integer, intent(in) :: pixel_index
    logical :: is_set
    integer :: y, x, byte_index, bit_index
    
    y = (pixel_index - 1) / bitmap%width
    x = mod(pixel_index - 1, bitmap%width)
    byte_index = y * bitmap%pitch + x / 8 + 1
    bit_index = 7 - mod(x, 8)
    
    if (byte_index <= size(bitmap%buffer)) then
      is_set = btest(bitmap%buffer(byte_index), bit_index)
    else
      is_set = .false.
    end if
  end function is_mono_pixel_set

end program investigate_aa_mismatch