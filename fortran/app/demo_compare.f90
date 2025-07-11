program demo_compare
  use ft_types
  use ft_geometry
  use ft_bitmap_mod
  use ft_bitmap_io
  use ft_outline_mod
  use ft_scanline_simple
  use ft_compare
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  
  integer(FT_Error) :: error
  logical :: success
  type(FT_Bitmap) :: bitmap1, bitmap2
  type(FT_Outline), target :: outline
  integer :: total_pixels, different_pixels
  real :: diff_percentage
  
  print '("FreeType Fortran Comparison Tool Demo")'
  print '("=====================================")'
  print '("")'
  
  ! Create first bitmap - letter 'F' 
  print '("Creating first bitmap with letter F...")'
  success = ft_outline_new(10, 1, outline, error)
  if (.not. success) then
    print '("ERROR: Failed to create outline")'
    stop 1
  end if
  
  ! Letter F outline
  outline%points(1) = FT_Vector(512, 3584)    ! Bottom left
  outline%points(2) = FT_Vector(512, 512)     ! Top left
  outline%points(3) = FT_Vector(2048, 512)    ! Top right
  outline%points(4) = FT_Vector(2048, 1024)   ! Top bar bottom
  outline%points(5) = FT_Vector(1024, 1024)   ! Back to stem
  outline%points(6) = FT_Vector(1024, 1792)   ! Middle bar top
  outline%points(7) = FT_Vector(1792, 1792)   ! Middle bar right
  outline%points(8) = FT_Vector(1792, 2304)   ! Middle bar bottom
  outline%points(9) = FT_Vector(1024, 2304)   ! Back to stem
  outline%points(10) = FT_Vector(1024, 3584)  ! Bottom
  
  outline%n_points = 10
  outline%n_contours = 1
  outline%contours(1) = 9  ! 10 points
  outline%tags = FT_CURVE_TAG_ON
  
  ! Create bitmap and render
  success = ft_bitmap_new(32, 32, FT_PIXEL_MODE_GRAY, bitmap1, error)
  if (.not. success) then
    call ft_outline_done(outline)
    print '("ERROR: Failed to create bitmap1")'
    stop 1
  end if
  
  success = ft_render_outline_filled(outline, bitmap1, error)
  if (success) then
    print '("First bitmap rendered successfully")'
    success = ft_bitmap_write_png(bitmap1, "demo_bitmap1.png", error)
    if (success) print '("  Saved as: demo_bitmap1.png")'
  end if
  
  ! Create second bitmap - letter 'F' with slight modification
  print '("")'
  print '("Creating second bitmap with modified F...")'
  
  ! Modify the outline slightly
  outline%points(7) = FT_Vector(1920, 1792)   ! Middle bar extends further
  
  success = ft_bitmap_new(32, 32, FT_PIXEL_MODE_GRAY, bitmap2, error)
  if (.not. success) then
    call ft_outline_done(outline)
    call ft_bitmap_done(bitmap1)
    print '("ERROR: Failed to create bitmap2")'
    stop 1
  end if
  
  success = ft_render_outline_filled(outline, bitmap2, error)
  if (success) then
    print '("Second bitmap rendered successfully")'
    success = ft_bitmap_write_png(bitmap2, "demo_bitmap2.png", error)
    if (success) print '("  Saved as: demo_bitmap2.png")'
  end if
  
  ! Create side-by-side comparison
  print '("")'
  print '("Creating visual comparisons...")'
  
  success = ft_compare_create_side_by_side(bitmap1, bitmap2, &
                                         "demo_side_by_side.png", error)
  if (success) then
    print '("  Side-by-side comparison: demo_side_by_side.png")'
  end if
  
  ! Create difference image
  success = ft_compare_create_diff_image(bitmap1, bitmap2, &
                                       "demo_difference.png", error)
  if (success) then
    print '("  Difference image: demo_difference.png")'
  end if
  
  ! Calculate pixel statistics
  success = ft_compare_pixel_difference(bitmap1, bitmap2, &
                                      total_pixels, different_pixels, error)
  if (success) then
    diff_percentage = (real(different_pixels) / real(total_pixels)) * 100.0
    print '("")'
    print '("Pixel comparison statistics:")'
    print '("  Total pixels: ", I0)', total_pixels
    print '("  Different pixels: ", I0)', different_pixels
    print '("  Difference: ", F5.1, "%")', diff_percentage
    
    if (different_pixels == 0) then
      print '("  Result: PIXEL-PERFECT MATCH")'
    else if (diff_percentage < 1.0) then
      print '("  Result: EXCELLENT (< 1% difference)")'
    else if (diff_percentage < 5.0) then
      print '("  Result: GOOD (< 5% difference)")'
    else
      print '("  Result: SIGNIFICANT DIFFERENCES")'
    end if
  end if
  
  ! Also create monochrome versions for comparison
  print '("")'
  print '("Creating monochrome comparison...")'
  
  call ft_bitmap_done(bitmap1)
  call ft_bitmap_done(bitmap2)
  
  success = ft_bitmap_new(32, 32, FT_PIXEL_MODE_MONO, bitmap1, error)
  if (success) then
    success = ft_render_outline_filled(outline, bitmap1, error)
    if (success) then
      success = ft_bitmap_write_pbm(bitmap1, "demo_mono1.pbm", error)
    end if
  end if
  
  ! Restore original outline
  outline%points(7) = FT_Vector(1792, 1792)
  
  success = ft_bitmap_new(32, 32, FT_PIXEL_MODE_MONO, bitmap2, error)
  if (success) then
    success = ft_render_outline_filled(outline, bitmap2, error)
    if (success) then
      success = ft_bitmap_write_pbm(bitmap2, "demo_mono2.pbm", error)
    end if
  end if
  
  if (success) then
    success = ft_compare_create_side_by_side(bitmap1, bitmap2, &
                                           "demo_mono_comparison.pbm", error)
    if (success) then
      print '("  Monochrome comparison: demo_mono_comparison.pbm")'
    end if
  end if
  
  ! Cleanup
  call ft_outline_done(outline)
  call ft_bitmap_done(bitmap1)
  call ft_bitmap_done(bitmap2)
  
  print '("")'
  print '("Demo complete. Check the output files to see the comparisons.")'

end program demo_compare