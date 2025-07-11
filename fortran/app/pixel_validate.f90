program pixel_validate
  use ft_types
  use ft_geometry
  use ft_bitmap_mod
  use ft_bitmap_io
  use ft_outline_mod
  use ft_scanline_simple
  use ft_compare
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  
  type(FT_Bitmap) :: bitmap1, bitmap2
  type(FT_Outline), target :: outline
  integer(FT_Error) :: error
  logical :: success
  integer :: total_pixels, different_pixels
  real :: diff_percentage
  integer :: test_count, pass_count
  
  print '("FreeType Fortran - Pixel Validation Tests")'
  print '("=========================================")'
  print '("")'
  
  test_count = 0
  pass_count = 0
  
  ! Test 1: Render the same shape twice and compare
  print '("Test 1: Consistency Check")'
  print '("-------------------------")'
  test_count = test_count + 1
  
  ! Create a simple shape
  success = ft_outline_new(4, 1, outline, error)
  if (.not. success) then
    print '("ERROR: Failed to create outline")'
    stop 1
  end if
  
  ! Square in pixel coordinates (already scaled)
  outline%points(1) = FT_Vector(640, 640)    ! (10, 10) * 64
  outline%points(2) = FT_Vector(1280, 640)   ! (20, 10) * 64
  outline%points(3) = FT_Vector(1280, 1280)  ! (20, 20) * 64
  outline%points(4) = FT_Vector(640, 1280)   ! (10, 20) * 64
  outline%contours(1) = 3
  outline%n_points = 4
  outline%n_contours = 1
  outline%tags = FT_CURVE_TAG_ON
  
  ! Render twice
  success = ft_bitmap_new(32, 32, FT_PIXEL_MODE_MONO, bitmap1, error)
  if (.not. success) then
    call ft_outline_done(outline)
    print '("ERROR: Failed to create bitmap1")'
    stop 1
  end if
  
  success = ft_bitmap_new(32, 32, FT_PIXEL_MODE_MONO, bitmap2, error)
  if (.not. success) then
    call ft_bitmap_done(bitmap1)
    call ft_outline_done(outline)
    print '("ERROR: Failed to create bitmap2")'
    stop 1
  end if
  
  ! Render same outline twice
  success = ft_render_outline_filled(outline, bitmap1, error)
  if (.not. success) then
    print '("ERROR: Failed to render bitmap1")'
  end if
  
  success = ft_render_outline_filled(outline, bitmap2, error)
  if (.not. success) then
    print '("ERROR: Failed to render bitmap2")'
  end if
  
  ! Compare - should be identical
  success = ft_compare_pixel_difference(bitmap1, bitmap2, total_pixels, different_pixels, error)
  if (success) then
    diff_percentage = (real(different_pixels) / real(total_pixels)) * 100.0
    print '("  Total pixels: ", I0)', total_pixels
    print '("  Different pixels: ", I0)', different_pixels
    print '("  Difference: ", F5.1, "%")', diff_percentage
    
    if (different_pixels == 0) then
      print '("  Result: PASS - Consistent rendering")'
      pass_count = pass_count + 1
    else
      print '("  Result: FAIL - Inconsistent rendering!")'
    end if
  end if
  
  ! Save for inspection
  success = ft_bitmap_write_pbm(bitmap1, "validate_square.pbm", error)
  
  call ft_bitmap_done(bitmap1)
  call ft_bitmap_done(bitmap2)
  call ft_outline_done(outline)
  
  ! Test 2: Mirror symmetry test
  print '("")'
  print '("Test 2: Mirror Symmetry")'
  print '("-----------------------")'
  test_count = test_count + 1
  
  ! Create symmetric shape
  success = ft_outline_new(6, 1, outline, error)
  if (success) then
    ! Hexagon centered at (16, 16)
    outline%points(1) = FT_Vector(1024, 512)   ! Top
    outline%points(2) = FT_Vector(1408, 768)   ! Top-right  
    outline%points(3) = FT_Vector(1408, 1280)  ! Bottom-right
    outline%points(4) = FT_Vector(1024, 1536)  ! Bottom
    outline%points(5) = FT_Vector(640, 1280)   ! Bottom-left
    outline%points(6) = FT_Vector(640, 768)    ! Top-left
    outline%contours(1) = 5
    outline%n_points = 6
    outline%n_contours = 1
    outline%tags = FT_CURVE_TAG_ON
    
    success = ft_bitmap_new(32, 32, FT_PIXEL_MODE_MONO, bitmap1, error)
    if (success) then
      success = ft_render_outline_filled(outline, bitmap1, error)
      if (success) then
        ! Check vertical symmetry
        if (check_vertical_symmetry(bitmap1)) then
          print '("  Result: PASS - Shape is vertically symmetric")'
          pass_count = pass_count + 1
        else
          print '("  Result: FAIL - Shape is not vertically symmetric")'
        end if
        
        success = ft_bitmap_write_pbm(bitmap1, "validate_hexagon.pbm", error)
      end if
      call ft_bitmap_done(bitmap1)
    end if
    call ft_outline_done(outline)
  end if
  
  ! Test 3: Grayscale antialiasing consistency
  print '("")'
  print '("Test 3: Grayscale Rendering")'
  print '("---------------------------")'
  test_count = test_count + 1
  
  success = ft_outline_new(3, 1, outline, error)
  if (success) then
    ! Triangle
    outline%points(1) = FT_Vector(1024, 640)   ! Top
    outline%points(2) = FT_Vector(1408, 1408)  ! Bottom-right
    outline%points(3) = FT_Vector(640, 1408)   ! Bottom-left
    outline%contours(1) = 2
    outline%n_points = 3
    outline%n_contours = 1
    outline%tags = FT_CURVE_TAG_ON
    
    success = ft_bitmap_new(32, 32, FT_PIXEL_MODE_GRAY, bitmap1, error)
    if (success) then
      success = ft_render_outline_filled(outline, bitmap1, error)
      if (success) then
        ! Check that we have grayscale values (not just black/white)
        if (has_grayscale_values(bitmap1)) then
          print '("  Result: PASS - Grayscale antialiasing present")'
          pass_count = pass_count + 1
        else
          print '("  Result: FAIL - No antialiasing detected")'
        end if
        
        success = ft_bitmap_write_pgm(bitmap1, "validate_triangle_gray.pgm", error)
        success = ft_bitmap_write_png(bitmap1, "validate_triangle_gray.png", error)
      end if
      call ft_bitmap_done(bitmap1)
    end if
    call ft_outline_done(outline)
  end if
  
  ! Summary
  print '("")'
  print '("===== VALIDATION SUMMARY =====")'
  print '("Tests run: ", I0)', test_count
  print '("Tests passed: ", I0)', pass_count
  if (pass_count == test_count) then
    print '("All validation tests PASSED!")'
  else
    print '("Some validation tests FAILED!")'
  end if
  
contains

  function check_vertical_symmetry(bitmap) result(is_symmetric)
    type(FT_Bitmap), intent(in) :: bitmap
    logical :: is_symmetric
    integer :: x, y, mid_x
    logical :: left_pixel, right_pixel
    
    is_symmetric = .true.
    mid_x = bitmap%width / 2
    
    do y = 0, bitmap%rows - 1
      do x = 0, mid_x - 1
        left_pixel = ft_bitmap_get_pixel(bitmap, x, y)
        right_pixel = ft_bitmap_get_pixel(bitmap, bitmap%width - 1 - x, y)
        if (left_pixel .neqv. right_pixel) then
          is_symmetric = .false.
          return
        end if
      end do
    end do
    
  end function check_vertical_symmetry
  
  function has_grayscale_values(bitmap) result(has_gray)
    type(FT_Bitmap), intent(in) :: bitmap
    logical :: has_gray
    integer :: x, y, byte_offset
    integer :: pixel_value
    integer :: black_count, white_count, gray_count
    
    has_gray = .false.
    black_count = 0
    white_count = 0 
    gray_count = 0
    
    do y = 0, bitmap%rows - 1
      do x = 0, bitmap%width - 1
        byte_offset = y * abs(bitmap%pitch) + x + 1
        if (byte_offset > 0 .and. byte_offset <= size(bitmap%buffer)) then
          pixel_value = iand(int(bitmap%buffer(byte_offset)), 255)
          if (pixel_value == 0) then
            black_count = black_count + 1
          else if (pixel_value == 255) then
            white_count = white_count + 1
          else
            gray_count = gray_count + 1
          end if
        end if
      end do
    end do
    
    ! We expect some gray values for antialiasing
    has_gray = (gray_count > 0)
    
    print '("    Black pixels: ", I0)', black_count
    print '("    White pixels: ", I0)', white_count  
    print '("    Gray pixels: ", I0)', gray_count
    
  end function has_grayscale_values

end program pixel_validate