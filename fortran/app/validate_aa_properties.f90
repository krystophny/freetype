program validate_aa_properties
  use ft_types
  use ft_bitmap_mod
  use ft_outline_mod
  use ft_raster
  use ft_raster_types
  use ft_geometry, only: FT_Vector
  use ft_bitmap_io
  implicit none
  
  integer :: total_tests = 0
  integer :: passed_tests = 0
  
  print '("FreeType Antialiasing Property Validation")'
  print '("========================================")'
  print '()'
  
  ! Test 1: Antialiasing produces grayscale values
  call test_grayscale_output(passed_tests, total_tests)
  
  ! Test 2: Antialiasing is different from monochrome
  call test_aa_vs_mono_difference(passed_tests, total_tests)
  
  ! Test 3: Edge coverage consistency
  call test_edge_coverage_consistency(passed_tests, total_tests)
  
  ! Test 4: Subpixel positioning creates antialiasing
  call test_subpixel_antialiasing(passed_tests, total_tests)
  
  ! Test 5: Performance validation
  call test_performance_reasonable(passed_tests, total_tests)
  
  ! Summary
  print '()'
  print '("===== VALIDATION SUMMARY =====")'
  print '("Total tests: ", I0)', total_tests
  print '("Passed tests: ", I0)', passed_tests
  print '("Failed tests: ", I0)', total_tests - passed_tests
  print '("Success rate: ", F5.1, "%")', real(passed_tests) / real(total_tests) * 100.0
  
  if (passed_tests == total_tests) then
    print '("✓ All antialiasing property tests PASSED!")'
  else
    print '("✗ Some antialiasing property tests FAILED!")'
  end if
  
contains

  ! Test that antialiasing produces grayscale values (not just black/white)
  subroutine test_grayscale_output(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_Outline), target :: outline
    type(FT_Bitmap), target :: bitmap
    type(FT_Raster_State) :: raster
    type(FT_Raster_Params) :: params
    integer(FT_Error) :: error
    logical :: success
    integer :: i, gray_count, black_count, white_count
    
    total = total + 1
    print '("Test ", I0, ": Grayscale output validation")', total
    
    ! Create off-grid rectangle to ensure antialiasing
    success = ft_outline_new(4, 1, outline, error)
    if (.not. success) goto 999
    
    outline%points(1) = FT_Vector(20 * 64 + 32, 20 * 64 + 16)  ! 20.5, 20.25
    outline%points(2) = FT_Vector(44 * 64 + 16, 20 * 64 + 48)  ! 44.25, 20.75
    outline%points(3) = FT_Vector(44 * 64 + 48, 44 * 64 + 32)  ! 44.75, 44.5
    outline%points(4) = FT_Vector(20 * 64 + 32, 44 * 64 + 16)  ! 20.5, 44.25
    
    outline%tags = FT_CURVE_TAG_ON
    outline%contours(1) = 3
    outline%n_points = 4
    outline%n_contours = 1
    
    success = ft_bitmap_new(64, 64, FT_PIXEL_MODE_GRAY, bitmap, error)
    if (.not. success) goto 999
    
    success = ft_raster_new(raster, error)
    if (.not. success) goto 999
    
    raster%outline => outline
    params%target => bitmap
    params%flags = FT_RASTER_FLAG_AA
    
    success = ft_raster_render_outline(raster, params, error)
    if (.not. success) goto 999
    
    ! Count pixel types
    gray_count = 0
    black_count = 0
    white_count = 0
    
    do i = 1, bitmap%width * bitmap%rows
      if (bitmap%buffer(i) == 0) then
        white_count = white_count + 1
      else if (bitmap%buffer(i) == 255) then
        black_count = black_count + 1
      else
        gray_count = gray_count + 1
      end if
    end do
    
    print '("  White pixels: ", I0)', white_count
    print '("  Gray pixels: ", I0)', gray_count
    print '("  Black pixels: ", I0)', black_count
    
    ! Test passes if we have grayscale pixels
    if (gray_count > 0) then
      print '("  Result: PASS - Grayscale antialiasing detected")'
      passed = passed + 1
      success = ft_bitmap_write_png(bitmap, "aa_grayscale_test.png", error)
    else
      print '("  Result: FAIL - No grayscale pixels (monochrome output)")'
    end if
    
    call ft_raster_done(raster)
    call ft_bitmap_done(bitmap)
    call ft_outline_done(outline)
    return
    
999 continue
    print '("  Result: FAIL - Test setup failed")'
    
  end subroutine test_grayscale_output

  ! Test that antialiasing differs from monochrome
  subroutine test_aa_vs_mono_difference(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_Outline), target :: outline
    type(FT_Bitmap), target :: aa_bitmap, mono_bitmap
    type(FT_Raster_State) :: raster
    type(FT_Raster_Params) :: params
    integer(FT_Error) :: error
    logical :: success
    integer :: i, diff_count
    
    total = total + 1
    print '("Test ", I0, ": AA vs Monochrome difference")', total
    
    ! Create off-grid rectangle
    success = ft_outline_new(4, 1, outline, error)
    if (.not. success) goto 999
    
    outline%points(1) = FT_Vector(20 * 64 + 32, 20 * 64 + 16)
    outline%points(2) = FT_Vector(44 * 64 + 16, 20 * 64 + 48)
    outline%points(3) = FT_Vector(44 * 64 + 48, 44 * 64 + 32)
    outline%points(4) = FT_Vector(20 * 64 + 32, 44 * 64 + 16)
    
    outline%tags = FT_CURVE_TAG_ON
    outline%contours(1) = 3
    outline%n_points = 4
    outline%n_contours = 1
    
    ! Create bitmaps
    success = ft_bitmap_new(64, 64, FT_PIXEL_MODE_GRAY, aa_bitmap, error)
    if (.not. success) goto 999
    
    success = ft_bitmap_new(64, 64, FT_PIXEL_MODE_MONO, mono_bitmap, error)
    if (.not. success) goto 999
    
    success = ft_raster_new(raster, error)
    if (.not. success) goto 999
    
    raster%outline => outline
    
    ! Render with antialiasing
    params%target => aa_bitmap
    params%flags = FT_RASTER_FLAG_AA
    success = ft_raster_render_outline(raster, params, error)
    if (.not. success) goto 999
    
    ! Render monochrome
    params%target => mono_bitmap
    params%flags = 0
    success = ft_raster_render_outline(raster, params, error)
    if (.not. success) goto 999
    
    ! Count differences (comparing normalized values)
    diff_count = 0
    do i = 1, aa_bitmap%width * aa_bitmap%rows
      ! Convert mono to grayscale for comparison
      ! Check if pixel is set in mono bitmap (bit-packed format)
      if ((aa_bitmap%buffer(i) > 0) .neqv. is_mono_pixel_set(mono_bitmap, i)) then
        diff_count = diff_count + 1
      end if
    end do
    
    print '("  Different pixels: ", I0, " / ", I0)', diff_count, aa_bitmap%width * aa_bitmap%rows
    print '("  Difference: ", F5.1, "%")', real(diff_count) / real(aa_bitmap%width * aa_bitmap%rows) * 100.0
    
    ! Test passes if there are some differences (antialiasing should affect edges)
    if (diff_count > 0) then
      print '("  Result: PASS - AA produces different output than monochrome")'
      passed = passed + 1
    else
      print '("  Result: FAIL - AA identical to monochrome")'
    end if
    
    call ft_raster_done(raster)
    call ft_bitmap_done(aa_bitmap)
    call ft_bitmap_done(mono_bitmap)
    call ft_outline_done(outline)
    return
    
999 continue
    print '("  Result: FAIL - Test setup failed")'
    
  end subroutine test_aa_vs_mono_difference

  ! Test edge coverage consistency
  subroutine test_edge_coverage_consistency(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_Outline), target :: outline
    type(FT_Bitmap), target :: bitmap
    type(FT_Raster_State) :: raster
    type(FT_Raster_Params) :: params
    integer(FT_Error) :: error
    logical :: success
    integer :: i, edge_pixels, interior_pixels
    
    total = total + 1
    print '("Test ", I0, ": Edge coverage consistency")', total
    
    ! Create large rectangle to have clear interior and edges
    success = ft_outline_new(4, 1, outline, error)
    if (.not. success) goto 999
    
    outline%points(1) = FT_Vector(10 * 64, 10 * 64)
    outline%points(2) = FT_Vector(54 * 64, 10 * 64)
    outline%points(3) = FT_Vector(54 * 64, 54 * 64)
    outline%points(4) = FT_Vector(10 * 64, 54 * 64)
    
    outline%tags = FT_CURVE_TAG_ON
    outline%contours(1) = 3
    outline%n_points = 4
    outline%n_contours = 1
    
    success = ft_bitmap_new(64, 64, FT_PIXEL_MODE_GRAY, bitmap, error)
    if (.not. success) goto 999
    
    success = ft_raster_new(raster, error)
    if (.not. success) goto 999
    
    raster%outline => outline
    params%target => bitmap
    params%flags = FT_RASTER_FLAG_AA
    
    success = ft_raster_render_outline(raster, params, error)
    if (.not. success) goto 999
    
    ! Count edge vs interior pixels
    edge_pixels = 0
    interior_pixels = 0
    
    do i = 1, bitmap%width * bitmap%rows
      if (bitmap%buffer(i) > 0 .and. bitmap%buffer(i) < 255) then
        edge_pixels = edge_pixels + 1
      else if (bitmap%buffer(i) == 255) then
        interior_pixels = interior_pixels + 1
      end if
    end do
    
    print '("  Edge pixels (gray): ", I0)', edge_pixels
    print '("  Interior pixels (black): ", I0)', interior_pixels
    
    ! Test passes if we have both edge and interior pixels
    if (edge_pixels > 0 .and. interior_pixels > 0) then
      print '("  Result: PASS - Consistent edge/interior coverage")'
      passed = passed + 1
    else
      print '("  Result: FAIL - Missing edge or interior pixels")'
    end if
    
    call ft_raster_done(raster)
    call ft_bitmap_done(bitmap)
    call ft_outline_done(outline)
    return
    
999 continue
    print '("  Result: FAIL - Test setup failed")'
    
  end subroutine test_edge_coverage_consistency

  ! Test subpixel positioning creates antialiasing
  subroutine test_subpixel_antialiasing(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_Outline), target :: outline1, outline2
    type(FT_Bitmap), target :: bitmap1, bitmap2
    type(FT_Raster_State) :: raster
    type(FT_Raster_Params) :: params
    integer(FT_Error) :: error
    logical :: success
    integer :: i, diff_count
    
    total = total + 1
    print '("Test ", I0, ": Subpixel positioning effect")', total
    
    ! Create two similar rectangles with different subpixel positions
    success = ft_outline_new(4, 1, outline1, error)
    if (.not. success) goto 999
    
    success = ft_outline_new(4, 1, outline2, error)
    if (.not. success) goto 999
    
    ! Rectangle 1: aligned to pixel grid
    outline1%points(1) = FT_Vector(20 * 64, 20 * 64)
    outline1%points(2) = FT_Vector(44 * 64, 20 * 64)
    outline1%points(3) = FT_Vector(44 * 64, 44 * 64)
    outline1%points(4) = FT_Vector(20 * 64, 44 * 64)
    
    outline1%tags = FT_CURVE_TAG_ON
    outline1%contours(1) = 3
    outline1%n_points = 4
    outline1%n_contours = 1
    
    ! Rectangle 2: offset by 0.5 pixels
    outline2%points(1) = FT_Vector(20 * 64 + 32, 20 * 64 + 32)
    outline2%points(2) = FT_Vector(44 * 64 + 32, 20 * 64 + 32)
    outline2%points(3) = FT_Vector(44 * 64 + 32, 44 * 64 + 32)
    outline2%points(4) = FT_Vector(20 * 64 + 32, 44 * 64 + 32)
    
    outline2%tags = FT_CURVE_TAG_ON
    outline2%contours(1) = 3
    outline2%n_points = 4
    outline2%n_contours = 1
    
    ! Create bitmaps
    success = ft_bitmap_new(64, 64, FT_PIXEL_MODE_GRAY, bitmap1, error)
    if (.not. success) goto 999
    
    success = ft_bitmap_new(64, 64, FT_PIXEL_MODE_GRAY, bitmap2, error)
    if (.not. success) goto 999
    
    success = ft_raster_new(raster, error)
    if (.not. success) goto 999
    
    params%flags = FT_RASTER_FLAG_AA
    
    ! Render both rectangles
    raster%outline => outline1
    params%target => bitmap1
    success = ft_raster_render_outline(raster, params, error)
    if (.not. success) goto 999
    
    raster%outline => outline2
    params%target => bitmap2
    success = ft_raster_render_outline(raster, params, error)
    if (.not. success) goto 999
    
    ! Count differences
    diff_count = 0
    do i = 1, bitmap1%width * bitmap1%rows
      if (bitmap1%buffer(i) /= bitmap2%buffer(i)) then
        diff_count = diff_count + 1
      end if
    end do
    
    print '("  Different pixels: ", I0, " / ", I0)', diff_count, bitmap1%width * bitmap1%rows
    print '("  Difference: ", F5.1, "%")', real(diff_count) / real(bitmap1%width * bitmap1%rows) * 100.0
    
    ! Test passes if subpixel positioning creates differences
    if (diff_count > 0) then
      print '("  Result: PASS - Subpixel positioning affects antialiasing")'
      passed = passed + 1
    else
      print '("  Result: FAIL - Subpixel positioning has no effect")'
    end if
    
    call ft_raster_done(raster)
    call ft_bitmap_done(bitmap1)
    call ft_bitmap_done(bitmap2)
    call ft_outline_done(outline1)
    call ft_outline_done(outline2)
    return
    
999 continue
    print '("  Result: FAIL - Test setup failed")'
    
  end subroutine test_subpixel_antialiasing

  ! Test that performance is reasonable
  subroutine test_performance_reasonable(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_Outline), target :: outline
    type(FT_Bitmap), target :: bitmap
    type(FT_Raster_State) :: raster
    type(FT_Raster_Params) :: params
    integer(FT_Error) :: error
    logical :: success
    integer :: i, iterations = 100
    integer(8) :: start_time, end_time, freq
    real :: elapsed_time, shapes_per_sec
    
    total = total + 1
    print '("Test ", I0, ": Performance validation")', total
    
    ! Create test outline
    success = ft_outline_new(4, 1, outline, error)
    if (.not. success) goto 999
    
    outline%points(1) = FT_Vector(16 * 64, 16 * 64)
    outline%points(2) = FT_Vector(48 * 64, 16 * 64)
    outline%points(3) = FT_Vector(48 * 64, 48 * 64)
    outline%points(4) = FT_Vector(16 * 64, 48 * 64)
    
    outline%tags = FT_CURVE_TAG_ON
    outline%contours(1) = 3
    outline%n_points = 4
    outline%n_contours = 1
    
    success = ft_bitmap_new(64, 64, FT_PIXEL_MODE_GRAY, bitmap, error)
    if (.not. success) goto 999
    
    success = ft_raster_new(raster, error)
    if (.not. success) goto 999
    
    raster%outline => outline
    params%target => bitmap
    params%flags = FT_RASTER_FLAG_AA
    
    ! Time the rendering
    call system_clock(start_time, freq)
    
    do i = 1, iterations
      success = ft_raster_render_outline(raster, params, error)
      if (.not. success) goto 999
    end do
    
    call system_clock(end_time)
    
    elapsed_time = real(end_time - start_time) / real(freq)
    shapes_per_sec = real(iterations) / elapsed_time
    
    print '("  Rendered ", I0, " shapes in ", F6.3, " seconds")', iterations, elapsed_time
    print '("  Performance: ", F10.1, " shapes/second")', shapes_per_sec
    
    ! Test passes if performance is reasonable (>50K shapes/sec)
    if (shapes_per_sec > 50000.0) then
      print '("  Result: PASS - Performance is reasonable")'
      passed = passed + 1
    else
      print '("  Result: FAIL - Performance too low (<50K shapes/sec)")'
    end if
    
    call ft_raster_done(raster)
    call ft_bitmap_done(bitmap)
    call ft_outline_done(outline)
    return
    
999 continue
    print '("  Result: FAIL - Test setup failed")'
    
  end subroutine test_performance_reasonable

  ! Helper function to check if a pixel is set in a monochrome bitmap
  function is_mono_pixel_set(bitmap, pixel_index) result(is_set)
    type(FT_Bitmap), intent(in) :: bitmap
    integer, intent(in) :: pixel_index
    logical :: is_set
    
    integer :: y, x, byte_index, bit_index
    
    ! Convert linear index to x,y coordinates
    y = (pixel_index - 1) / bitmap%width
    x = mod(pixel_index - 1, bitmap%width)
    
    ! Calculate byte and bit position
    byte_index = y * bitmap%pitch + x / 8 + 1
    bit_index = 7 - mod(x, 8)
    
    ! Check if bit is set
    if (byte_index <= size(bitmap%buffer)) then
      is_set = btest(bitmap%buffer(byte_index), bit_index)
    else
      is_set = .false.
    end if
    
  end function is_mono_pixel_set

end program validate_aa_properties