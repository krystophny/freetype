program test_ft_compare
  use ft_types
  use ft_geometry
  use ft_bitmap_mod
  use ft_bitmap_io
  use ft_outline_mod
  use ft_scanline_simple
  use ft_compare
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Run all tests
  call test_side_by_side_comparison()
  call test_diff_image()
  call test_pixel_statistics()
  
  ! Print test summary
  print '(/, "FT_Compare Tests - Tests run: ", I0)', test_count
  print '("FT_Compare Tests - Tests failed: ", I0)', failed_count
  
  if (failed_count == 0) then
    print '(/, "All FT_Compare tests passed!")'
    stop 0
  else
    print '(/, "Some FT_Compare tests failed!")'
    stop 1
  end if

contains

  subroutine test_side_by_side_comparison()
    type(FT_Bitmap) :: bitmap1, bitmap2
    type(FT_Outline), target :: outline1, outline2
    integer(FT_Error) :: error
    logical :: success
    logical :: file_exists
    
    print '(/, "Testing side-by-side comparison...")'
    
    ! Create first bitmap with letter 'A'
    success = ft_outline_new(4, 1, outline1, error)
    if (.not. success) return
    
    outline1%points(1) = FT_Vector(512, 3072)
    outline1%points(2) = FT_Vector(1024, 512)
    outline1%points(3) = FT_Vector(1536, 512)
    outline1%points(4) = FT_Vector(2048, 3072)
    outline1%contours(1) = 3
    outline1%tags = FT_CURVE_TAG_ON
    
    success = ft_bitmap_new(24, 24, FT_PIXEL_MODE_MONO, bitmap1, error)
    if (.not. success) then
      call ft_outline_done(outline1)
      return
    end if
    
    success = ft_render_outline_filled(outline1, bitmap1, error)
    
    ! Create second bitmap with letter 'B' (slightly different)
    success = ft_outline_new(6, 1, outline2, error)
    if (.not. success) then
      call ft_bitmap_done(bitmap1)
      call ft_outline_done(outline1)
      return
    end if
    
    outline2%points(1) = FT_Vector(512, 3072)
    outline2%points(2) = FT_Vector(512, 512)
    outline2%points(3) = FT_Vector(1792, 512)
    outline2%points(4) = FT_Vector(1792, 1536)
    outline2%points(5) = FT_Vector(1792, 3072)
    outline2%points(6) = FT_Vector(512, 3072)
    outline2%contours(1) = 5
    outline2%tags = FT_CURVE_TAG_ON
    
    success = ft_bitmap_new(24, 24, FT_PIXEL_MODE_MONO, bitmap2, error)
    if (.not. success) then
      call ft_outline_done(outline1)
      call ft_outline_done(outline2)
      call ft_bitmap_done(bitmap1)
      return
    end if
    
    success = ft_render_outline_filled(outline2, bitmap2, error)
    
    ! Create side-by-side comparison
    test_count = test_count + 1
    success = ft_compare_create_side_by_side(bitmap1, bitmap2, "test_compare_side_by_side.png", error)
    
    if (success) then
      print '("PASS: Side-by-side comparison created")'
      
      inquire(file="test_compare_side_by_side.png", exist=file_exists)
      test_count = test_count + 1
      if (file_exists) then
        print '("PASS: Side-by-side PNG file exists")'
      else
        print '("FAIL: Side-by-side PNG file not created")'
        failed_count = failed_count + 1
      end if
    else
      print '("FAIL: Could not create side-by-side comparison")'
      failed_count = failed_count + 1
    end if
    
    ! Also test with PBM output
    test_count = test_count + 1
    success = ft_compare_create_side_by_side(bitmap1, bitmap2, "test_compare_side_by_side.pbm", error)
    if (success) then
      print '("PASS: Side-by-side PBM created")'
    else
      print '("FAIL: Could not create side-by-side PBM")'
      failed_count = failed_count + 1
    end if
    
    ! Cleanup
    call ft_outline_done(outline1)
    call ft_outline_done(outline2)
    call ft_bitmap_done(bitmap1)
    call ft_bitmap_done(bitmap2)
    
  end subroutine test_side_by_side_comparison
  
  subroutine test_diff_image()
    type(FT_Bitmap) :: bitmap1, bitmap2
    integer(FT_Error) :: error
    logical :: success
    logical :: file_exists
    integer :: x, y
    
    print '(/, "Testing difference image generation...")'
    
    ! Create two similar grayscale bitmaps
    success = ft_bitmap_new(16, 16, FT_PIXEL_MODE_GRAY, bitmap1, error)
    if (.not. success) return
    
    success = ft_bitmap_new(16, 16, FT_PIXEL_MODE_GRAY, bitmap2, error)
    if (.not. success) then
      call ft_bitmap_done(bitmap1)
      return
    end if
    
    ! Fill first with gradient
    do y = 0, 15
      do x = 0, 15
        if (associated(bitmap1%buffer)) then
          bitmap1%buffer(y * bitmap1%pitch + x + 1) = int((x * 255) / 15, int8)
        end if
      end do
    end do
    
    ! Fill second with slightly different gradient
    do y = 0, 15
      do x = 0, 15
        if (associated(bitmap2%buffer)) then
          ! Add some noise to create differences
          bitmap2%buffer(y * bitmap2%pitch + x + 1) = &
            int(min(255, (x * 255) / 15 + mod(x * y, 20)), int8)
        end if
      end do
    end do
    
    ! Create difference image
    test_count = test_count + 1
    success = ft_compare_create_diff_image(bitmap1, bitmap2, "test_compare_diff.png", error)
    
    if (success) then
      print '("PASS: Difference image created")'
      
      inquire(file="test_compare_diff.png", exist=file_exists)
      test_count = test_count + 1
      if (file_exists) then
        print '("PASS: Difference PNG file exists")'
      else
        print '("FAIL: Difference PNG file not created")'
        failed_count = failed_count + 1
      end if
    else
      print '("FAIL: Could not create difference image")'
      failed_count = failed_count + 1
    end if
    
    ! Cleanup
    call ft_bitmap_done(bitmap1)
    call ft_bitmap_done(bitmap2)
    
  end subroutine test_diff_image
  
  subroutine test_pixel_statistics()
    type(FT_Bitmap) :: bitmap1, bitmap2
    integer(FT_Error) :: error
    logical :: success
    integer :: total_pixels, different_pixels
    real :: diff_percentage
    
    print '(/, "Testing pixel difference statistics...")'
    
    ! Create two bitmaps
    success = ft_bitmap_new(10, 10, FT_PIXEL_MODE_MONO, bitmap1, error)
    if (.not. success) return
    
    success = ft_bitmap_new(10, 10, FT_PIXEL_MODE_MONO, bitmap2, error)
    if (.not. success) then
      call ft_bitmap_done(bitmap1)
      return
    end if
    
    ! Make them identical first
    call ft_bitmap_clear(bitmap1)
    call ft_bitmap_clear(bitmap2)
    
    ! Test identical bitmaps
    test_count = test_count + 1
    success = ft_compare_pixel_difference(bitmap1, bitmap2, total_pixels, different_pixels, error)
    
    if (success .and. different_pixels == 0) then
      print '("PASS: Identical bitmaps show 0 differences")'
    else
      print '("FAIL: Identical bitmaps show ", I0, " differences")', different_pixels
      failed_count = failed_count + 1
    end if
    
    ! Add some differences
    call ft_bitmap_set_pixel(bitmap1, 2, 2, .true.)
    call ft_bitmap_set_pixel(bitmap1, 5, 5, .true.)
    call ft_bitmap_set_pixel(bitmap2, 3, 3, .true.)
    call ft_bitmap_set_pixel(bitmap2, 7, 7, .true.)
    
    ! Test with differences
    test_count = test_count + 1
    success = ft_compare_pixel_difference(bitmap1, bitmap2, total_pixels, different_pixels, error)
    
    if (success .and. different_pixels == 4) then
      print '("PASS: Found correct number of different pixels: ", I0)', different_pixels
      diff_percentage = (real(different_pixels) / real(total_pixels)) * 100.0
      print '("      Difference: ", F5.1, "%")', diff_percentage
    else
      print '("FAIL: Expected 4 different pixels, found ", I0)', different_pixels
      failed_count = failed_count + 1
    end if
    
    ! Test different sized bitmaps
    call ft_bitmap_done(bitmap2)
    success = ft_bitmap_new(15, 15, FT_PIXEL_MODE_MONO, bitmap2, error)
    if (success) then
      test_count = test_count + 1
      success = ft_compare_pixel_difference(bitmap1, bitmap2, total_pixels, different_pixels, error)
      
      if (success .and. total_pixels == 225) then  ! 15x15
        print '("PASS: Correctly handled different sized bitmaps")'
        print '("      Total pixels compared: ", I0)', total_pixels
        print '("      Different pixels: ", I0)', different_pixels
      else
        print '("FAIL: Incorrect handling of different sized bitmaps")'
        failed_count = failed_count + 1
      end if
    end if
    
    ! Cleanup
    call ft_bitmap_done(bitmap1)
    call ft_bitmap_done(bitmap2)
    
  end subroutine test_pixel_statistics

end program test_ft_compare