program test_ft_bitmap
  use ft_types
  use ft_bitmap_mod
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Run all tests
  call test_bitmap_creation()
  call test_bitmap_monochrome()
  call test_bitmap_grayscale()
  call test_bitmap_grayscale_pixels()
  call test_bitmap_copy()
  call test_bitmap_clear_fill()
  call test_bitmap_pixel_operations()
  call test_bitmap_conversion()
  
  ! Print test summary
  print '(/, "FT_Bitmap Tests - Tests run: ", I0)', test_count
  print '("FT_Bitmap Tests - Tests failed: ", I0)', failed_count
  
  if (failed_count == 0) then
    print '(/, "All FT_Bitmap tests passed!")'
    stop 0
  else
    print '(/, "Some FT_Bitmap tests failed!")'
    stop 1
  end if

contains

  subroutine test_bitmap_creation()
    type(FT_Bitmap) :: bitmap
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing bitmap creation...")'
    
    ! Test monochrome bitmap creation
    test_count = test_count + 1
    success = ft_bitmap_new(17, 10, FT_PIXEL_MODE_MONO, bitmap, error)
    
    if (success .and. error == FT_Err_Ok) then
      print '("PASS: Monochrome bitmap created")'
      
      ! Check dimensions
      test_count = test_count + 1
      if (bitmap%width == 17 .and. bitmap%rows == 10) then
        print '("PASS: Correct dimensions")'
      else
        print '("FAIL: Incorrect dimensions")'
        failed_count = failed_count + 1
      end if
      
      ! Check pitch (17 pixels = 3 bytes for monochrome)
      test_count = test_count + 1
      if (bitmap%pitch == 3) then
        print '("PASS: Correct pitch for monochrome")'
      else
        print '("FAIL: Incorrect pitch: ", I0, " (expected 3)")', bitmap%pitch
        failed_count = failed_count + 1
      end if
      
      ! Check buffer allocation
      test_count = test_count + 1
      if (associated(bitmap%buffer) .and. size(bitmap%buffer) == 30) then
        print '("PASS: Buffer allocated correctly")'
      else
        print '("FAIL: Buffer not allocated correctly")'
        failed_count = failed_count + 1
      end if
      
      call ft_bitmap_done(bitmap)
    else
      print '("FAIL: Could not create monochrome bitmap")'
      failed_count = failed_count + 1
    end if
    
  end subroutine test_bitmap_creation
  
  subroutine test_bitmap_monochrome()
    type(FT_Bitmap) :: bitmap
    integer(FT_Error) :: error
    logical :: success
    integer :: x, y
    
    print '(/, "Testing monochrome bitmap operations...")'
    
    ! Create 8x8 monochrome bitmap
    success = ft_bitmap_new(8, 8, FT_PIXEL_MODE_MONO, bitmap, error)
    if (.not. success) return
    
    ! Test setting pixels
    call ft_bitmap_set_pixel(bitmap, 0, 0, .true.)
    call ft_bitmap_set_pixel(bitmap, 7, 0, .true.)
    call ft_bitmap_set_pixel(bitmap, 0, 7, .true.)
    call ft_bitmap_set_pixel(bitmap, 7, 7, .true.)
    
    ! Test getting pixels
    test_count = test_count + 1
    if (ft_bitmap_get_pixel(bitmap, 0, 0) .and. &
        ft_bitmap_get_pixel(bitmap, 7, 0) .and. &
        ft_bitmap_get_pixel(bitmap, 0, 7) .and. &
        ft_bitmap_get_pixel(bitmap, 7, 7)) then
      print '("PASS: Monochrome pixels set/get correctly")'
    else
      print '("FAIL: Monochrome pixel operations failed")'
      failed_count = failed_count + 1
    end if
    
    ! Test unset pixels
    test_count = test_count + 1
    if (.not. ft_bitmap_get_pixel(bitmap, 3, 3) .and. &
        .not. ft_bitmap_get_pixel(bitmap, 4, 4)) then
      print '("PASS: Unset pixels return false")'
    else
      print '("FAIL: Unset pixels should return false")'
      failed_count = failed_count + 1
    end if
    
    ! Test clearing a pixel
    call ft_bitmap_set_pixel(bitmap, 0, 0, .false.)
    test_count = test_count + 1
    if (.not. ft_bitmap_get_pixel(bitmap, 0, 0)) then
      print '("PASS: Pixel cleared correctly")'
    else
      print '("FAIL: Pixel clear failed")'
      failed_count = failed_count + 1
    end if
    
    call ft_bitmap_done(bitmap)
    
  end subroutine test_bitmap_monochrome
  
  subroutine test_bitmap_grayscale()
    type(FT_Bitmap) :: bitmap
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing grayscale bitmap...")'
    
    ! Create 10x10 grayscale bitmap
    success = ft_bitmap_new(10, 10, FT_PIXEL_MODE_GRAY, bitmap, error)
    if (.not. success) return
    
    test_count = test_count + 1
    if (bitmap%pitch == 10 .and. bitmap%num_grays == 256) then
      print '("PASS: Grayscale bitmap parameters correct")'
    else
      print '("FAIL: Incorrect grayscale parameters")'
      failed_count = failed_count + 1
    end if
    
    ! Test pixel operations
    call ft_bitmap_set_pixel(bitmap, 5, 5, .true.)
    
    test_count = test_count + 1
    if (ft_bitmap_get_pixel(bitmap, 5, 5)) then
      print '("PASS: Grayscale pixel set")'
    else
      print '("FAIL: Grayscale pixel not set")'
      failed_count = failed_count + 1
    end if
    
    call ft_bitmap_done(bitmap)
    
  end subroutine test_bitmap_grayscale
  
  subroutine test_bitmap_grayscale_pixels()
    type(FT_Bitmap) :: bitmap
    integer(FT_Error) :: error
    logical :: success
    integer :: x, y
    integer(int8) :: expected_value
    
    print '(/, "Testing grayscale pixel values...")'
    
    ! Create grayscale bitmap
    success = ft_bitmap_new(10, 10, FT_PIXEL_MODE_GRAY, bitmap, error)
    if (.not. success) return
    
    ! Test setting various gray levels
    call ft_bitmap_set_pixel_gray(bitmap, 0, 0, 0)      ! Black
    call ft_bitmap_set_pixel_gray(bitmap, 1, 0, 64)     ! Dark gray
    call ft_bitmap_set_pixel_gray(bitmap, 2, 0, 128)    ! Medium gray
    call ft_bitmap_set_pixel_gray(bitmap, 3, 0, 192)    ! Light gray
    call ft_bitmap_set_pixel_gray(bitmap, 4, 0, 255)    ! White
    
    ! Test clamping
    call ft_bitmap_set_pixel_gray(bitmap, 5, 0, -50)    ! Should clamp to 0
    call ft_bitmap_set_pixel_gray(bitmap, 6, 0, 300)    ! Should clamp to 255
    
    ! Verify values
    test_count = test_count + 1
    if (bitmap%buffer(1) == 0_int8 .and. &
        bitmap%buffer(2) == 64_int8 .and. &
        bitmap%buffer(3) == int(-128, int8) .and. &  ! 128 as signed int8
        bitmap%buffer(4) == int(-64, int8) .and. &   ! 192 as signed int8
        bitmap%buffer(5) == -1_int8 .and. &    ! 255 as signed int8
        bitmap%buffer(6) == 0_int8 .and. &     ! Clamped to 0
        bitmap%buffer(7) == -1_int8) then      ! Clamped to 255
      print '("PASS: Grayscale pixel values set correctly")'
    else
      print '("FAIL: Grayscale pixel values incorrect")'
      print '("  Expected: 0, 64, 128, 192, 255, 0, 255")'
      print '("  Got:      ", I0, ", ", I0, ", ", I0, ", ", I0, ", ", I0, ", ", I0, ", ", I0)', &
        bitmap%buffer(1), bitmap%buffer(2), bitmap%buffer(3), bitmap%buffer(4), &
        bitmap%buffer(5), bitmap%buffer(6), bitmap%buffer(7)
      failed_count = failed_count + 1
    end if
    
    ! Test out-of-bounds handling
    call ft_bitmap_set_pixel_gray(bitmap, -1, 0, 128)   ! Out of bounds
    call ft_bitmap_set_pixel_gray(bitmap, 10, 0, 128)   ! Out of bounds
    call ft_bitmap_set_pixel_gray(bitmap, 0, -1, 128)   ! Out of bounds
    call ft_bitmap_set_pixel_gray(bitmap, 0, 10, 128)   ! Out of bounds
    
    test_count = test_count + 1
    print '("PASS: Out-of-bounds grayscale pixels handled safely")'
    
    call ft_bitmap_done(bitmap)
    
  end subroutine test_bitmap_grayscale_pixels
  
  subroutine test_bitmap_copy()
    type(FT_Bitmap) :: source, target
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing bitmap copy...")'
    
    ! Create source bitmap
    success = ft_bitmap_new(16, 8, FT_PIXEL_MODE_MONO, source, error)
    if (.not. success) return
    
    ! Set some pixels
    call ft_bitmap_set_pixel(source, 0, 0, .true.)
    call ft_bitmap_set_pixel(source, 15, 7, .true.)
    
    ! Copy bitmap
    test_count = test_count + 1
    success = ft_bitmap_copy(source, target, error)
    
    if (success) then
      print '("PASS: Bitmap copied")'
      
      ! Check dimensions
      test_count = test_count + 1
      if (target%width == source%width .and. target%rows == source%rows .and. &
          target%pixel_mode == source%pixel_mode) then
        print '("PASS: Copied bitmap has correct parameters")'
      else
        print '("FAIL: Copied bitmap parameters incorrect")'
        failed_count = failed_count + 1
      end if
      
      ! Check pixel data
      test_count = test_count + 1
      if (ft_bitmap_get_pixel(target, 0, 0) .and. &
          ft_bitmap_get_pixel(target, 15, 7)) then
        print '("PASS: Pixel data copied correctly")'
      else
        print '("FAIL: Pixel data not copied correctly")'
        failed_count = failed_count + 1
      end if
      
      call ft_bitmap_done(target)
    else
      print '("FAIL: Bitmap copy failed")'
      failed_count = failed_count + 1
    end if
    
    call ft_bitmap_done(source)
    
  end subroutine test_bitmap_copy
  
  subroutine test_bitmap_clear_fill()
    type(FT_Bitmap) :: bitmap
    integer(FT_Error) :: error
    logical :: success
    integer :: i
    
    print '(/, "Testing bitmap clear and fill...")'
    
    ! Create bitmap
    success = ft_bitmap_new(8, 8, FT_PIXEL_MODE_MONO, bitmap, error)
    if (.not. success) return
    
    ! Fill with 1s
    call ft_bitmap_fill(bitmap, -1_int8)
    
    test_count = test_count + 1
    if (all(bitmap%buffer == -1_int8)) then
      print '("PASS: Bitmap filled correctly")'
    else
      print '("FAIL: Bitmap fill failed")'
      failed_count = failed_count + 1
    end if
    
    ! Clear bitmap
    call ft_bitmap_clear(bitmap)
    
    test_count = test_count + 1
    if (all(bitmap%buffer == 0_int8)) then
      print '("PASS: Bitmap cleared correctly")'
    else
      print '("FAIL: Bitmap clear failed")'
      failed_count = failed_count + 1
    end if
    
    call ft_bitmap_done(bitmap)
    
  end subroutine test_bitmap_clear_fill
  
  subroutine test_bitmap_pixel_operations()
    type(FT_Bitmap) :: bitmap
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing pixel boundary conditions...")'
    
    ! Create small bitmap
    success = ft_bitmap_new(4, 4, FT_PIXEL_MODE_MONO, bitmap, error)
    if (.not. success) return
    
    ! Test out-of-bounds pixel operations (should not crash)
    call ft_bitmap_set_pixel(bitmap, -1, 0, .true.)
    call ft_bitmap_set_pixel(bitmap, 4, 0, .true.)
    call ft_bitmap_set_pixel(bitmap, 0, -1, .true.)
    call ft_bitmap_set_pixel(bitmap, 0, 4, .true.)
    
    test_count = test_count + 1
    if (.not. ft_bitmap_get_pixel(bitmap, -1, 0) .and. &
        .not. ft_bitmap_get_pixel(bitmap, 4, 0) .and. &
        .not. ft_bitmap_get_pixel(bitmap, 0, -1) .and. &
        .not. ft_bitmap_get_pixel(bitmap, 0, 4)) then
      print '("PASS: Out-of-bounds pixels handled correctly")'
    else
      print '("FAIL: Out-of-bounds pixel handling failed")'
      failed_count = failed_count + 1
    end if
    
    call ft_bitmap_done(bitmap)
    
  end subroutine test_bitmap_pixel_operations
  
  subroutine test_bitmap_conversion()
    type(FT_Bitmap) :: source, target
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing bitmap conversion...")'
    
    ! Create monochrome source
    success = ft_bitmap_new(8, 8, FT_PIXEL_MODE_MONO, source, error)
    if (.not. success) return
    
    ! Set diagonal line
    call ft_bitmap_set_pixel(source, 0, 0, .true.)
    call ft_bitmap_set_pixel(source, 1, 1, .true.)
    call ft_bitmap_set_pixel(source, 2, 2, .true.)
    call ft_bitmap_set_pixel(source, 3, 3, .true.)
    
    ! Convert to grayscale
    test_count = test_count + 1
    success = ft_bitmap_convert(source, target, FT_PIXEL_MODE_GRAY, error)
    
    if (success) then
      print '("PASS: Bitmap converted")'
      
      ! Check converted pixels
      test_count = test_count + 1
      if (ft_bitmap_get_pixel(target, 0, 0) .and. &
          ft_bitmap_get_pixel(target, 1, 1) .and. &
          ft_bitmap_get_pixel(target, 2, 2) .and. &
          ft_bitmap_get_pixel(target, 3, 3)) then
        print '("PASS: Converted pixels correct")'
      else
        print '("FAIL: Converted pixels incorrect")'
        failed_count = failed_count + 1
      end if
      
      call ft_bitmap_done(target)
    else
      print '("FAIL: Bitmap conversion failed")'
      failed_count = failed_count + 1
    end if
    
    call ft_bitmap_done(source)
    
  end subroutine test_bitmap_conversion

end program test_ft_bitmap