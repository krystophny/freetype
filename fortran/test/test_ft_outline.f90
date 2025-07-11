program test_ft_outline
  use ft_types
  use ft_geometry
  use ft_outline_mod
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Run all tests
  call test_outline_creation()
  call test_outline_copy()
  call test_outline_translate()
  call test_outline_transform()
  call test_outline_bbox()
  call test_outline_reverse()
  call test_outline_check()
  
  ! Print test summary
  print '(/, "FT_Outline Tests - Tests run: ", I0)', test_count
  print '("FT_Outline Tests - Tests failed: ", I0)', failed_count
  
  if (failed_count == 0) then
    print '(/, "All FT_Outline tests passed!")'
    stop 0
  else
    print '(/, "Some FT_Outline tests failed!")'
    stop 1
  end if

contains

  subroutine test_outline_creation()
    type(FT_Outline) :: outline
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing outline creation and cleanup...")'
    
    ! Test outline creation
    test_count = test_count + 1
    success = ft_outline_new(5, 2, outline, error)
    
    if (success .and. error == FT_Err_Ok) then
      print '("PASS: Outline created successfully")'
      
      ! Check dimensions
      test_count = test_count + 1
      if (outline%n_points == 5 .and. outline%n_contours == 2) then
        print '("PASS: Correct outline dimensions")'
      else
        print '("FAIL: Incorrect outline dimensions")'
        failed_count = failed_count + 1
      end if
      
      ! Check memory allocation
      test_count = test_count + 1
      if (associated(outline%points) .and. associated(outline%tags) .and. &
          associated(outline%contours)) then
        print '("PASS: Memory allocated correctly")'
      else
        print '("FAIL: Memory not allocated correctly")'
        failed_count = failed_count + 1
      end if
      
      ! Check owner flag
      test_count = test_count + 1
      if (iand(outline%flags, FT_OUTLINE_OWNER) /= 0) then
        print '("PASS: Owner flag set")'
      else
        print '("FAIL: Owner flag not set")'
        failed_count = failed_count + 1
      end if
      
      ! Cleanup
      call ft_outline_done(outline)
      
      test_count = test_count + 1
      if (.not. associated(outline%points) .and. .not. associated(outline%tags) .and. &
          .not. associated(outline%contours)) then
        print '("PASS: Memory freed correctly")'
      else
        print '("FAIL: Memory not freed correctly")'
        failed_count = failed_count + 1
      end if
      
    else
      print '("FAIL: Could not create outline, error: ", I0)', error
      failed_count = failed_count + 1
    end if
    
  end subroutine test_outline_creation
  
  subroutine test_outline_copy()
    type(FT_Outline) :: source, target
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing outline copying...")'
    
    ! Create source outline
    success = ft_outline_new(3, 1, source, error)
    if (.not. success) return
    
    ! Set up source data
    source%points(1) = FT_Vector(100, 200)
    source%points(2) = FT_Vector(300, 400)
    source%points(3) = FT_Vector(500, 600)
    source%tags(1) = FT_CURVE_TAG_ON
    source%tags(2) = FT_CURVE_TAG_CONIC
    source%tags(3) = FT_CURVE_TAG_ON
    source%contours(1) = 2  ! 0-based, so point index 2 is end of contour
    
    ! Copy outline
    test_count = test_count + 1
    success = ft_outline_copy(source, target, error)
    
    if (success) then
      print '("PASS: Outline copied successfully")'
      
      ! Check dimensions
      test_count = test_count + 1
      if (target%n_points == source%n_points .and. &
          target%n_contours == source%n_contours) then
        print '("PASS: Copied outline has correct dimensions")'
      else
        print '("FAIL: Copied outline dimensions incorrect")'
        failed_count = failed_count + 1
      end if
      
      ! Check data
      test_count = test_count + 1
      if (target%points(1)%x == 100 .and. target%points(1)%y == 200 .and. &
          target%points(2)%x == 300 .and. target%points(2)%y == 400 .and. &
          target%tags(1) == FT_CURVE_TAG_ON .and. &
          target%tags(2) == FT_CURVE_TAG_CONIC .and. &
          target%contours(1) == 2) then
        print '("PASS: Copied data is correct")'
      else
        print '("FAIL: Copied data is incorrect")'
        failed_count = failed_count + 1
      end if
      
      call ft_outline_done(target)
    else
      print '("FAIL: Could not copy outline")'
      failed_count = failed_count + 1
    end if
    
    call ft_outline_done(source)
    
  end subroutine test_outline_copy
  
  subroutine test_outline_translate()
    type(FT_Outline) :: outline
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing outline translation...")'
    
    ! Create outline
    success = ft_outline_new(2, 1, outline, error)
    if (.not. success) return
    
    outline%points(1) = FT_Vector(100, 200)
    outline%points(2) = FT_Vector(300, 400)
    
    ! Translate outline
    call ft_outline_translate(outline, 50_FT_Fixed, -25_FT_Fixed)
    
    test_count = test_count + 1
    if (outline%points(1)%x == 150 .and. outline%points(1)%y == 175 .and. &
        outline%points(2)%x == 350 .and. outline%points(2)%y == 375) then
      print '("PASS: Outline translated correctly")'
    else
      print '("FAIL: Outline translation incorrect")'
      failed_count = failed_count + 1
    end if
    
    call ft_outline_done(outline)
    
  end subroutine test_outline_translate
  
  subroutine test_outline_transform()
    type(FT_Outline) :: outline
    type(FT_Matrix) :: matrix
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing outline transformation...")'
    
    ! Create outline
    success = ft_outline_new(1, 1, outline, error)
    if (.not. success) return
    
    outline%points(1) = FT_Vector(100, 200)
    
    ! Create 2x scale matrix
    matrix%xx = 2 * 65536  ! 2.0 in 16.16 fixed point
    matrix%xy = 0
    matrix%yx = 0
    matrix%yy = 2 * 65536  ! 2.0 in 16.16 fixed point
    
    ! Transform outline
    call ft_outline_transform(outline, matrix)
    
    test_count = test_count + 1
    if (outline%points(1)%x == 200 .and. outline%points(1)%y == 400) then
      print '("PASS: Outline transformed correctly")'
    else
      print '("FAIL: Outline transformation incorrect")'
      print '("Expected: (200, 400), Got: (", I0, ", ", I0, ")")', &
        outline%points(1)%x, outline%points(1)%y
      failed_count = failed_count + 1
    end if
    
    call ft_outline_done(outline)
    
  end subroutine test_outline_transform
  
  subroutine test_outline_bbox()
    type(FT_Outline) :: outline
    type(FT_BBox) :: bbox
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing outline bounding box...")'
    
    ! Create outline with triangle
    success = ft_outline_new(3, 1, outline, error)
    if (.not. success) return
    
    outline%points(1) = FT_Vector(100, 200)  ! Bottom-left
    outline%points(2) = FT_Vector(500, 200)  ! Bottom-right
    outline%points(3) = FT_Vector(300, 600)  ! Top
    outline%contours(1) = 2
    
    ! Get bounding box
    test_count = test_count + 1
    success = ft_outline_get_bbox(outline, bbox, error)
    
    if (success) then
      print '("PASS: Bounding box computed")'
      
      test_count = test_count + 1
      if (bbox%xMin == 100 .and. bbox%xMax == 500 .and. &
          bbox%yMin == 200 .and. bbox%yMax == 600) then
        print '("PASS: Bounding box values correct")'
      else
        print '("FAIL: Bounding box values incorrect")'
        print '("Got: xMin=", I0, " xMax=", I0, " yMin=", I0, " yMax=", I0)', &
          bbox%xMin, bbox%xMax, bbox%yMin, bbox%yMax
        failed_count = failed_count + 1
      end if
    else
      print '("FAIL: Could not compute bounding box")'
      failed_count = failed_count + 1
    end if
    
    call ft_outline_done(outline)
    
  end subroutine test_outline_bbox
  
  subroutine test_outline_reverse()
    type(FT_Outline) :: outline
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing outline reversal...")'
    
    ! Create outline with 4 points in one contour
    success = ft_outline_new(4, 1, outline, error)
    if (.not. success) return
    
    outline%points(1) = FT_Vector(100, 100)
    outline%points(2) = FT_Vector(200, 100)
    outline%points(3) = FT_Vector(200, 200)
    outline%points(4) = FT_Vector(100, 200)
    outline%tags = FT_CURVE_TAG_ON
    outline%contours(1) = 3  ! 0-based, point 3 is end
    
    ! Reverse outline
    call ft_outline_reverse(outline)
    
    test_count = test_count + 1
    if (outline%points(1)%x == 100 .and. outline%points(1)%y == 200 .and. &
        outline%points(2)%x == 200 .and. outline%points(2)%y == 200 .and. &
        outline%points(3)%x == 200 .and. outline%points(3)%y == 100 .and. &
        outline%points(4)%x == 100 .and. outline%points(4)%y == 100) then
      print '("PASS: Outline reversed correctly")'
    else
      print '("FAIL: Outline reversal incorrect")'
      failed_count = failed_count + 1
    end if
    
    call ft_outline_done(outline)
    
  end subroutine test_outline_reverse
  
  subroutine test_outline_check()
    type(FT_Outline) :: outline
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing outline validation...")'
    
    ! Test valid outline
    success = ft_outline_new(3, 1, outline, error)
    if (.not. success) return
    
    outline%contours(1) = 2  ! Valid endpoint
    
    test_count = test_count + 1
    success = ft_outline_check(outline, error)
    
    if (success) then
      print '("PASS: Valid outline passes check")'
    else
      print '("FAIL: Valid outline fails check, error: ", I0)', error
      failed_count = failed_count + 1
    end if
    
    ! Test invalid outline (bad endpoint)
    outline%contours(1) = 5  ! Invalid endpoint (beyond n_points)
    
    test_count = test_count + 1
    success = ft_outline_check(outline, error)
    
    if (.not. success .and. error == FT_Err_Invalid_Outline) then
      print '("PASS: Invalid outline detected")'
    else
      print '("FAIL: Invalid outline not detected")'
      failed_count = failed_count + 1
    end if
    
    call ft_outline_done(outline)
    
  end subroutine test_outline_check

end program test_ft_outline