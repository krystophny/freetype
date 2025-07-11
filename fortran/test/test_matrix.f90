program test_matrix
  use ft_types
  use ft_geometry
  use, intrinsic :: iso_c_binding
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Test matrix functionality
  call test_matrix_operations()
  call test_vector_transform()
  
  ! Print test summary
  print '(/, "Tests run: ", I0)', test_count
  print '("Tests failed: ", I0)', failed_count
  
  if (failed_count == 0) then
    print '(/, "All tests passed!")'
    stop 0
  else
    print '(/, "Some tests failed!")'
    stop 1
  end if

contains

  subroutine test_matrix_operations()
    type(FT_Matrix) :: a, b, c, inv
    logical :: success
    
    print '(/, "Testing matrix operations...")'
    
    ! Test identity matrix multiplication
    test_count = test_count + 1
    ! Create identity matrix
    a%xx = FT_FIXED_ONE
    a%xy = 0
    a%yx = 0
    a%yy = FT_FIXED_ONE
    
    ! Create another matrix
    b%xx = ft_fixed_from_int(2)
    b%xy = ft_fixed_from_int(1)
    b%yx = ft_fixed_from_int(0)
    b%yy = ft_fixed_from_int(3)
    
    c = ft_matrix_multiply(a, b)
    
    if (c%xx /= b%xx .or. c%xy /= b%xy .or. c%yx /= b%yx .or. c%yy /= b%yy) then
      print '("FAIL: Identity matrix multiplication")'
      failed_count = failed_count + 1
    else
      print '("PASS: Identity matrix multiplication")'
    end if
    
    ! Test matrix inversion
    test_count = test_count + 1
    ! Simple 2x2 matrix with determinant = 2
    a%xx = ft_fixed_from_int(3)
    a%xy = ft_fixed_from_int(1)
    a%yx = ft_fixed_from_int(1)
    a%yy = ft_fixed_from_int(1)
    
    call ft_matrix_invert(a, inv, success)
    
    if (.not. success) then
      print '("FAIL: Matrix inversion failed")'
      failed_count = failed_count + 1
    else
      ! Verify A * A^-1 = I
      c = ft_matrix_multiply(a, inv)
      if (abs(ft_fixed_to_int(c%xx) - 1) > 0 .or. &
          ft_fixed_to_int(c%xy) /= 0 .or. &
          ft_fixed_to_int(c%yx) /= 0 .or. &
          abs(ft_fixed_to_int(c%yy) - 1) > 0) then
        print '("FAIL: Matrix inversion verification")'
        failed_count = failed_count + 1
      else
        print '("PASS: Matrix inversion")'
      end if
    end if
    
  end subroutine test_matrix_operations
  
  subroutine test_vector_transform()
    type(FT_Vector) :: vec, result
    type(FT_Matrix) :: matrix
    
    print '(/, "Testing vector transformation...")'
    
    ! Test rotation by 90 degrees (simplified)
    test_count = test_count + 1
    
    ! Create vector (1, 0)
    vec%x = FT_FIXED_ONE
    vec%y = 0
    
    ! Create 90-degree rotation matrix
    ! [0 -1]
    ! [1  0]
    matrix%xx = 0
    matrix%xy = -FT_FIXED_ONE
    matrix%yx = FT_FIXED_ONE
    matrix%yy = 0
    
    result = ft_vector_transform(vec, matrix)
    
    ! Should get (0, 1)
    if (ft_fixed_to_int(result%x) /= 0 .or. ft_fixed_to_int(result%y) /= 1) then
      print '("FAIL: Vector rotation")'
      failed_count = failed_count + 1
    else
      print '("PASS: Vector rotation")'
    end if
    
    ! Test scaling
    test_count = test_count + 1
    
    vec%x = ft_fixed_from_int(2)
    vec%y = ft_fixed_from_int(3)
    
    ! Create scaling matrix (2x, 3x)
    matrix%xx = ft_fixed_from_int(2)
    matrix%xy = 0
    matrix%yx = 0
    matrix%yy = ft_fixed_from_int(3)
    
    result = ft_vector_transform(vec, matrix)
    
    if (ft_fixed_to_int(result%x) /= 4 .or. ft_fixed_to_int(result%y) /= 9) then
      print '("FAIL: Vector scaling")'
      failed_count = failed_count + 1
    else
      print '("PASS: Vector scaling")'
    end if
    
  end subroutine test_vector_transform

end program test_matrix