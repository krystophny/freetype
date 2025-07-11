program test_vector
  use ft_types
  use ft_geometry
  use, intrinsic :: iso_c_binding
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Test vector functionality
  call test_vector_creation()
  call test_vector_operations()
  
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

  subroutine test_vector_creation()
    type(FT_Vector) :: vec
    
    print '(/, "Testing vector creation...")'
    
    ! Test vector initialization
    test_count = test_count + 1
    vec%x = ft_fixed_from_int(10)
    vec%y = ft_fixed_from_int(20)
    
    if (ft_fixed_to_int(vec%x) /= 10 .or. ft_fixed_to_int(vec%y) /= 20) then
      print '("FAIL: Vector creation")'
      failed_count = failed_count + 1
    else
      print '("PASS: Vector creation")'
    end if
  end subroutine test_vector_creation
  
  subroutine test_vector_operations()
    type(FT_Vector) :: a, b, c
    integer(FT_Fixed) :: result
    
    print '(/, "Testing vector operations...")'
    
    ! Initialize test vectors
    a%x = ft_fixed_from_int(3)
    a%y = ft_fixed_from_int(4)
    
    b%x = ft_fixed_from_int(1)
    b%y = ft_fixed_from_int(2)
    
    ! Test vector addition
    test_count = test_count + 1
    c = ft_vector_add(a, b)
    if (ft_fixed_to_int(c%x) /= 4 .or. ft_fixed_to_int(c%y) /= 6) then
      print '("FAIL: Vector addition")'
      failed_count = failed_count + 1
    else
      print '("PASS: Vector addition")'
    end if
    
    ! Test vector subtraction
    test_count = test_count + 1
    c = ft_vector_sub(a, b)
    if (ft_fixed_to_int(c%x) /= 2 .or. ft_fixed_to_int(c%y) /= 2) then
      print '("FAIL: Vector subtraction")'
      failed_count = failed_count + 1
    else
      print '("PASS: Vector subtraction")'
    end if
    
    ! Test dot product
    test_count = test_count + 1
    result = ft_vector_dot(a, b)
    ! a·b = 3*1 + 4*2 = 3 + 8 = 11
    if (ft_fixed_to_int(result) /= 11) then
      print '("FAIL: Vector dot product - expected 11, got ", I0)', ft_fixed_to_int(result)
      failed_count = failed_count + 1
    else
      print '("PASS: Vector dot product")'
    end if
    
  end subroutine test_vector_operations

end program test_vector