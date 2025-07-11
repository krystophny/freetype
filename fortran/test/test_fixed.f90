program test_fixed
  use ft_types
  use, intrinsic :: iso_c_binding
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Test fixed-point functionality
  call test_fixed_conversions()
  call test_fixed_arithmetic()
  
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

  subroutine test_fixed_conversions()
    integer(FT_Fixed) :: fixed
    integer :: int_val
    real :: float_val
    real :: epsilon = 1.0e-4
    
    print '(/, "Testing fixed-point conversions...")'
    
    ! Test integer to fixed conversion
    test_count = test_count + 1
    fixed = ft_fixed_from_int(10)
    int_val = ft_fixed_to_int(fixed)
    if (int_val /= 10) then
      print '("FAIL: Integer conversion - expected 10, got ", I0)', int_val
      failed_count = failed_count + 1
    else
      print '("PASS: Integer conversion")'
    end if
    
    ! Test float to fixed conversion
    test_count = test_count + 1
    fixed = ft_fixed_from_float(3.14159)
    float_val = ft_fixed_to_float(fixed)
    if (abs(float_val - 3.14159) > epsilon) then
      print '("FAIL: Float conversion - expected 3.14159, got ", F0.5)', float_val
      failed_count = failed_count + 1
    else
      print '("PASS: Float conversion")'
    end if
    
    ! Test negative values
    test_count = test_count + 1
    fixed = ft_fixed_from_int(-5)
    int_val = ft_fixed_to_int(fixed)
    if (int_val /= -5) then
      print '("FAIL: Negative integer conversion - expected -5, got ", I0)', int_val
      failed_count = failed_count + 1
    else
      print '("PASS: Negative value conversion")'
    end if
    
  end subroutine test_fixed_conversions
  
  subroutine test_fixed_arithmetic()
    integer(FT_Fixed) :: a, b, c
    real :: result
    real :: epsilon = 1.0e-4
    
    print '(/, "Testing fixed-point arithmetic...")'
    
    ! Test addition
    test_count = test_count + 1
    a = ft_fixed_from_float(2.5)
    b = ft_fixed_from_float(1.5)
    c = ft_fixed_add(a, b)
    result = ft_fixed_to_float(c)
    if (abs(result - 4.0) > epsilon) then
      print '("FAIL: Addition - expected 4.0, got ", F0.5)', result
      failed_count = failed_count + 1
    else
      print '("PASS: Addition")'
    end if
    
    ! Test subtraction
    test_count = test_count + 1
    a = ft_fixed_from_float(5.0)
    b = ft_fixed_from_float(2.0)
    c = ft_fixed_sub(a, b)
    result = ft_fixed_to_float(c)
    if (abs(result - 3.0) > epsilon) then
      print '("FAIL: Subtraction - expected 3.0, got ", F0.5)', result
      failed_count = failed_count + 1
    else
      print '("PASS: Subtraction")'
    end if
    
    ! Test multiplication
    test_count = test_count + 1
    a = ft_fixed_from_float(2.0)
    b = ft_fixed_from_float(3.0)
    c = ft_fixed_mul(a, b)
    result = ft_fixed_to_float(c)
    if (abs(result - 6.0) > epsilon) then
      print '("FAIL: Multiplication - expected 6.0, got ", F0.5)', result
      failed_count = failed_count + 1
    else
      print '("PASS: Multiplication")'
    end if
    
    ! Test division
    test_count = test_count + 1
    a = ft_fixed_from_float(10.0)
    b = ft_fixed_from_float(4.0)
    c = ft_fixed_div(a, b)
    result = ft_fixed_to_float(c)
    if (abs(result - 2.5) > epsilon) then
      print '("FAIL: Division - expected 2.5, got ", F0.5)', result
      failed_count = failed_count + 1
    else
      print '("PASS: Division")'
    end if
    
    ! Test fractional multiplication
    test_count = test_count + 1
    a = ft_fixed_from_float(0.5)
    b = ft_fixed_from_float(0.5)
    c = ft_fixed_mul(a, b)
    result = ft_fixed_to_float(c)
    if (abs(result - 0.25) > epsilon) then
      print '("FAIL: Fractional multiplication - expected 0.25, got ", F0.5)', result
      failed_count = failed_count + 1
    else
      print '("PASS: Fractional multiplication")'
    end if
    
  end subroutine test_fixed_arithmetic

end program test_fixed