program test_f26dot6
  use ft_types
  use, intrinsic :: iso_c_binding
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Test F26Dot6 functionality
  call test_f26dot6_conversions()
  
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

  subroutine test_f26dot6_conversions()
    integer(FT_F26Dot6) :: f26dot6
    integer :: int_val
    real :: float_val
    real :: epsilon = 1.0e-2  ! 26.6 has less precision than 16.16
    
    print '(/, "Testing F26Dot6 conversions...")'
    
    ! Test integer to F26Dot6 conversion
    test_count = test_count + 1
    f26dot6 = ft_f26dot6_from_int(10)
    int_val = ft_f26dot6_to_int(f26dot6)
    if (int_val /= 10) then
      print '("FAIL: Integer conversion - expected 10, got ", I0)', int_val
      failed_count = failed_count + 1
    else
      print '("PASS: Integer conversion")'
    end if
    
    ! Test float to F26Dot6 conversion
    test_count = test_count + 1
    f26dot6 = ft_f26dot6_from_float(3.5)
    float_val = ft_f26dot6_to_float(f26dot6)
    if (abs(float_val - 3.5) > epsilon) then
      print '("FAIL: Float conversion - expected 3.5, got ", F0.3)', float_val
      failed_count = failed_count + 1
    else
      print '("PASS: Float conversion")'
    end if
    
    ! Test pixel conversion
    test_count = test_count + 1
    f26dot6 = ft_f26dot6_from_pixels(100)
    int_val = ft_f26dot6_to_int(f26dot6)
    if (int_val /= 100) then
      print '("FAIL: Pixel conversion - expected 100, got ", I0)', int_val
      failed_count = failed_count + 1
    else
      print '("PASS: Pixel conversion")'
    end if
    
    ! Test fractional pixel coordinates
    test_count = test_count + 1
    f26dot6 = ft_f26dot6_from_float(10.25)
    float_val = ft_f26dot6_to_float(f26dot6)
    if (abs(float_val - 10.25) > epsilon) then
      print '("FAIL: Fractional pixel - expected 10.25, got ", F0.3)', float_val
      failed_count = failed_count + 1
    else
      print '("PASS: Fractional pixel conversion")'
    end if
    
  end subroutine test_f26dot6_conversions

end program test_f26dot6