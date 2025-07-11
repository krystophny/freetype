program test_types
  use ft_types
  use, intrinsic :: iso_c_binding
  implicit none

  integer :: test_count = 0
  integer :: failed_count = 0

  ! Test that FT_Error is the same as C int
  call test_error_type_compatibility()
  
  ! Test error code values
  call test_error_code_values()
  
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

  subroutine test_error_type_compatibility()
    integer(FT_Error) :: err
    integer(c_int) :: c_err
    
    test_count = test_count + 1
    
    ! Check that FT_Error has the same size as c_int
    if (storage_size(err) /= storage_size(c_err)) then
      print '("FAIL: FT_Error size mismatch")'
      failed_count = failed_count + 1
    else
      print '("PASS: FT_Error type compatibility")'
    end if
  end subroutine test_error_type_compatibility
  
  subroutine test_error_code_values()
    ! Test that error codes have expected values
    test_count = test_count + 1
    
    if (FT_Err_Ok /= 0) then
      print '("FAIL: FT_Err_Ok should be 0, got ", I0)', FT_Err_Ok
      failed_count = failed_count + 1
      return
    end if
    
    if (FT_Err_Cannot_Open_Resource /= 1) then
      print '("FAIL: FT_Err_Cannot_Open_Resource should be 1, got ", I0)', &
        FT_Err_Cannot_Open_Resource
      failed_count = failed_count + 1
      return
    end if
    
    if (FT_Err_Out_Of_Memory /= 64) then
      print '("FAIL: FT_Err_Out_Of_Memory should be 64, got ", I0)', &
        FT_Err_Out_Of_Memory
      failed_count = failed_count + 1
      return
    end if
    
    if (FT_Err_Invalid_Stream_Skip /= 83) then
      print '("FAIL: FT_Err_Invalid_Stream_Skip should be 83, got ", I0)', &
        FT_Err_Invalid_Stream_Skip
      failed_count = failed_count + 1
      return
    end if
    
    print '("PASS: Error code values")'
  end subroutine test_error_code_values

end program test_types