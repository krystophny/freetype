program test_font_format_file
  use ft_types
  use ft_stream
  use ft_font_format
  implicit none
  
  integer :: total_tests = 0
  integer :: passed_tests = 0
  
  print '("Font Format Detection Tests (File-based)")'
  print '("========================================")'
  print '()'
  
  ! Test with files
  call test_cff_file_detection(passed_tests, total_tests)
  
  ! Summary
  print '()'
  print '("===== TEST SUMMARY =====")'
  print '("Total tests: ", I0)', total_tests
  print '("Passed tests: ", I0)', passed_tests
  print '("Failed tests: ", I0)', total_tests - passed_tests
  print '("Success rate: ", F5.1, "%")', real(passed_tests) / real(total_tests) * 100.0
  
  if (passed_tests == total_tests) then
    print '("✓ All font format tests PASSED!")'
  else
    print '("✗ Some font format tests FAILED!")'
  end if
  
contains

  ! Test CFF file detection
  subroutine test_cff_file_detection(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_Stream_Type) :: stream
    integer :: format
    integer(FT_Error) :: error
    logical :: success
    character(len=256) :: temp_file
    integer :: unit, iostat
    
    ! CFF header data
    character(len=1), parameter :: cff_data(*) = [ &
      char(1), char(0), char(4), char(1), &    ! major=1, minor=0, hdr_size=4, offset_size=1
      char(0), char(0), char(0), char(0), &    ! padding
      char(0), char(0), char(0), char(0) &     ! more padding
    ]
    
    total = total + 1
    print '("Test ", I0, ": CFF file detection")', total
    
    ! Create temporary file
    temp_file = "test_cff_format.cff"
    open(newunit=unit, file=temp_file, form='unformatted', access='stream', &
         status='replace', iostat=iostat)
    if (iostat /= 0) then
      print '("  ERROR: Could not create temp file")'
      return
    end if
    
    ! Write CFF data
    write(unit) cff_data
    close(unit)
    
    ! Open stream
    if (.not. ft_stream_open(stream, temp_file, error)) then
      print '("  ERROR: Could not open stream, error=", I0)', error
      call execute_command_line("rm -f " // trim(temp_file))
      return
    end if
    
    success = ft_detect_font_format(stream, format, error)
    if (.not. success) then
      print '("  ERROR: Detection failed, error=", I0)', error
      call ft_stream_close(stream)
      call execute_command_line("rm -f " // trim(temp_file))
      return
    end if
    
    if (format == FT_FONT_FORMAT_CFF) then
      print '("  Format detected: ", A)', trim(ft_get_format_name(format))
      print '("  Result: PASS")'
      passed = passed + 1
    else
      print '("  Format detected: ", A)', trim(ft_get_format_name(format))
      print '("  Result: FAIL - Expected CFF")'
    end if
    
    call ft_stream_close(stream)
    call execute_command_line("rm -f " // trim(temp_file))
    
  end subroutine test_cff_file_detection

end program test_font_format_file