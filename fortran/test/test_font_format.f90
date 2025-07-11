program test_font_format
  use ft_types
  use ft_stream
  use ft_font_format
  implicit none
  
  integer :: total_tests = 0
  integer :: passed_tests = 0
  
  print '("Font Format Detection Tests")'
  print '("===========================")'
  print '()'
  
  ! Test 1: TrueType detection
  call test_truetype_detection(passed_tests, total_tests)
  
  ! Test 2: CFF detection
  call test_cff_detection(passed_tests, total_tests)
  
  ! Test 3: Type 1 detection
  call test_type1_detection(passed_tests, total_tests)
  
  ! Test 4: OpenType detection
  call test_opentype_detection(passed_tests, total_tests)
  
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

  ! Test TrueType detection
  subroutine test_truetype_detection(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_Stream_Type) :: stream
    integer :: format
    integer(FT_Error) :: error
    logical :: success
    
    ! TrueType header data
    character(len=12) :: tt_data = char(0) // char(1) // char(0) // char(0) // &
                                   char(0) // char(12) // char(0) // char(0) // &
                                   char(0) // char(0) // char(0) // char(0)
    
    total = total + 1
    print '("Test ", I0, ": TrueType detection")', total
    
    ! Create memory stream
    if (.not. ft_stream_open_memory(stream, tt_data, len(tt_data), error)) then
      print '("  ERROR: Could not create memory stream")'
      return
    end if
    
    success = ft_detect_font_format(stream, format, error)
    if (.not. success) then
      print '("  ERROR: Detection failed, error=", I0)', error
      call ft_stream_close(stream)
      return
    end if
    
    if (format == FT_FONT_FORMAT_TRUETYPE) then
      print '("  Format detected: ", A)', trim(ft_get_format_name(format))
      print '("  Result: PASS")'
      passed = passed + 1
    else
      print '("  Format detected: ", A)', trim(ft_get_format_name(format))
      print '("  Result: FAIL - Expected TrueType")'
    end if
    
    call ft_stream_close(stream)
    
  end subroutine test_truetype_detection

  ! Test CFF detection
  subroutine test_cff_detection(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_Stream_Type) :: stream
    integer :: format
    integer(FT_Error) :: error
    logical :: success
    
    ! CFF header data
    character(len=12) :: cff_data = char(1) // char(0) // char(4) // char(1) // &
                                    char(0) // char(0) // char(0) // char(0) // &
                                    char(0) // char(0) // char(0) // char(0)
    
    total = total + 1
    print '("Test ", I0, ": CFF detection")', total
    
    ! Create memory stream
    if (.not. ft_stream_open_memory(stream, cff_data, len(cff_data), error)) then
      print '("  ERROR: Could not create memory stream")'
      return
    end if
    
    success = ft_detect_font_format(stream, format, error)
    if (.not. success) then
      print '("  ERROR: Detection failed, error=", I0)', error
      call ft_stream_close(stream)
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
    
  end subroutine test_cff_detection

  ! Test Type 1 detection
  subroutine test_type1_detection(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_Stream_Type) :: stream
    integer :: format
    integer(FT_Error) :: error
    logical :: success
    
    ! Type 1 PFA header data
    character(len=16) :: pfa_data = '%!PS-AdobeFont-1' 
    
    total = total + 1
    print '("Test ", I0, ": Type 1 detection")', total
    
    ! Create memory stream
    if (.not. ft_stream_open_memory(stream, pfa_data, len(pfa_data), error)) then
      print '("  ERROR: Could not create memory stream")'
      return
    end if
    
    success = ft_detect_font_format(stream, format, error)
    if (.not. success) then
      print '("  ERROR: Detection failed, error=", I0)', error
      call ft_stream_close(stream)
      return
    end if
    
    if (format == FT_FONT_FORMAT_TYPE1) then
      print '("  Format detected: ", A)', trim(ft_get_format_name(format))
      print '("  Result: PASS")'
      passed = passed + 1
    else
      print '("  Format detected: ", A)', trim(ft_get_format_name(format))
      print '("  Result: FAIL - Expected Type 1")'
    end if
    
    call ft_stream_close(stream)
    
  end subroutine test_type1_detection

  ! Test OpenType detection
  subroutine test_opentype_detection(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_Stream_Type) :: stream
    integer :: format
    integer(FT_Error) :: error
    logical :: success
    
    ! OpenType with CFF data
    character(len=12) :: otto_data = 'OTTO' // char(0) // char(0) // char(0) // char(0) // &
                                     char(0) // char(0) // char(0) // char(0)
    
    total = total + 1
    print '("Test ", I0, ": OpenType detection")', total
    
    ! Create memory stream
    if (.not. ft_stream_open_memory(stream, otto_data, len(otto_data), error)) then
      print '("  ERROR: Could not create memory stream")'
      return
    end if
    
    success = ft_detect_font_format(stream, format, error)
    if (.not. success) then
      print '("  ERROR: Detection failed, error=", I0)', error
      call ft_stream_close(stream)
      return
    end if
    
    if (format == FT_FONT_FORMAT_OPENTYPE) then
      print '("  Format detected: ", A)', trim(ft_get_format_name(format))
      print '("  Result: PASS")'
      passed = passed + 1
    else
      print '("  Format detected: ", A)', trim(ft_get_format_name(format))
      print '("  Result: FAIL - Expected OpenType")'
    end if
    
    call ft_stream_close(stream)
    
  end subroutine test_opentype_detection

end program test_font_format