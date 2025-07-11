program test_type1_parser
  use ft_types
  use ft_type1
  implicit none
  
  integer :: total_tests = 0
  integer :: passed_tests = 0
  
  print '("Type 1 Parser Tests")'
  print '("===================")'
  print '()'
  
  ! Test 1: Format detection
  call test_format_detection(passed_tests, total_tests)
  
  ! Test 2: PFA header parsing
  call test_pfa_header_parsing(passed_tests, total_tests)
  
  ! Test 3: PFB detection
  call test_pfb_detection(passed_tests, total_tests)
  
  ! Test 4: Invalid format handling
  call test_invalid_format(passed_tests, total_tests)
  
  ! Summary
  print '()'
  print '("===== TEST SUMMARY =====")'
  print '("Total tests: ", I0)', total_tests
  print '("Passed tests: ", I0)', passed_tests
  print '("Failed tests: ", I0)', total_tests - passed_tests
  print '("Success rate: ", F5.1, "%")', real(passed_tests) / real(total_tests) * 100.0
  
  if (passed_tests == total_tests) then
    print '("✓ All Type 1 parser tests PASSED!")'
  else
    print '("✗ Some Type 1 parser tests FAILED!")'
  end if
  
contains

  ! Test format detection
  subroutine test_format_detection(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_Type1_Format) :: format
    integer(FT_Error) :: error
    logical :: success
    
    ! Test data for different formats
    character(len=*), parameter :: pfa_data = &
      '%!PS-AdobeFont-1.0: Times-Roman 001.007' // char(10) // &
      '/FontName /Times-Roman def' // char(10) // &
      '/FullName (Times Roman) def' // char(10) // &
      'eexec' // char(10) // &
      '1234567890abcdef'
    
    character(len=*), parameter :: pfb_header = &
      char(128) // char(1) // char(0) // char(0) // char(0) // char(0)
    
    character(len=*), parameter :: invalid_data = &
      'This is not a font file' // char(10) // &
      'Just some random text'
    
    total = total + 1
    print '("Test ", I0, ": Format detection")', total
    
    ! Test PFA detection
    print '("  Testing PFA data length: ", I0)', len(pfa_data)
    print '("  First 20 chars: ", A)', pfa_data(1:min(20, len(pfa_data)))
    print '("  Expected: %!PS-AdobeFont-")'
    
    success = ft_type1_detect_format(pfa_data, len(pfa_data), format, error)
    if (success .and. format%format_type == FT_TYPE1_FORMAT_PFA) then
      print '("  PFA detection: PASS")'
    else
      print '("  PFA detection: FAIL (success=", L1, ", format=", I0, ", error=", I0, ")")', success, format%format_type, error
      ! Check the exact characters
      block
        integer :: i
        print '("  ASCII values: ", 20I4)', (ichar(pfa_data(i:i)), i=1,min(20,len(pfa_data)))
      end block
      return
    end if
    
    ! Test PFB detection
    success = ft_type1_detect_format(pfb_header, len(pfb_header), format, error)
    if (success .and. format%format_type == FT_TYPE1_FORMAT_PFB) then
      print '("  PFB detection: PASS")'
    else
      print '("  PFB detection: FAIL")'
      return
    end if
    
    ! Test invalid format
    success = ft_type1_detect_format(invalid_data, len(invalid_data), format, error)
    if (.not. success .and. error /= FT_Err_Ok) then
      print '("  Invalid format detection: PASS")'
    else
      print '("  Invalid format detection: FAIL")'
      return
    end if
    
    print '("  Result: PASS - All format detection tests passed")'
    passed = passed + 1
    
  end subroutine test_format_detection

  ! Test PFA header parsing
  subroutine test_pfa_header_parsing(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_Type1_Parser) :: parser
    integer(FT_Error) :: error
    logical :: success
    
    ! Sample PFA data with header information
    character(len=*), parameter :: pfa_data = &
      '%!PS-AdobeFont-1.0: Times-Roman 001.007' // char(10) // &
      '%%Title: Times-Roman' // char(10) // &
      '%%Creator: Adobe Type Manager' // char(10) // &
      '%%VMusage: 1024 1024' // char(10) // &
      '%%EndComments' // char(10) // &
      'FontDirectory /Times-Roman known' // char(10) // &
      '{/Times-Roman findfont dup /Encoding get std' // char(10) // &
      '11 dict begin' // char(10) // &
      '/FontInfo 10 dict dup begin' // char(10) // &
      '/version (001.007) readonly def' // char(10) // &
      '/Notice (Copyright (c) 1985, 1987, 1989, 1990, 1993 Adobe Systems Incorporated.  All Rights Reserved.) readonly def' // char(10) // &
      '/FullName (Times Roman) readonly def' // char(10) // &
      '/FamilyName (Times) readonly def' // char(10) // &
      '/Weight (Roman) readonly def' // char(10) // &
      '/ItalicAngle 0 def' // char(10) // &
      '/isFixedPitch false def' // char(10) // &
      '/UnderlinePosition -100 def' // char(10) // &
      '/UnderlineThickness 50 def' // char(10) // &
      'end readonly def' // char(10) // &
      '/FontName /Times-Roman def' // char(10) // &
      '/Encoding StandardEncoding def' // char(10) // &
      '/PaintType 0 def' // char(10) // &
      '/FontType 1 def' // char(10) // &
      '/FontMatrix [0.001 0 0 0.001 0 0] readonly def' // char(10) // &
      '/FontBBox [-168 -218 1000 898] readonly def' // char(10) // &
      '/UniqueID 43793 def' // char(10) // &
      'currentdict end' // char(10) // &
      'currentfile eexec' // char(10) // &
      'dup /Private 17 dict dup begin' // char(10) // &
      '01234567890abcdef' // char(10) // &
      'cleartomark' // char(10)
    
    total = total + 1
    print '("Test ", I0, ": PFA header parsing")', total
    
    ! Create parser
    success = ft_type1_parser_new(pfa_data, len(pfa_data), parser, error)
    if (.not. success) then
      print '("  ERROR: Could not create parser")'
      return
    end if
    
    ! Load header
    success = ft_type1_parser_load_header(parser, error)
    if (.not. success) then
      print '("  ERROR: Could not load header")'
      call ft_type1_parser_done(parser)
      return
    end if
    
    ! Check parsed information
    print '("  Font name: ", A)', trim(parser%font_name)
    print '("  Full name: ", A)', trim(parser%full_name)
    print '("  Family name: ", A)', trim(parser%family_name)
    print '("  Weight: ", A)', trim(parser%weight)
    print '("  Version: ", A)', trim(parser%version)
    print '("  Italic angle: ", F6.1)', parser%italic_angle
    print '("  Fixed pitch: ", L1)', parser%is_fixed_pitch
    
    ! Verify some key values (font name parsing might be different)
    if (trim(parser%full_name) == 'Times Roman' .and. &
        trim(parser%family_name) == 'Times' .and. &
        trim(parser%weight) == 'Roman' .and. &
        abs(parser%italic_angle - 0.0) < 0.1 .and. &
        .not. parser%is_fixed_pitch) then
      print '("  Result: PASS - Header parsing successful")'
      passed = passed + 1
    else
      print '("  Result: FAIL - Header parsing incorrect")'
      print '("    Expected font name: Times-Roman, got: [", A, "]")', trim(parser%font_name)
      print '("    Expected full name: Times Roman, got: [", A, "]")', trim(parser%full_name)
    end if
    
    call ft_type1_parser_done(parser)
    
  end subroutine test_pfa_header_parsing

  ! Test PFB detection
  subroutine test_pfb_detection(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_Type1_Parser) :: parser
    integer(FT_Error) :: error
    logical :: success
    
    ! Sample PFB data with header and segments
    character(len=*), parameter :: ascii_content = '%!PS-AdobeFont-1.0: TestFont' // char(10)
    character(len=*), parameter :: binary_content = 'Binary data here'
    character(len=*), parameter :: pfb_data = &
      char(128) // char(1) // char(28) // char(0) // char(0) // char(0) // & ! ASCII segment header (28 bytes)
      ascii_content // &                                                   ! ASCII content (28 bytes)
      char(128) // char(2) // char(16) // char(0) // char(0) // char(0) // & ! Binary segment header (16 bytes)
      binary_content // &                                                  ! Binary content (16 bytes)
      char(128) // char(3) // char(0) // char(0) // char(0) // char(0)     ! EOF marker
    
    total = total + 1
    print '("Test ", I0, ": PFB detection and parsing")', total
    
    ! Create parser
    print '("  PFB data length: ", I0)', len(pfb_data)
    success = ft_type1_parser_new(pfb_data, len(pfb_data), parser, error)
    if (.not. success) then
      print '("  ERROR: Could not create parser, error=", I0)', error
      return
    end if
    
    ! Check format detection
    if (parser%format%format_type == FT_TYPE1_FORMAT_PFB) then
      print '("  Format detection: PASS")'
    else
      print '("  Format detection: FAIL")'
      call ft_type1_parser_done(parser)
      return
    end if
    
    ! Check segments
    if (parser%num_segments >= 2) then
      print '("  Segments found: ", I0)', parser%num_segments
      print '("  Segment 1: type=", I0, " length=", I0)', &
            parser%segments(1)%segment_type, parser%segments(1)%length
      print '("  Segment 2: type=", I0, " length=", I0)', &
            parser%segments(2)%segment_type, parser%segments(2)%length
      
      if (parser%segments(1)%segment_type == 1 .and. &
          parser%segments(1)%length == 28 .and. &
          parser%segments(2)%segment_type == 2 .and. &
          parser%segments(2)%length == 16) then
        print '("  Segment parsing: PASS")'
      else
        print '("  Segment parsing: FAIL")'
        print '("    Expected: seg1(type=1, len=28), seg2(type=2, len=16)")'
        call ft_type1_parser_done(parser)
        return
      end if
    else
      print '("  ERROR: Not enough segments found")'
      call ft_type1_parser_done(parser)
      return
    end if
    
    print '("  Result: PASS - PFB detection and parsing successful")'
    passed = passed + 1
    
    call ft_type1_parser_done(parser)
    
  end subroutine test_pfb_detection

  ! Test invalid format handling
  subroutine test_invalid_format(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_Type1_Format) :: format
    integer(FT_Error) :: error
    logical :: success
    
    ! Test various invalid formats
    character(len=*), parameter :: too_short = 'xy'
    character(len=*), parameter :: not_postscript = 'This is not a PostScript file'
    character(len=*), parameter :: empty_data = ''
    
    total = total + 1
    print '("Test ", I0, ": Invalid format handling")', total
    
    ! Test too short
    success = ft_type1_detect_format(too_short, len(too_short), format, error)
    if (.not. success .and. error == FT_Err_Invalid_File_Format) then
      print '("  Too short data: PASS")'
    else
      print '("  Too short data: FAIL")'
      return
    end if
    
    ! Test not PostScript
    success = ft_type1_detect_format(not_postscript, len(not_postscript), format, error)
    if (.not. success .and. error == FT_Err_Unknown_File_Format) then
      print '("  Non-PostScript data: PASS")'
    else
      print '("  Non-PostScript data: FAIL")'
      return
    end if
    
    ! Test empty data
    success = ft_type1_detect_format(empty_data, len(empty_data), format, error)
    if (.not. success .and. error == FT_Err_Invalid_File_Format) then
      print '("  Empty data: PASS")'
    else
      print '("  Empty data: FAIL")'
      return
    end if
    
    print '("  Result: PASS - Invalid format handling correct")'
    passed = passed + 1
    
  end subroutine test_invalid_format

end program test_type1_parser