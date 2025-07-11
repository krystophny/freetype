program test_cff_parser
  use ft_types
  use ft_cff
  implicit none
  
  integer :: total_tests = 0
  integer :: passed_tests = 0
  
  print '("CFF Parser Tests")'
  print '("================")'
  print '()'
  
  ! Test 1: CFF format detection
  call test_cff_format_detection(passed_tests, total_tests)
  
  ! Test 2: CFF header parsing
  call test_cff_header_parsing(passed_tests, total_tests)
  
  ! Test 3: CFF INDEX parsing
  call test_cff_index_parsing(passed_tests, total_tests)
  
  ! Test 4: CFF DICT parsing
  call test_cff_dict_parsing(passed_tests, total_tests)
  
  ! Test 5: Invalid format handling
  call test_invalid_cff_format(passed_tests, total_tests)
  
  ! Summary
  print '()'
  print '("===== TEST SUMMARY =====")'
  print '("Total tests: ", I0)', total_tests
  print '("Passed tests: ", I0)', passed_tests
  print '("Failed tests: ", I0)', total_tests - passed_tests
  print '("Success rate: ", F5.1, "%")', real(passed_tests) / real(total_tests) * 100.0
  
  if (passed_tests == total_tests) then
    print '("✓ All CFF parser tests PASSED!")'
  else
    print '("✗ Some CFF parser tests FAILED!")'
  end if
  
contains

  ! Test CFF format detection
  subroutine test_cff_format_detection(passed, total)
    integer, intent(inout) :: passed, total
    
    logical :: is_cff, is_cff2
    
    ! Test data for CFF1 format
    character(len=1), parameter :: cff1_data(8) = [ &
      char(1), char(0), char(4), char(1), &  ! major=1, minor=0, hdr_size=4, offset_size=1
      char(0), char(0), char(0), char(0)  &  ! Additional data
    ]
    
    ! Test data for CFF2 format
    character(len=1), parameter :: cff2_data(8) = [ &
      char(2), char(0), char(5), char(2), &  ! major=2, minor=0, hdr_size=5, offset_size=2
      char(0), char(0), char(0), char(0)  &  ! Additional data
    ]
    
    ! Test data for invalid format
    character(len=1), parameter :: invalid_data(8) = [ &
      char(3), char(0), char(4), char(1), &  ! major=3 (invalid)
      char(0), char(0), char(0), char(0)  &  ! Additional data
    ]
    
    total = total + 1
    print '("Test ", I0, ": CFF format detection")', total
    
    ! Test CFF1 detection
    is_cff = ft_cff_detect_format(cff1_data, size(cff1_data), is_cff2)
    if (is_cff .and. .not. is_cff2) then
      print '("  CFF1 detection: PASS")'
    else
      print '("  CFF1 detection: FAIL (is_cff=", L1, ", is_cff2=", L1, ")")', is_cff, is_cff2
      return
    end if
    
    ! Test CFF2 detection
    is_cff = ft_cff_detect_format(cff2_data, size(cff2_data), is_cff2)
    if (is_cff .and. is_cff2) then
      print '("  CFF2 detection: PASS")'
    else
      print '("  CFF2 detection: FAIL (is_cff=", L1, ", is_cff2=", L1, ")")', is_cff, is_cff2
      return
    end if
    
    ! Test invalid format
    is_cff = ft_cff_detect_format(invalid_data, size(invalid_data), is_cff2)
    if (.not. is_cff) then
      print '("  Invalid format detection: PASS")'
    else
      print '("  Invalid format detection: FAIL")'
      return
    end if
    
    print '("  Result: PASS - All format detection tests passed")'
    passed = passed + 1
    
  end subroutine test_cff_format_detection

  ! Test CFF header parsing
  subroutine test_cff_header_parsing(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_CFF_Parser) :: parser
    integer(FT_Error) :: error
    logical :: success
    
    ! Test data with valid CFF header
    character(len=1), parameter :: cff_data(8) = [ &
      char(1), char(0), char(4), char(2), &  ! major=1, minor=0, hdr_size=4, offset_size=2
      char(0), char(0), char(0), char(0)  &  ! Additional data
    ]
    
    total = total + 1
    print '("Test ", I0, ": CFF header parsing")', total
    
    ! Create parser
    success = ft_cff_parser_new(cff_data, size(cff_data), parser, error)
    if (.not. success) then
      print '("  ERROR: Could not create parser, error=", I0)', error
      return
    end if
    
    ! Check header fields
    print '("  Major version: ", I0)', int(parser%header%major_version)
    print '("  Minor version: ", I0)', int(parser%header%minor_version)
    print '("  Header size: ", I0)', int(parser%header%header_size)
    print '("  Offset size: ", I0)', int(parser%header%offset_size)
    print '("  Is CFF2: ", L1)', parser%is_cff2
    
    ! Verify header values
    if (parser%header%major_version == 1 .and. &
        parser%header%minor_version == 0 .and. &
        parser%header%header_size == 4 .and. &
        parser%header%offset_size == 2 .and. &
        .not. parser%is_cff2) then
      print '("  Result: PASS - Header parsing successful")'
      passed = passed + 1
    else
      print '("  Result: FAIL - Header parsing incorrect")'
    end if
    
    call ft_cff_parser_done(parser)
    
  end subroutine test_cff_header_parsing

  ! Test CFF INDEX parsing
  subroutine test_cff_index_parsing(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_CFF_Parser) :: parser
    type(FT_CFF_INDEX) :: index
    integer(FT_Error) :: error
    logical :: success
    
    ! Test data with CFF header + INDEX
    ! INDEX: count=2, offset_size=1, offsets=[1,4,7], data="ABC" + "XYZ"
    character(len=1), parameter :: cff_data(16) = [ &
      char(1), char(0), char(4), char(1), &    ! CFF header (4 bytes)
      char(0), char(2), &                      ! INDEX count = 2 (2 bytes)
      char(1), &                               ! offset_size = 1 (1 byte)
      char(1), char(4), char(7), &             ! offsets: 1, 4, 7 (3 bytes)
      char(65), char(66), char(67), &          ! data: "ABC" (3 bytes)
      char(88), char(89), char(90) &           ! data: "XYZ" (3 bytes)
    ]
    
    total = total + 1
    print '("Test ", I0, ": CFF INDEX parsing")', total
    
    ! Create parser
    success = ft_cff_parser_new(cff_data, size(cff_data), parser, error)
    if (.not. success) then
      print '("  ERROR: Could not create parser, error=", I0)', error
      return
    end if
    
    ! Set position to INDEX start (after header)
    parser%position = 5
    
    ! Parse INDEX
    success = ft_cff_parse_index(parser, index, error)
    if (.not. success) then
      print '("  ERROR: Could not parse INDEX, error=", I0)', error
      call ft_cff_parser_done(parser)
      return
    end if
    
    ! Check INDEX structure
    print '("  INDEX count: ", I0)', index%count
    print '("  INDEX offset_size: ", I0)', int(index%offset_size)
    print '("  INDEX data_size: ", I0)', index%data_size
    
    if (allocated(index%offsets)) then
      print '("  Offsets: ", 10I0)', index%offsets(1:min(size(index%offsets), 10))
    end if
    
    ! Verify INDEX values
    if (index%count == 2 .and. &
        index%offset_size == 1 .and. &
        index%data_size == 6 .and. &
        allocated(index%offsets) .and. &
        size(index%offsets) == 3 .and. &
        index%offsets(1) == 1 .and. &
        index%offsets(2) == 4 .and. &
        index%offsets(3) == 7) then
      print '("  Result: PASS - INDEX parsing successful")'
      passed = passed + 1
    else
      print '("  Result: FAIL - INDEX parsing incorrect")'
    end if
    
    call ft_cff_parser_done(parser)
    
  end subroutine test_cff_index_parsing

  ! Test CFF DICT parsing
  subroutine test_cff_dict_parsing(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_CFF_DICT) :: dict
    integer(FT_Error) :: error
    logical :: success
    
    ! Test DICT data: "100 0" (operands) + operator 5 (FontBBox)
    character(len=1), parameter :: dict_data(4) = [ &
      char(139), &      ! operand: 100 - 139 = -39 -> 100 (since 139 + 100 = 239, but 139 is base)
      char(139), &      ! operand: 0 (139 - 139 = 0)
      char(5), &        ! operator: FontBBox
      char(0) &         ! padding
    ]
    
    total = total + 1
    print '("Test ", I0, ": CFF DICT parsing")', total
    
    ! Parse DICT
    success = ft_cff_parse_dict(dict_data, size(dict_data), dict, error)
    if (.not. success) then
      print '("  ERROR: Could not parse DICT, error=", I0)', error
      return
    end if
    
    ! Check DICT structure
    print '("  DICT entries: ", I0)', dict%num_entries
    
    if (dict%num_entries > 0 .and. allocated(dict%entries)) then
      print '("  Entry 1 operator: ", I0)', dict%entries(1)%operator
      print '("  Entry 1 operands: ", I0)', dict%entries(1)%num_operands
      if (dict%entries(1)%num_operands > 0 .and. allocated(dict%entries(1)%operands)) then
        print '("    Operand values: ", 10F8.1)', dict%entries(1)%operands(1:min(dict%entries(1)%num_operands, 10))
      end if
    end if
    
    ! Verify DICT parsing (basic structure check)
    if (dict%num_entries >= 1 .and. &
        allocated(dict%entries) .and. &
        dict%entries(1)%operator == 5 .and. &
        dict%entries(1)%num_operands == 2) then
      print '("  Result: PASS - DICT parsing successful")'
      passed = passed + 1
    else
      print '("  Result: FAIL - DICT parsing incorrect")'
    end if
    
    ! Clean up
    call cleanup_dict_local(dict)
    
  end subroutine test_cff_dict_parsing

  ! Test invalid CFF format handling
  subroutine test_invalid_cff_format(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_CFF_Parser) :: parser
    integer(FT_Error) :: error
    logical :: success
    
    ! Test various invalid formats
    character(len=1), parameter :: too_short(2) = [char(1), char(0)]
    character(len=1), parameter :: invalid_version(4) = [char(5), char(0), char(4), char(1)]
    character(len=1), parameter :: invalid_offset_size(4) = [char(1), char(0), char(4), char(0)]
    
    total = total + 1
    print '("Test ", I0, ": Invalid CFF format handling")', total
    
    ! Test too short data
    success = ft_cff_parser_new(too_short, size(too_short), parser, error)
    if (.not. success .and. error == FT_Err_Invalid_File_Format) then
      print '("  Too short data: PASS")'
    else
      print '("  Too short data: FAIL (success=", L1, ", error=", I0, ")")', success, error
      return
    end if
    
    ! Test invalid version
    success = ft_cff_parser_new(invalid_version, size(invalid_version), parser, error)
    if (.not. success .and. error == FT_Err_Invalid_File_Format) then
      print '("  Invalid version: PASS")'
    else
      print '("  Invalid version: FAIL (success=", L1, ", error=", I0, ")")', success, error
      if (success) call ft_cff_parser_done(parser)
      return
    end if
    
    ! Test invalid offset size
    success = ft_cff_parser_new(invalid_offset_size, size(invalid_offset_size), parser, error)
    if (.not. success .and. error == FT_Err_Invalid_File_Format) then
      print '("  Invalid offset size: PASS")'
    else
      print '("  Invalid offset size: FAIL (success=", L1, ", error=", I0, ")")', success, error
      if (success) call ft_cff_parser_done(parser)
      return
    end if
    
    print '("  Result: PASS - Invalid format handling correct")'
    passed = passed + 1
    
  end subroutine test_invalid_cff_format

  ! Helper: Clean up DICT structure
  subroutine cleanup_dict_local(dict)
    type(FT_CFF_DICT), intent(inout) :: dict
    integer :: i
    
    if (allocated(dict%entries)) then
      do i = 1, dict%num_entries
        if (allocated(dict%entries(i)%operands)) then
          deallocate(dict%entries(i)%operands)
        end if
      end do
      deallocate(dict%entries)
    end if
    dict%num_entries = 0
  end subroutine cleanup_dict_local

end program test_cff_parser