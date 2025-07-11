program test_cff_face
  use ft_types
  use ft_cff
  use ft_cff_face
  implicit none
  
  integer :: total_tests = 0
  integer :: passed_tests = 0
  
  print '("CFF Face Loading Tests")'
  print '("=====================")'
  print '()'
  
  ! Test 1: CFF face initialization
  call test_cff_face_init(passed_tests, total_tests)
  
  ! Test 2: CFF font loading
  call test_cff_font_loading(passed_tests, total_tests)
  
  ! Test 3: Font information extraction
  call test_font_info_extraction(passed_tests, total_tests)
  
  ! Test 4: Error handling
  call test_error_handling(passed_tests, total_tests)
  
  ! Summary
  print '()'
  print '("===== TEST SUMMARY =====")'
  print '("Total tests: ", I0)', total_tests
  print '("Passed tests: ", I0)', passed_tests
  print '("Failed tests: ", I0)', total_tests - passed_tests
  print '("Success rate: ", F5.1, "%")', real(passed_tests) / real(total_tests) * 100.0
  
  if (passed_tests == total_tests) then
    print '("✓ All CFF face tests PASSED!")'
  else
    print '("✗ Some CFF face tests FAILED!")'
  end if
  
contains

  ! Test CFF face initialization
  subroutine test_cff_face_init(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_CFF_Face_Type) :: face
    integer(FT_Error) :: error
    logical :: success
    
    ! Basic CFF data with minimal structure
    character(len=1), parameter :: cff_data(*) = [ &
      char(1), char(0), char(4), char(1), &    ! CFF header (4 bytes)
      char(0), char(1), &                      ! Name INDEX: count=1 (2 bytes)
      char(1), &                               ! offset_size=1 (1 byte)
      char(1), char(9), &                      ! offsets: 1, 9 (2 bytes)
      char(84), char(101), char(115), char(116), char(70), char(111), char(110), char(116) & ! "TestFont" (8 bytes)
    ]
    
    total = total + 1
    print '("Test ", I0, ": CFF face initialization")', total
    
    ! Initialize face
    success = ft_cff_face_init(face, cff_data, size(cff_data), error)
    if (.not. success) then
      print '("  ERROR: Could not initialize CFF face, error=", I0)', error
      return
    end if
    
    ! Check basic properties
    if (face%is_loaded .and. &
        face%units_per_em == 1000 .and. &
        face%num_glyphs == 0) then
      print '("  Result: PASS - CFF face initialization successful")'
      passed = passed + 1
    else
      print '("  Result: FAIL - CFF face initialization incorrect")'
    end if
    
    ! Clean up
    call ft_cff_face_done(face)
    
  end subroutine test_cff_face_init

  ! Test CFF font loading
  subroutine test_cff_font_loading(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_CFF_Face_Type) :: face
    integer(FT_Error) :: error
    logical :: success
    
    ! Minimal CFF data that works (from test_cff_minimal)
    character(len=1), parameter :: cff_data(*) = [ &
      char(1), char(0), char(4), char(1), &    ! Header: major=1, minor=0, hdrSize=4, offSize=1 (4 bytes)
      ! Name INDEX
      char(0), char(1), &                      ! count=1 (2 bytes)
      char(1), &                               ! offset_size=1 (1 byte)
      char(1), char(5), &                      ! offsets: 1, 5 (2 bytes) 
      char(84), char(101), char(115), char(116), & ! "Test" (4 bytes)
      ! Top DICT INDEX
      char(0), char(1), &                      ! count=1 (2 bytes)
      char(1), &                               ! offset_size=1 (1 byte)
      char(1), char(2), &                      ! offsets: 1, 2 (2 bytes)
      char(0), &                               ! Empty DICT (1 byte)
      ! String INDEX (empty)
      char(0), char(0), &                      ! count=0 (2 bytes)
      ! Global Subr INDEX (empty)
      char(0), char(0), &                       ! count=0 (2 bytes)
      ! Padding to match test_cff_minimal size
      char(0), char(0), char(0), char(0), char(0), char(0), char(0) &  ! padding (7 bytes)
    ]
    
    total = total + 1
    print '("Test ", I0, ": CFF font loading")', total
    
    ! Initialize face
    success = ft_cff_face_init(face, cff_data, size(cff_data), error)
    if (.not. success) then
      print '("  ERROR: Could not initialize CFF face, error=", I0)', error
      return
    end if
    
    ! Load font
    success = ft_cff_load_font(face, error)
    if (.not. success) then
      print '("  ERROR: Could not load CFF font, error=", I0)', error
      call ft_cff_face_done(face)
      return
    end if
    
    ! Check that indices were loaded
    if (face%cff_parser%name_index%count == 1 .and. &
        face%cff_parser%top_dict_index%count == 1) then
      print '("  Name INDEX count: ", I0)', face%cff_parser%name_index%count
      print '("  Top DICT INDEX count: ", I0)', face%cff_parser%top_dict_index%count
      print '("  Face flags: ", I0)', face%face_flags
      print '("  Result: PASS - CFF font loading successful")'
      passed = passed + 1
    else
      print '("  Result: FAIL - CFF font loading incorrect")'
    end if
    
    ! Clean up
    call ft_cff_face_done(face)
    
  end subroutine test_cff_font_loading

  ! Test font information extraction
  subroutine test_font_info_extraction(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_CFF_Face_Type) :: face
    integer(FT_Error) :: error
    logical :: success
    
    ! Minimal CFF data
    character(len=1), parameter :: cff_data(*) = [ &
      char(1), char(0), char(4), char(1), &    ! Header (4 bytes)
      ! Name INDEX
      char(0), char(1), &                      ! count=1 (2 bytes)
      char(1), &                               ! offset_size=1 (1 byte)
      char(1), char(9), &                      ! offsets: 1, 9 (2 bytes)
      char(84), char(101), char(115), char(116), char(70), char(111), char(110), char(116), & ! "TestFont" (8 bytes)
      ! Top DICT INDEX
      char(0), char(1), &                      ! count=1 (2 bytes)
      char(1), &                               ! offset_size=1 (1 byte)
      char(1), char(2), &                      ! offsets: 1, 2 (2 bytes)
      char(0), &                               ! Empty DICT (1 byte)
      ! String INDEX (empty)
      char(0), char(0), &                      ! count=0 (2 bytes)
      ! Global Subr INDEX (empty)
      char(0), char(0), &                       ! count=0 (2 bytes)
      ! Padding
      char(0), char(0), char(0), char(0) &      ! padding (4 bytes)
    ]
    
    total = total + 1
    print '("Test ", I0, ": Font information extraction")', total
    
    ! Initialize and load face
    success = ft_cff_face_init(face, cff_data, size(cff_data), error)
    if (.not. success) then
      print '("  ERROR: Could not initialize CFF face, error=", I0)', error
      return
    end if
    
    success = ft_cff_load_font(face, error)
    if (.not. success) then
      print '("  ERROR: Could not load CFF font, error=", I0)', error
      call ft_cff_face_done(face)
      return
    end if
    
    ! Extract font information
    success = ft_cff_get_font_info(face, error)
    if (.not. success) then
      print '("  ERROR: Could not extract font info, error=", I0)', error
      call ft_cff_face_done(face)
      return
    end if
    
    ! Check extracted information
    print '("  Family name: [", A, "]")', trim(face%family_name)
    print '("  Style name: [", A, "]")', trim(face%style_name)
    print '("  Full name: [", A, "]")', trim(face%full_name)
    print '("  Units per EM: ", I0)', face%units_per_em
    
    ! Verify basic information was set
    if (len_trim(face%family_name) > 0 .and. &
        len_trim(face%style_name) > 0 .and. &
        face%units_per_em > 0) then
      print '("  Result: PASS - Font information extraction successful")'
      passed = passed + 1
    else
      print '("  Result: FAIL - Font information extraction incomplete")'
    end if
    
    ! Clean up
    call ft_cff_face_done(face)
    
  end subroutine test_font_info_extraction

  ! Test error handling
  subroutine test_error_handling(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_CFF_Face_Type) :: face
    integer(FT_Error) :: error
    logical :: success
    
    ! Invalid CFF data
    character(len=1), parameter :: invalid_data(4) = [ &
      char(5), char(0), char(4), char(1) &  ! Invalid major version
    ]
    
    total = total + 1
    print '("Test ", I0, ": Error handling")', total
    
    ! Try to initialize with invalid data
    success = ft_cff_face_init(face, invalid_data, size(invalid_data), error)
    if (.not. success .and. error == FT_Err_Invalid_File_Format) then
      print '("  Invalid data handling: PASS")'
    else
      print '("  Invalid data handling: FAIL (success=", L1, ", error=", I0, ")")', success, error
      if (success) call ft_cff_face_done(face)
      return
    end if
    
    ! Try to load font on uninitialized face
    face%is_loaded = .false.
    success = ft_cff_load_font(face, error)
    if (.not. success .and. error == FT_Err_Invalid_Face_Handle) then
      print '("  Uninitialized face handling: PASS")'
    else
      print '("  Uninitialized face handling: FAIL (success=", L1, ", error=", I0, ")")', success, error
      return
    end if
    
    print '("  Result: PASS - Error handling correct")'
    passed = passed + 1
    
  end subroutine test_error_handling

end program test_cff_face