program test_unified_face
  use ft_types
  use ft_face_unified
  use ft_font_format
  implicit none
  
  integer :: total_tests = 0
  integer :: passed_tests = 0
  
  print '("Unified Face Tests")'
  print '("==================")'
  print '()'
  
  ! Test 1: Test with minimal CFF data
  call test_cff_unified_face(passed_tests, total_tests)
  
  ! Summary
  print '()'
  print '("===== TEST SUMMARY =====")'
  print '("Total tests: ", I0)', total_tests
  print '("Passed tests: ", I0)', passed_tests
  print '("Failed tests: ", I0)', total_tests - passed_tests
  print '("Success rate: ", F5.1, "%")', real(passed_tests) / real(total_tests) * 100.0
  
  if (passed_tests == total_tests) then
    print '("✓ All unified face tests PASSED!")'
  else
    print '("✗ Some unified face tests FAILED!")'
  end if
  
contains

  ! Test CFF loading through unified face
  subroutine test_cff_unified_face(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_Unified_Face) :: face
    integer(FT_Error) :: error
    logical :: success
    character(len=256) :: temp_file
    integer :: unit, iostat
    
    ! Minimal CFF data (same as test_cff_minimal)
    character(len=1), parameter :: cff_data(*) = [ &
      ! Header (4 bytes)
      char(1), char(0), char(4), char(1), &    ! major=1, minor=0, hdr_size=4, offset_size=1
      
      ! Name INDEX (min 4 bytes for empty + 8 for one name)
      char(0), char(1), &                      ! count=1
      char(1), &                               ! offset_size=1
      char(1), char(5), &                      ! offsets: 1, 5 (data is 4 bytes)
      char(84), char(101), char(115), char(116), & ! "Test"
      
      ! Top DICT INDEX (min 4 bytes for empty + content)
      char(0), char(1), &                      ! count=1
      char(1), &                               ! offset_size=1
      char(1), char(2), &                      ! offsets: 1, 2 (minimal dict)
      char(0), &                               ! empty dict
      
      ! String INDEX (4 bytes for empty)
      char(0), char(0), &                      ! count=0
      
      ! Global Subr INDEX (4 bytes for empty)
      char(0), char(0), &                      ! count=0
      
      ! Padding to ensure we don't run out of data
      char(0), char(0), char(0), char(0), &
      char(0), char(0), char(0), char(0) &
    ]
    
    total = total + 1
    print '("Test ", I0, ": CFF unified face loading")', total
    
    ! Create temporary file
    temp_file = "/tmp/test_cff_unified.cff"
    print *, "Creating temp file: ", trim(temp_file)
    open(newunit=unit, file=temp_file, form='unformatted', access='stream', &
         status='replace', iostat=iostat)
    if (iostat /= 0) then
      print '("  ERROR: Could not create temp file")'
      return
    end if
    
    ! Write CFF data
    print *, "Writing ", size(cff_data), " bytes of CFF data"
    write(unit, iostat=iostat) cff_data
    if (iostat /= 0) then
      print '("  ERROR: Could not write to temp file, iostat=", I0)', iostat
      close(unit)
      return
    end if
    close(unit)
    print *, "File written and closed"
    
    ! Check file exists
    inquire(file=temp_file, exist=success)
    if (.not. success) then
      print '("  ERROR: File does not exist after writing")'
      return
    end if
    
    ! Load through unified face
    success = ft_new_unified_face(temp_file, 0, face, error)
    if (.not. success) then
      print '("  ERROR: Could not load unified face, error=", I0)', error
      call execute_command_line("rm -f " // trim(temp_file))
      return
    end if
    
    ! Check properties
    print '("  Font format: ", A)', trim(ft_get_format_name(face%font_format))
    print '("  Family name: ", A)', trim(face%family_name)
    print '("  Units per EM: ", I0)', face%units_per_em
    print '("  Face flags: ", I0)', face%face_flags
    
    ! Verify it loaded as CFF
    if (face%font_format == FT_FONT_FORMAT_CFF .and. &
        allocated(face%cff_face) .and. &
        iand(face%face_flags, FT_FACE_FLAG_SCALABLE) /= 0) then
      print '("  Result: PASS - CFF face loaded through unified interface")'
      passed = passed + 1
    else
      print '("  Result: FAIL - CFF face not loaded correctly")'
    end if
    
    ! Clean up
    call ft_done_unified_face(face)
    call execute_command_line("rm -f " // trim(temp_file))
    
  end subroutine test_cff_unified_face

end program test_unified_face