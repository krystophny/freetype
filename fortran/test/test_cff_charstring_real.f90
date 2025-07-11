program test_cff_charstring_real
  use ft_types
  use ft_face_unified
  use ft_font_format
  use ft_outline_mod
  implicit none
  
  integer :: total_tests = 0
  integer :: passed_tests = 0
  
  print '("CFF Real CharString Tests")'
  print '("=========================")'
  print '()'
  
  ! Test 1: Test CFF with real CharStrings
  call test_cff_with_charstrings(passed_tests, total_tests)
  
  ! Summary
  print '()'
  print '("===== TEST SUMMARY =====")'
  print '("Total tests: ", I0)', total_tests
  print '("Passed tests: ", I0)', passed_tests
  print '("Failed tests: ", I0)', total_tests - passed_tests
  print '("Success rate: ", F5.1, "%")', real(passed_tests) / real(total_tests) * 100.0
  
  if (passed_tests == total_tests) then
    print '("✓ All CFF real CharString tests PASSED!")'
  else
    print '("✗ Some CFF real CharString tests FAILED!")'
  end if
  
contains

  ! Test CFF with CharStrings INDEX
  subroutine test_cff_with_charstrings(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_Unified_Face) :: face
    type(FT_Outline) :: outline
    integer(FT_Error) :: error
    logical :: success
    character(len=256) :: temp_file
    integer :: unit, iostat
    integer :: glyph_index
    
    ! More complete CFF data with CharStrings
    ! This includes a minimal CharStrings INDEX with 2 glyphs
    character(len=1), parameter :: cff_data(*) = [ &
      ! Header (4 bytes)
      char(1), char(0), char(4), char(1), &    ! major=1, minor=0, hdr_size=4, offset_size=1
      
      ! Name INDEX (min 4 bytes for empty + 8 for one name)
      char(0), char(1), &                      ! count=1
      char(1), &                               ! offset_size=1
      char(1), char(5), &                      ! offsets: 1, 5 (data is 4 bytes)
      char(84), char(101), char(115), char(116), & ! "Test"
      
      ! Top DICT INDEX
      char(0), char(1), &                      ! count=1
      char(1), &                               ! offset_size=1
      char(1), char(5), &                      ! offsets: 1, 5 (4 bytes of data)
      ! Top DICT data: CharStrings offset operator
      ! To encode offset 100: b0 = 100 + 139 = 239
      char(239), char(17), &  ! operand=100, operator=17 (CharStrings)
      char(0), char(0), &     ! padding
      
      ! String INDEX (4 bytes for empty)
      char(0), char(0), &                      ! count=0
      char(0), char(0), &                      ! padding
      
      ! Global Subr INDEX (4 bytes for empty)
      char(0), char(0), &                      ! count=0
      char(0), char(0), &                      ! padding
      
      ! Padding to reach offset 100 (CFF offsets are 1-based)
      ! Current position: 4 + 11 + 12 + 4 + 4 = 35
      ! Need to reach 100, so need 100 - 35 = 65 bytes of padding
      char(0), char(0), char(0), char(0), char(0), char(0), char(0), char(0), &
      char(0), char(0), char(0), char(0), char(0), char(0), char(0), char(0), &
      char(0), char(0), char(0), char(0), char(0), char(0), char(0), char(0), &
      char(0), char(0), char(0), char(0), char(0), char(0), char(0), char(0), &
      char(0), char(0), char(0), char(0), char(0), char(0), char(0), char(0), &
      char(0), char(0), char(0), char(0), char(0), char(0), char(0), char(0), &
      char(0), char(0), char(0), char(0), char(0), char(0), char(0), char(0), &
      char(0), char(0), char(0), char(0), char(0), char(0), char(0), char(0), &
      char(0), &  ! 65 bytes total
      
      ! CharStrings INDEX at offset 100
      char(0), char(2), &                      ! count=2 (2 glyphs)
      char(1), &                               ! offset_size=1
      char(1), char(2), char(10), &           ! offsets: 1, 2, 10 (glyph 0: 1 byte, glyph 1: 8 bytes)
      ! Glyph 0 CharString (1 byte - endchar)
      char(14), &                              ! 14 = endchar
      ! Glyph 1 CharString (8 bytes - simple square)
      char(100), char(100), char(21), &        ! 100 100 rmoveto
      char(144), char(1), char(22), &          ! 400 0 rlineto (400 = 256 + 144)
      char(14) &                               ! endchar
    ]
    
    total = total + 1
    print '("Test ", I0, ": CFF with real CharStrings")', total
    
    ! Create temporary file
    temp_file = "/tmp/test_cff_real_charstring.cff"
    open(newunit=unit, file=temp_file, form='unformatted', access='stream', &
         status='replace', iostat=iostat)
    if (iostat /= 0) then
      print '("  ERROR: Could not create temp file")'
      return
    end if
    
    ! Write CFF data
    write(unit) cff_data
    close(unit)
    
    ! Load through unified face
    success = ft_new_unified_face(temp_file, 0, face, error)
    if (.not. success) then
      print '("  ERROR: Could not load unified face, error=", I0)', error
      call execute_command_line("rm -f " // trim(temp_file))
      return
    end if
    
    print '("  Face loaded successfully")'
    print '("  Font format: ", I0)', face%font_format
    print '("  Num glyphs: ", I0)', face%num_glyphs
    
    ! Test loading glyph 0 (.notdef)
    glyph_index = 0
    success = ft_unified_load_glyph(face, glyph_index, outline, error)
    if (.not. success) then
      print '("  ERROR: Could not load glyph 0, error=", I0)', error
    else
      print '("  Glyph 0 loaded: ", I0, " points, ", I0, " contours")', &
            outline%n_points, outline%n_contours
      if (outline%n_points == 0) then
        print '("  Result: PASS - .notdef glyph parsed correctly")'
        passed = passed + 1
      else
        print '("  Result: FAIL - .notdef should be empty")'
      end if
    end if
    
    ! Clean up
    call ft_done_unified_face(face)
    call execute_command_line("rm -f " // trim(temp_file))
    
  end subroutine test_cff_with_charstrings

end program test_cff_charstring_real