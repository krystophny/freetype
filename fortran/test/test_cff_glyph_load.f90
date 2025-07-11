program test_cff_glyph_load
  use ft_types
  use ft_face_unified
  use ft_font_format
  use ft_outline_mod
  implicit none
  
  integer :: total_tests = 0
  integer :: passed_tests = 0
  
  print '("CFF Glyph Loading Tests")'
  print '("=======================")'
  print '()'
  
  ! Test 1: Test CFF glyph loading
  call test_cff_glyph_outline(passed_tests, total_tests)
  
  ! Summary
  print '()'
  print '("===== TEST SUMMARY =====")'
  print '("Total tests: ", I0)', total_tests
  print '("Passed tests: ", I0)', passed_tests
  print '("Failed tests: ", I0)', total_tests - passed_tests
  print '("Success rate: ", F5.1, "%")', real(passed_tests) / real(total_tests) * 100.0
  
  if (passed_tests == total_tests) then
    print '("✓ All CFF glyph loading tests PASSED!")'
  else
    print '("✗ Some CFF glyph loading tests FAILED!")'
  end if
  
contains

  ! Test CFF glyph outline loading
  subroutine test_cff_glyph_outline(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_Unified_Face) :: face
    type(FT_Outline) :: outline
    integer(FT_Error) :: error
    logical :: success
    character(len=256) :: temp_file
    integer :: unit, iostat
    integer :: glyph_index
    
    ! Minimal CFF data (same as before)
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
    print '("Test ", I0, ": CFF glyph outline loading")', total
    
    ! Create temporary file
    temp_file = "/tmp/test_cff_glyph.cff"
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
        print '("  Result: PASS - .notdef glyph is empty as expected")'
        passed = passed + 1
      else
        print '("  Result: FAIL - .notdef should be empty")'
      end if
    end if
    
    ! Test loading glyph 1
    total = total + 1
    print '()'
    print '("Test ", I0, ": CFF glyph 1 loading")', total
    
    glyph_index = 1
    success = ft_unified_load_glyph(face, glyph_index, outline, error)
    if (.not. success) then
      print '("  ERROR: Could not load glyph 1, error=", I0)', error
    else
      print '("  Glyph 1 loaded: ", I0, " points, ", I0, " contours")', &
            outline%n_points, outline%n_contours
      if (outline%n_points == 4 .and. outline%n_contours == 1) then
        print '("  Point 0: (", I0, ",", I0, ")")', outline%points(1)%x, outline%points(1)%y
        print '("  Point 1: (", I0, ",", I0, ")")', outline%points(2)%x, outline%points(2)%y
        print '("  Result: PASS - Placeholder glyph created")'
        passed = passed + 1
      else
        print '("  Result: FAIL - Expected 4 points, 1 contour")'
      end if
    end if
    
    ! Clean up
    call ft_done_unified_face(face)
    call execute_command_line("rm -f " // trim(temp_file))
    
  end subroutine test_cff_glyph_outline

end program test_cff_glyph_load