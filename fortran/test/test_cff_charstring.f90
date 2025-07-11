program test_cff_charstring
  use ft_types
  use ft_outline_mod, only: FT_Outline, ft_outline_done
  use ft_cff_charstring
  implicit none
  
  integer :: total_tests = 0
  integer :: passed_tests = 0
  
  print '("CFF CharString Tests")'
  print '("====================")'
  print '()'
  
  ! Test 1: Context initialization
  call test_context_init(passed_tests, total_tests)
  
  ! Test 2: Simple path (moveto + lineto)
  call test_simple_path(passed_tests, total_tests)
  
  ! Test 3: Curve path
  call test_curve_path(passed_tests, total_tests)
  
  ! Test 4: Complete glyph
  call test_complete_glyph(passed_tests, total_tests)
  
  ! Summary
  print '()'
  print '("===== TEST SUMMARY =====")'
  print '("Total tests: ", I0)', total_tests
  print '("Passed tests: ", I0)', passed_tests
  print '("Failed tests: ", I0)', total_tests - passed_tests
  print '("Success rate: ", F5.1, "%")', real(passed_tests) / real(total_tests) * 100.0
  
  if (passed_tests == total_tests) then
    print '("✓ All CFF CharString tests PASSED!")'
  else
    print '("✗ Some CFF CharString tests FAILED!")'
  end if
  
contains

  ! Test context initialization
  subroutine test_context_init(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_CFF_CharString_Context) :: context
    integer(FT_Error) :: error
    logical :: success
    
    total = total + 1
    print '("Test ", I0, ": Context initialization")', total
    
    success = ft_cff_charstring_init(context, error)
    if (.not. success) then
      print '("  ERROR: Could not initialize context, error=", I0)', error
      return
    end if
    
    ! Check initial state
    if (allocated(context%stack) .and. &
        context%stack_top == 0 .and. &
        context%x == 0.0 .and. &
        context%y == 0.0 .and. &
        .not. context%path_open) then
      print '("  Result: PASS - Context initialized correctly")'
      passed = passed + 1
    else
      print '("  Result: FAIL - Context state incorrect")'
    end if
    
    call ft_cff_charstring_done(context)
    
  end subroutine test_context_init

  ! Test simple path construction
  subroutine test_simple_path(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_Outline) :: outline
    integer(FT_Error) :: error
    logical :: success
    
    ! Simple CharString: 100 100 rmoveto 50 0 rlineto 0 50 rlineto endchar
    ! Operands: 100=239, 50=189, 0=139
    character(len=1), parameter :: charstring(*) = [ &
      char(239), char(239), char(21), &  ! 100 100 rmoveto
      char(189), char(139), char(5), &   ! 50 0 rlineto
      char(139), char(189), char(5), &   ! 0 50 rlineto
      char(14) &                          ! endchar
    ]
    
    total = total + 1
    print '("Test ", I0, ": Simple path construction")', total
    
    success = ft_cff_charstring_to_outline(charstring, size(charstring), outline, error)
    if (.not. success) then
      print '("  ERROR: Could not parse CharString, error=", I0)', error
      return
    end if
    
    ! Check outline
    print '("  Outline points: ", I0)', outline%n_points
    print '("  Outline contours: ", I0)', outline%n_contours
    
    if (outline%n_points >= 2 .and. outline%n_contours >= 1) then
      print '("  Result: PASS - Simple path created")'
      passed = passed + 1
    else
      print '("  Result: FAIL - Path not created correctly")'
    end if
    
    call ft_outline_done(outline)
    
  end subroutine test_simple_path

  ! Test curve path construction
  subroutine test_curve_path(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_Outline) :: outline
    integer(FT_Error) :: error
    logical :: success
    
    ! CharString with curve: moveto + rrcurveto + endchar
    ! 100 100 rmoveto 30 0 30 30 0 30 rrcurveto endchar
    character(len=1), parameter :: charstring(*) = [ &
      char(239), char(239), char(21), &  ! 100 100 rmoveto
      char(169), char(139), char(169), char(169), char(139), char(169), char(8), & ! 30 0 30 30 0 30 rrcurveto
      char(14) &                          ! endchar
    ]
    
    total = total + 1
    print '("Test ", I0, ": Curve path construction")', total
    
    success = ft_cff_charstring_to_outline(charstring, size(charstring), outline, error)
    if (.not. success) then
      print '("  ERROR: Could not parse CharString, error=", I0)', error
      return
    end if
    
    ! Check outline
    print '("  Outline points: ", I0)', outline%n_points
    print '("  Outline contours: ", I0)', outline%n_contours
    
    ! Should have 3 points for a cubic Bezier curve
    if (outline%n_points >= 3 .and. outline%n_contours >= 1) then
      print '("  Result: PASS - Curve path created")'
      passed = passed + 1
    else
      print '("  Result: FAIL - Curve not created correctly")'
    end if
    
    call ft_outline_done(outline)
    
  end subroutine test_curve_path

  ! Test complete glyph
  subroutine test_complete_glyph(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_Outline) :: outline
    integer(FT_Error) :: error
    logical :: success
    
    ! More complex CharString: square glyph with width
    ! 500 100 100 rmoveto 200 0 rlineto 0 200 rlineto -200 0 rlineto endchar
    ! Width=500 (639), movements and lines
    character(len=1), parameter :: charstring(*) = [ &
      char(251), char(0), char(135), &   ! 500 (width)
      char(239), char(239), char(21), &  ! 100 100 rmoveto
      char(28), char(0), char(200), char(139), char(5), & ! 200 0 rlineto (using short int)
      char(139), char(28), char(0), char(200), char(5), & ! 0 200 rlineto
      char(251), char(0), char(92), char(139), char(5), & ! -200 0 rlineto
      char(14) &                          ! endchar
    ]
    
    total = total + 1
    print '("Test ", I0, ": Complete glyph construction")', total
    
    success = ft_cff_charstring_to_outline(charstring, size(charstring), outline, error)
    if (.not. success) then
      print '("  ERROR: Could not parse CharString, error=", I0)', error
      return
    end if
    
    ! Check outline
    print '("  Outline points: ", I0)', outline%n_points
    print '("  Outline contours: ", I0)', outline%n_contours
    
    ! Should have 3 line segments (3 points after the move)
    if (outline%n_points >= 3 .and. outline%n_contours >= 1) then
      ! Check if it forms a closed path
      if (associated(outline%points) .and. outline%n_points >= 3) then
        print '("  First point: (", I0, ", ", I0, ")")', outline%points(1)%x, outline%points(1)%y
        print '("  Last point: (", I0, ", ", I0, ")")', outline%points(outline%n_points)%x, outline%points(outline%n_points)%y
      end if
      print '("  Result: PASS - Complete glyph created")'
      passed = passed + 1
    else
      print '("  Result: FAIL - Glyph not created correctly")'
    end if
    
    call ft_outline_done(outline)
    
  end subroutine test_complete_glyph

end program test_cff_charstring