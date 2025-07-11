program test_cff_subroutines
  use ft_types
  use ft_outline_mod, only: FT_Outline, ft_outline_done
  use ft_cff_charstring
  implicit none
  
  integer :: total_tests = 0
  integer :: passed_tests = 0
  
  print '("CFF Subroutine Tests")'
  print '("===================")'
  print '()'
  
  ! Test 1: Basic subroutine operator handling
  call test_subroutine_operators(passed_tests, total_tests)
  
  ! Test 2: Subroutine call and return
  call test_subroutine_call_return(passed_tests, total_tests)
  
  ! Summary
  print '()'
  print '("===== TEST SUMMARY =====")'
  print '("Total tests: ", I0)', total_tests
  print '("Passed tests: ", I0)', passed_tests
  print '("Failed tests: ", I0)', total_tests - passed_tests
  print '("Success rate: ", F5.1, "%")', real(passed_tests) / real(total_tests) * 100.0
  
  if (passed_tests == total_tests) then
    print '("✓ All CFF subroutine tests PASSED!")'
  else
    print '("✗ Some CFF subroutine tests FAILED!")'
  end if
  
contains

  ! Test basic subroutine operator handling
  subroutine test_subroutine_operators(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_Outline) :: outline
    integer(FT_Error) :: error
    logical :: success
    
    ! CharString with callsubr and callgsubr operators
    ! For now, these should just consume the stack operand and continue
    character(len=1), parameter :: charstring(*) = [ &
      char(239), char(239), char(21), &  ! 100 100 rmoveto
      char(139), char(10), &             ! 0 callsubr (local subroutine 0)
      char(139), char(29), &             ! 0 callgsubr (global subroutine 0)
      char(189), char(139), char(5), &   ! 50 0 rlineto
      char(14) &                         ! endchar
    ]
    
    total = total + 1
    print '("Test ", I0, ": Basic subroutine operator handling")', total
    
    success = ft_cff_charstring_to_outline(charstring, size(charstring), outline, error)
    if (.not. success) then
      print '("  ERROR: Could not parse CharString with subroutines, error=", I0)', error
      return
    end if
    
    ! Check that the outline was created despite the subroutine calls
    if (outline%n_points >= 1 .and. outline%n_contours >= 1) then
      print '("  Result: PASS - CharString with subroutines parsed")'
      print '("  Points: ", I0, " Contours: ", I0)', outline%n_points, outline%n_contours
      passed = passed + 1
    else
      print '("  Result: FAIL - No outline created")'
    end if
    
    call ft_outline_done(outline)
    
  end subroutine test_subroutine_operators

  ! Test subroutine call and return
  subroutine test_subroutine_call_return(passed, total)
    integer, intent(inout) :: passed, total
    
    type(FT_Outline) :: outline
    integer(FT_Error) :: error
    logical :: success
    
    ! CharString with return operator
    character(len=1), parameter :: charstring(*) = [ &
      char(239), char(239), char(21), &  ! 100 100 rmoveto
      char(11), &                        ! return (should just succeed)
      char(189), char(139), char(5), &   ! 50 0 rlineto
      char(14) &                         ! endchar
    ]
    
    total = total + 1
    print '("Test ", I0, ": Subroutine call and return")', total
    
    success = ft_cff_charstring_to_outline(charstring, size(charstring), outline, error)
    if (.not. success) then
      print '("  ERROR: Could not parse CharString with return, error=", I0)', error
      return
    end if
    
    ! Check that the outline was created despite the return
    if (outline%n_points >= 1 .and. outline%n_contours >= 1) then
      print '("  Result: PASS - CharString with return parsed")'
      print '("  Points: ", I0, " Contours: ", I0)', outline%n_points, outline%n_contours
      passed = passed + 1
    else
      print '("  Result: FAIL - No outline created")'
    end if
    
    call ft_outline_done(outline)
    
  end subroutine test_subroutine_call_return

end program test_cff_subroutines