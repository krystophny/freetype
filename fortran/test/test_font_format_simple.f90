program test_font_format_simple
  use ft_types
  use ft_font_format
  implicit none
  
  integer :: total_tests = 0
  integer :: passed_tests = 0
  
  print '("Font Format Detection Tests (Simple)")'
  print '("====================================")'
  print '()'
  
  ! Test format name function
  call test_format_names(passed_tests, total_tests)
  
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

  ! Test format name function
  subroutine test_format_names(passed, total)
    integer, intent(inout) :: passed, total
    
    character(len=32) :: name
    logical :: all_good
    
    total = total + 1
    print '("Test ", I0, ": Format name function")', total
    
    all_good = .true.
    
    ! Test each format
    name = ft_get_format_name(FT_FONT_FORMAT_UNKNOWN)
    print '("  Unknown: ", A)', trim(name)
    if (name /= "Unknown") all_good = .false.
    
    name = ft_get_format_name(FT_FONT_FORMAT_TRUETYPE)
    print '("  TrueType: ", A)', trim(name)
    if (name /= "TrueType") all_good = .false.
    
    name = ft_get_format_name(FT_FONT_FORMAT_TYPE1)
    print '("  Type 1: ", A)', trim(name)
    if (name /= "Type 1") all_good = .false.
    
    name = ft_get_format_name(FT_FONT_FORMAT_CFF)
    print '("  CFF: ", A)', trim(name)
    if (name /= "CFF") all_good = .false.
    
    name = ft_get_format_name(FT_FONT_FORMAT_OPENTYPE)
    print '("  OpenType: ", A)', trim(name)
    if (name /= "OpenType") all_good = .false.
    
    if (all_good) then
      print '("  Result: PASS - All format names correct")'
      passed = passed + 1
    else
      print '("  Result: FAIL - Some format names incorrect")'
    end if
    
  end subroutine test_format_names

end program test_font_format_simple