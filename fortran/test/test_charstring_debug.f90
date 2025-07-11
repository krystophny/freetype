program test_charstring_debug
  use ft_cff_charstring
  use ft_outline_mod
  use ft_types
  implicit none
  
  type(FT_Outline) :: outline
  integer(FT_Error) :: error
  logical :: success
  
  ! Test two rlineto operations
  character(len=1), parameter :: simple_charstring(*) = [ &
    char(28), char(1), char(244), &    ! 500 (width) - short integer
    char(239), char(239), char(21), &  ! 100 100 rmoveto
    char(28), char(0), char(200), char(139), char(5), & ! 200 0 rlineto
    char(139), char(28), char(0), char(200), char(5), & ! 0 200 rlineto  
    char(14) &                          ! endchar
  ]
  
  print '("CharString Debug Test")'
  print '("====================")'
  
  ! Test the simple CharString
  success = ft_cff_charstring_to_outline(simple_charstring, size(simple_charstring), outline, error)
  if (.not. success) then
    print '("ERROR: Could not parse simple CharString, error=", I0)', error
  else
    print '("SUCCESS: Simple CharString parsed")'
    print '("Points: ", I0, " Contours: ", I0)', outline%n_points, outline%n_contours
  end if
  
  call ft_outline_done(outline)
  
end program test_charstring_debug