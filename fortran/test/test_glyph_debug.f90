program test_glyph_debug
  use ft_types
  use ft_face_unified, only: FT_Unified_Face, ft_new_unified_face, ft_done_unified_face, &
                             ft_unified_get_glyph_index, ft_unified_load_glyph
  use ft_outline_mod, only: FT_Outline, ft_outline_done
  implicit none
  
  type(FT_Unified_Face) :: face
  type(FT_Outline) :: outline
  integer(FT_Error) :: error
  logical :: success
  integer :: glyph_index
  character(len=256) :: font_file
  integer :: i
  
  print '("Glyph Debug Test")'
  print '("================")'
  print '()'
  
  ! Test with a simpler font first
  font_file = "/usr/share/fonts/TTF/DejaVuSans.ttf"
  
  ! Load the font
  print '("Loading font: ", A)', trim(font_file)
  success = ft_new_unified_face(font_file, 0, face, error)
  if (.not. success) then
    print '("FAILED to load font, error=", I0)', error
    stop 1
  end if
  
  print '("Font loaded successfully")'
  print '("Number of glyphs: ", I0)', face%num_glyphs
  print '()'
  
  ! Test a series of glyphs
  do i = 0, 5
    print '("Testing glyph index ", I0, ":")', i
    
    success = ft_unified_load_glyph(face, i, outline, error)
    if (success) then
      print '("  Points: ", I0, ", Contours: ", I0)', outline%n_points, outline%n_contours
      
      ! Validate the data
      if (outline%n_contours > 0 .and. outline%n_contours < outline%n_points) then
        print '("  GOOD: Reasonable contour/point ratio")'
      else if (outline%n_contours == 0 .and. outline%n_points == 0) then
        print '("  EMPTY: No glyph data")'
      else
        print '("  BAD: Suspicious contour count!")'
      end if
    else
      print '("  FAILED: Error ", I0)', error
    end if
    
    call ft_outline_done(outline)
    print '()'
  end do
  
  ! Now test the problematic glyph 36 (letter 'A')
  print '("Testing problematic glyph 36 (letter A):")'
  glyph_index = 36
  
  success = ft_unified_load_glyph(face, glyph_index, outline, error)
  if (success) then
    print '("  Points: ", I0, ", Contours: ", I0)', outline%n_points, outline%n_contours
    
    if (outline%n_contours > 10) then
      print '("  ERROR: Way too many contours for letter A!")'
      print '("  Expected ~2 contours, got ", I0)', outline%n_contours
    end if
    
    ! Show contour endpoints
    if (outline%n_contours > 0 .and. associated(outline%contours)) then
      print '("  Contour endpoints:")'
      do i = 1, min(10, outline%n_contours)
        print '("    Contour ", I0, " ends at: ", I0)', i, outline%contours(i)
      end do
    end if
  else
    print '("  FAILED: Error ", I0)', error
  end if
  
  call ft_outline_done(outline)
  call ft_done_unified_face(face)
  
  print '()'
  print '("Debug completed")'
  
end program test_glyph_debug