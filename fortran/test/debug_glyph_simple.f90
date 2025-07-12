program debug_glyph_simple
  use ft_types
  use ft_face_unified
  use ft_outline_mod
  implicit none
  
  type(FT_Unified_Face) :: face
  type(FT_Outline) :: outline
  integer(FT_Error) :: error
  logical :: success
  integer :: glyph_index
  character(len=256) :: font_file
  
  print *, "Debug: Simple Glyph Loading Test"
  print *, "================================"
  
  font_file = "/usr/share/fonts/TTF/DejaVuSans.ttf"
  
  ! Load font
  print *, "Loading font:", trim(font_file)
  success = ft_new_unified_face(font_file, 0, face, error)
  if (.not. success) then
    print *, "FAILED to load font, error=", error
    stop 1
  end if
  
  print *, "Font loaded, units_per_em:", face%units_per_em
  
  ! Try several different glyph indices
  do glyph_index = 1, 10
    print *, ""
    print *, "Testing glyph index:", glyph_index
    
    success = ft_unified_load_glyph(face, glyph_index, outline, error)
    if (success) then
      print *, "  SUCCESS: points=", outline%n_points, "contours=", outline%n_contours
      if (outline%n_points > 0) then
        print *, "  First point: (", outline%points(1)%x, ",", outline%points(1)%y, ")"
        exit ! Found a valid glyph, stop here
      else
        print *, "  WARNING: Glyph loaded but has no points"
      end if
      
      ! Clean up outline
      if (associated(outline%points)) deallocate(outline%points)
      if (associated(outline%tags)) deallocate(outline%tags)
      if (associated(outline%contours)) deallocate(outline%contours)
    else
      print *, "  FAILED: error=", error
    end if
  end do
  
  ! Try the 'A' character specifically
  print *, ""
  print *, "Testing character 'A' (glyph index 36):"
  glyph_index = ft_unified_get_glyph_index(face, iachar('A'))
  print *, "  Glyph index for 'A':", glyph_index
  
  if (glyph_index > 0) then
    success = ft_unified_load_glyph(face, glyph_index, outline, error)
    if (success) then
      print *, "  SUCCESS: points=", outline%n_points, "contours=", outline%n_contours
      if (outline%n_points > 0) then
        print *, "  First point: (", outline%points(1)%x, ",", outline%points(1)%y, ")"
        print *, "  Last point: (", outline%points(outline%n_points)%x, ",", outline%points(outline%n_points)%y, ")"
      end if
      
      ! Clean up outline
      if (associated(outline%points)) deallocate(outline%points)
      if (associated(outline%tags)) deallocate(outline%tags)
      if (associated(outline%contours)) deallocate(outline%contours)
    else
      print *, "  FAILED: error=", error
    end if
  end if
  
  call ft_done_unified_face(face)
  
end program debug_glyph_simple