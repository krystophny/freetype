program debug_rendering_coords
  use ft_face_unified
  use ft_types
  use ft_outline_mod
  use ft_geometry, only: FT_Vector
  implicit none
  
  type(FT_Unified_Face) :: face
  type(FT_Outline) :: outline
  integer(FT_Error) :: error
  logical :: success
  integer :: glyph_index
  character(len=256) :: font_file
  integer :: i
  
  print *, "DEBUG: Rendering Coordinate Analysis"
  print *, "==================================="
  print *, ""
  
  font_file = "/usr/share/fonts/TTF/DejaVuSans.ttf"
  
  ! Load font
  success = ft_new_unified_face(font_file, 0, face, error)
  if (.not. success) then
    print *, "Failed to load face, error:", error
    stop
  end if
  
  print *, "Font loaded successfully!"
  print *, "  Units per EM:", face%units_per_em
  print *, ""
  
  ! Get glyph for 'A'
  glyph_index = ft_unified_get_glyph_index(face, iachar('A'))
  print *, "Glyph index for 'A':", glyph_index
  
  ! Load glyph outline
  success = ft_unified_load_glyph(face, glyph_index, outline, error)
  if (.not. success) then
    print *, "ERROR: Failed to load glyph:", error
    call ft_done_unified_face(face)
    stop
  end if
  
  print *, "Glyph loaded successfully!"
  print *, "  Points:", outline%n_points
  print *, "  Contours:", outline%n_contours
  print *, ""
  
  ! Show all coordinates
  print *, "Raw outline coordinates (font units):"
  do i = 1, outline%n_points
    print *, "  Point", i-1, ": (", outline%points(i)%x, ",", outline%points(i)%y, ") on_curve:", &
             iand(int(outline%tags(i)), 1) /= 0
  end do
  
  print *, ""
  print *, "Contour endpoints:"
  do i = 1, outline%n_contours
    print *, "  Contour", i-1, ": ends at point", outline%contours(i)
  end do
  
  ! Calculate bounding box
  block
    integer :: min_x, max_x, min_y, max_y
    
    min_x = outline%points(1)%x
    max_x = outline%points(1)%x
    min_y = outline%points(1)%y
    max_y = outline%points(1)%y
    
    do i = 2, outline%n_points
      min_x = min(min_x, outline%points(i)%x)
      max_x = max(max_x, outline%points(i)%x)
      min_y = min(min_y, outline%points(i)%y)
      max_y = max(max_y, outline%points(i)%y)
    end do
    
    print *, ""
    print *, "Bounding box:"
    print *, "  X range: ", min_x, " to ", max_x, " (width:", max_x - min_x, ")"
    print *, "  Y range: ", min_y, " to ", max_y, " (height:", max_y - min_y, ")"
    print *, ""
    print *, "For bitmap rendering, these coordinates need to be:"
    print *, "  1. Scaled from font units (", face%units_per_em, ") to pixel size"
    print *, "  2. Translated to fit within bitmap bounds"
    print *, "  3. Y-flipped (font Y up -> bitmap Y down)"
  end block
  
  ! Clean up
  if (associated(outline%points)) deallocate(outline%points)
  if (associated(outline%tags)) deallocate(outline%tags)
  if (associated(outline%contours)) deallocate(outline%contours)
  
  call ft_done_unified_face(face)
  
  print *, ""
  print *, "Analysis complete!"
  
end program debug_rendering_coords