program debug_glyph_loading_detailed
  use ft_face_unified
  use ft_types
  use ft_outline_mod
  use tt_glyph, only: TT_Simple_Glyph, tt_load_glyph_by_index_with_offset
  use tt_types, only: TTAG_glyf
  use ft_geometry, only: FT_Vector
  implicit none
  
  type(FT_Unified_Face) :: face
  type(FT_Outline) :: outline
  integer(FT_Error) :: error
  logical :: success
  integer :: glyph_index
  character(len=256) :: font_file
  integer :: i
  
  ! Additional debug variables
  type(TT_Simple_Glyph) :: raw_glyph
  integer(8) :: glyf_offset
  logical :: found
  
  print *, "DETAILED GLYPH LOADING DEBUG"
  print *, "============================"
  print *, ""
  
  font_file = "/usr/share/fonts/TTF/DejaVuSans.ttf"
  
  ! Load face
  success = ft_new_unified_face(font_file, 0, face, error)
  if (.not. success) stop "Failed to load face"
  
  ! Get glyph index
  glyph_index = ft_unified_get_glyph_index(face, iachar('A'))
  print *, "Glyph index for 'A':", glyph_index
  
  if (.not. allocated(face%truetype_face)) then
    print *, "ERROR: TrueType face not allocated!"
    stop
  end if
  
  ! Debug: Check loca table
  print *, ""
  print *, "Loca table debug:"
  print *, "  loca_loaded:", face%truetype_face%loca_loaded
  if (face%truetype_face%loca_loaded) then
    ! print *, "  loca format:", face%truetype_face%tt_loca%format
    print *, "  loca table size:", size(face%truetype_face%tt_loca%offsets)
    if (glyph_index + 2 <= size(face%truetype_face%tt_loca%offsets)) then
      print *, "  Glyph", glyph_index, "offset:", face%truetype_face%tt_loca%offsets(glyph_index + 1)
      print *, "  Glyph", glyph_index+1, "offset:", face%truetype_face%tt_loca%offsets(glyph_index + 2)
      print *, "  Glyph size:", face%truetype_face%tt_loca%offsets(glyph_index + 2) - &
                                  face%truetype_face%tt_loca%offsets(glyph_index + 1)
    end if
  end if
  
  ! Debug: Find glyf table offset
  found = .false.
  glyf_offset = 0
  do i = 1, size(face%truetype_face%directory%tables)
    if (face%truetype_face%directory%tables(i)%tag == TTAG_glyf) then
      glyf_offset = int(face%truetype_face%directory%tables(i)%offset, 8)
      found = .true.
      exit
    end if
  end do
  
  print *, ""
  print *, "Glyf table debug:"
  print *, "  glyf table found:", found
  if (found) then
    print *, "  glyf table offset:", glyf_offset
  end if
  
  ! Load raw glyph data
  print *, ""
  print *, "Loading raw glyph data..."
  success = tt_load_glyph_by_index_with_offset(face%truetype_face%stream, &
                                               face%truetype_face%tt_loca, &
                                               glyph_index, int(glyf_offset, 8), &
                                               raw_glyph, error)
  
  if (.not. success) then
    print *, "ERROR: Failed to load raw glyph:", error
    stop
  end if
  
  print *, "Raw glyph loaded successfully!"
  print *, "  Header num_contours:", raw_glyph%header%num_contours
  print *, "  Header x_min:", raw_glyph%header%x_min
  print *, "  Header y_min:", raw_glyph%header%y_min
  print *, "  Header x_max:", raw_glyph%header%x_max
  print *, "  Header y_max:", raw_glyph%header%y_max
  print *, "  Number of points:", raw_glyph%num_points
  
  if (raw_glyph%header%num_contours >= 0) then
    print *, "  Glyph type: SIMPLE"
  else
    print *, "  Glyph type: COMPOSITE (num_contours =", raw_glyph%header%num_contours, ")"
  end if
  
  ! Show first few coordinates
  print *, ""
  print *, "Raw coordinates:"
  do i = 1, min(raw_glyph%num_points, 5)
    print *, "  Point", i-1, ": (", raw_glyph%x_coordinates(i), ",", raw_glyph%y_coordinates(i), ")", &
             " on_curve:", raw_glyph%on_curve(i)
  end do
  
  ! Compare with expected C values
  print *, ""
  print *, "Comparison with C FreeType:"
  print *, "  C points: 11, Fortran points:", raw_glyph%num_points
  print *, "  C contours: 2, Fortran contours:", raw_glyph%header%num_contours
  print *, "  C Point 0: (700, 1294), Fortran Point 0: (", &
           raw_glyph%x_coordinates(1), ",", raw_glyph%y_coordinates(1), ")"
  
  ! Now convert to outline and see what happens
  print *, ""
  print *, "Converting to outline..."
  success = ft_unified_load_glyph(face, glyph_index, outline, error)
  if (.not. success) then
    print *, "ERROR: Failed to convert to outline:", error
    stop
  end if
  
  print *, "Outline conversion:"
  print *, "  Outline points:", outline%n_points
  print *, "  Outline contours:", outline%n_contours
  print *, "  First outline point: (", outline%points(1)%x, ",", outline%points(1)%y, ")"
  
  ! Check if there's expansion happening
  if (outline%n_points /= raw_glyph%num_points) then
    print *, ""
    print *, "WARNING: Point count changed during conversion!"
    print *, "  Raw glyph points:", raw_glyph%num_points
    print *, "  Outline points:", outline%n_points
    print *, "  This suggests the conversion process is modifying the data"
  end if
  
  ! Clean up
  if (associated(outline%points)) deallocate(outline%points)
  if (associated(outline%tags)) deallocate(outline%tags)
  if (associated(outline%contours)) deallocate(outline%contours)
  
  call ft_done_unified_face(face)
  
  print *, ""
  print *, "Debug complete!"
  
end program debug_glyph_loading_detailed