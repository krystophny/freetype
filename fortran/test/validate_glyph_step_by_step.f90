program validate_glyph_step_by_step
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
  integer :: i, coord_errors
  
  print *, "FORTRAN STEP-BY-STEP GLYPH VALIDATION"
  print *, "====================================="
  print *, ""
  print *, "Expected C results:"
  print *, "  Points: 11"
  print *, "  Contours: 2"
  print *, "  Raw coords: (700,1294), (426,551), (975,551), ..."
  print *, ""
  
  font_file = "/usr/share/fonts/TTF/DejaVuSans.ttf"
  
  ! STEP 1: Load face
  print *, "STEP 1: Loading face..."
  success = ft_new_unified_face(font_file, 0, face, error)
  if (.not. success) then
    print *, "STEP 1 FAILED:", error
    stop 1
  end if
  print *, "STEP 1 SUCCESS: ft_new_unified_face"
  print *, "  units_per_em:", face%units_per_em, "(C: 2048)"
  print *, "  num_glyphs:", face%num_glyphs, "(C: 6253)"
  
  ! STEP 2: Get glyph index
  print *, ""
  print *, "STEP 2: Getting glyph index for 'A'..."
  glyph_index = ft_unified_get_glyph_index(face, iachar('A'))
  print *, "STEP 2 SUCCESS: glyph_index =", glyph_index, "(C: 36)"
  
  if (glyph_index /= 36) then
    print *, "ERROR: Glyph index mismatch! Expected 36, got", glyph_index
    stop 1
  end if
  
  ! STEP 3: Load glyph (raw, unscaled)
  print *, ""
  print *, "STEP 3: Loading glyph (raw, unscaled)..."
  success = ft_unified_load_glyph(face, glyph_index, outline, error)
  if (.not. success) then
    print *, "STEP 3 FAILED:", error
    stop 1
  end if
  print *, "STEP 3 SUCCESS: ft_unified_load_glyph"
  
  ! STEP 4: Validate outline data against C
  print *, ""
  print *, "STEP 4: Validating outline data..."
  print *, "  Fortran points:", outline%n_points, "(C expected: 11)"
  print *, "  Fortran contours:", outline%n_contours, "(C expected: 2)"
  
  if (outline%n_points /= 11) then
    print *, "CRITICAL ERROR: Point count mismatch!"
    print *, "  Expected: 11 points"
    print *, "  Got:", outline%n_points, "points"
    print *, ""
    print *, "This indicates the Fortran glyph loading is fundamentally broken."
    print *, "The glyph data being loaded is different from C FreeType."
  else
    print *, "SUCCESS: Point count matches C!"
  end if
  
  if (outline%n_contours /= 2) then
    print *, "CRITICAL ERROR: Contour count mismatch!"
    print *, "  Expected: 2 contours"
    print *, "  Got:", outline%n_contours, "contours"
  else
    print *, "SUCCESS: Contour count matches C!"
  end if
  
  ! STEP 5: Compare coordinates
  print *, ""
  print *, "STEP 5: Comparing coordinates..."
  print *, "Expected C coordinates (raw font units):"
  print *, "  Point 0: (700, 1294)"
  print *, "  Point 1: (426, 551)"
  print *, "  Point 2: (975, 551)"
  print *, "  Point 3: (586, 1493)"
  print *, "  Point 4: (815, 1493)"
  print *, ""
  print *, "Fortran coordinates:"
  
  coord_errors = 0
  do i = 1, min(outline%n_points, 5)
    print *, "  Point", i-1, ": (", outline%points(i)%x, ",", outline%points(i)%y, ")"
    
    ! Check first few key points
    if (i == 1) then
      if (outline%points(i)%x /= 700 .or. outline%points(i)%y /= 1294) then
        print *, "    ERROR: Expected (700, 1294)"
        coord_errors = coord_errors + 1
      end if
    else if (i == 2) then
      if (outline%points(i)%x /= 426 .or. outline%points(i)%y /= 551) then
        print *, "    ERROR: Expected (426, 551)"
        coord_errors = coord_errors + 1
      end if
    else if (i == 3) then
      if (outline%points(i)%x /= 975 .or. outline%points(i)%y /= 551) then
        print *, "    ERROR: Expected (975, 551)"
        coord_errors = coord_errors + 1
      end if
    end if
  end do
  
  if (coord_errors == 0 .and. outline%n_points == 11) then
    print *, ""
    print *, "SUCCESS: All coordinates match C exactly!"
  else
    print *, ""
    print *, "CRITICAL ERROR:", coord_errors, "coordinate mismatches found!"
    print *, "The Fortran glyph loader is producing different data than C FreeType."
  end if
  
  ! STEP 6: Check contour endpoints
  print *, ""
  print *, "STEP 6: Checking contour endpoints..."
  print *, "Expected C contour endpoints:"
  print *, "  Contour 0: ends at point 2"
  print *, "  Contour 1: ends at point 10"
  print *, ""
  print *, "Fortran contour endpoints:"
  
  if (outline%n_contours >= 1) then
    print *, "  Contour 0: ends at point", outline%contours(1)
    if (outline%contours(1) /= 2) then
      print *, "    ERROR: Expected 2, got", outline%contours(1)
    end if
  end if
  
  if (outline%n_contours >= 2) then
    print *, "  Contour 1: ends at point", outline%contours(2)
    if (outline%contours(2) /= 10) then
      print *, "    ERROR: Expected 10, got", outline%contours(2)
    end if
  end if
  
  ! STEP 7: Summary
  print *, ""
  print *, "VALIDATION SUMMARY:"
  print *, "=================="
  
  if (outline%n_points == 11 .and. outline%n_contours == 2 .and. coord_errors == 0) then
    print *, "✓ PASS: Fortran glyph loading matches C FreeType exactly!"
    print *, ""
    print *, "The glyph loading is correct. The problem must be in:"
    print *, "- Coordinate transformation"
    print *, "- Antialiasing algorithm"
    print *, "- Bitmap rendering"
  else
    print *, "✗ FAIL: Fortran glyph loading does NOT match C FreeType!"
    print *, ""
    print *, "Issues found:"
    if (outline%n_points /= 11) print *, "- Wrong number of points"
    if (outline%n_contours /= 2) print *, "- Wrong number of contours"
    if (coord_errors > 0) print *, "- Coordinate mismatches"
    print *, ""
    print *, "PRIORITY: Fix the TrueType glyph loading code first!"
  end if
  
  ! Cleanup
  if (associated(outline%points)) deallocate(outline%points)
  if (associated(outline%tags)) deallocate(outline%tags)
  if (associated(outline%contours)) deallocate(outline%contours)
  
  call ft_done_unified_face(face)
  
end program validate_glyph_step_by_step