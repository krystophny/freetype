program debug_loca_loading
  use ft_face_unified
  use ft_types
  use tt_loca
  implicit none
  
  type(FT_Unified_Face) :: face
  integer(FT_Error) :: error
  logical :: success
  character(len=256) :: font_file
  integer :: i
  
  print *, "DEBUG LOCA TABLE LOADING"
  print *, "========================"
  print *, ""
  
  font_file = "/usr/share/fonts/TTF/DejaVuSans.ttf"
  
  ! Load face
  success = ft_new_unified_face(font_file, 0, face, error)
  if (.not. success) then
    print *, "Failed to load face, error:", error
    stop
  end if
  
  if (.not. allocated(face%truetype_face)) then
    print *, "ERROR: TrueType face not allocated!"
    stop
  end if
  
  ! Check loca table details
  if (face%truetype_face%loca_loaded) then
    print *, "Loca table loaded successfully"
    print *, "  Array bounds:", lbound(face%truetype_face%tt_loca%offsets), &
             "to", ubound(face%truetype_face%tt_loca%offsets)
    print *, "  Is long format:", face%truetype_face%tt_loca%is_long_format
    print *, "  Num glyphs:", face%truetype_face%tt_loca%num_glyphs
    
    print *, ""
    print *, "First 10 loca entries:"
    do i = 0, 9
      if (i <= ubound(face%truetype_face%tt_loca%offsets, 1)) then
        print *, "  loca[", i, "] =", face%truetype_face%tt_loca%offsets(i)
      end if
    end do
    
    print *, ""
    print *, "Loca entries around glyph 36:"
    do i = 34, 39
      if (i <= ubound(face%truetype_face%tt_loca%offsets, 1)) then
        print *, "  loca[", i, "] =", face%truetype_face%tt_loca%offsets(i)
      end if
    end do
    
    print *, ""
    print *, "Expected from C analysis:"
    print *, "  loca[36] should be 5432 (0x1538)"
    print *, "  loca[37] should be 5684 (0x1634)"
    print *, "  Actual Fortran values:"
    if (36 <= ubound(face%truetype_face%tt_loca%offsets, 1)) then
      print *, "  loca[36] =", face%truetype_face%tt_loca%offsets(36)
    end if
    if (37 <= ubound(face%truetype_face%tt_loca%offsets, 1)) then
      print *, "  loca[37] =", face%truetype_face%tt_loca%offsets(37)
    end if
  else
    print *, "ERROR: Loca table not loaded!"
  end if
  
  call ft_done_unified_face(face)
  
  print *, ""
  print *, "Debug complete!"
  
end program debug_loca_loading