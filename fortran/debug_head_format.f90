program debug_head_format
  use ft_face_unified
  use ft_types
  implicit none
  
  type(FT_Unified_Face) :: face
  integer(FT_Error) :: error
  logical :: success
  character(len=256) :: font_file
  
  print *, "DEBUG HEAD TABLE FORMAT"
  print *, "======================="
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
  
  ! Check head table
  print *, "Head table info:"
  print *, "  head_loaded:", face%truetype_face%head_loaded
  if (face%truetype_face%head_loaded) then
    print *, "  index_to_loc_format:", face%truetype_face%tt_head%index_to_loc_format
    print *, "  units_per_em:", face%truetype_face%tt_head%units_per_em
    print *, "  magic_number:", face%truetype_face%tt_head%magic_number
  end if
  
  ! Check what the loca loading decision is
  print *, ""
  print *, "Loca loading decision:"
  print *, "  is_long condition (index_to_loc_format == 1):", &
           face%truetype_face%tt_head%index_to_loc_format == 1
  print *, "  Should be FALSE for SHORT format (correct for this font)"
  
  call ft_done_unified_face(face)
  
  print *, ""
  print *, "Debug complete!"
  
end program debug_head_format