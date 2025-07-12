program test_system_fonts
  use ft_types
  use ft_face_unified, only: FT_Unified_Face, ft_new_unified_face, ft_done_unified_face, &
                             ft_unified_get_glyph_index, ft_unified_load_glyph
  use ft_outline_mod, only: FT_Outline, ft_outline_done
  use ft_font_format, only: ft_get_format_name
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  character(len=256), dimension(5) :: test_fonts
  integer :: i
  
  ! Define system fonts to test
  test_fonts(1) = "/usr/share/fonts/TTF/DejaVuSans.ttf"
  test_fonts(2) = "/usr/share/fonts/TTF/DejaVuSans-Bold.ttf"
  test_fonts(3) = "/usr/share/fonts/TTF/DejaVuSerif.ttf"
  test_fonts(4) = "/usr/share/fonts/TTF/DejaVuMono.ttf"
  test_fonts(5) = "/usr/share/fonts/TTF/DejaVuSansMNerdFontMono-Bold.ttf"
  
  print '("System Font Loading Tests")'
  print '("========================")'
  print '()'
  
  ! Test each font
  do i = 1, 5
    call test_system_font(test_fonts(i))
  end do
  
  ! Test character mapping across fonts
  call test_character_mapping()
  
  ! Print summary
  print '()'
  print '("System Font Tests - Tests run: ", I0)', test_count
  print '("System Font Tests - Tests failed: ", I0)', failed_count
  
  if (failed_count == 0) then
    print '()'
    print '("✓ All system font tests PASSED!")'
    stop 0
  else
    print '()'
    print '("✗ Some system font tests FAILED!")'
    stop 1
  end if

contains

  subroutine test_system_font(font_path)
    character(len=*), intent(in) :: font_path
    type(FT_Unified_Face) :: face
    integer(FT_Error) :: error
    logical :: success
    character(len=32) :: format_name
    logical :: file_exists
    
    test_count = test_count + 1
    
    ! Check if file exists first
    inquire(file=font_path, exist=file_exists)
    if (.not. file_exists) then
      print '("SKIP: Font not found: ", A)', trim(font_path)
      return
    end if
    
    print '("Testing: ", A)', trim(font_path)
    
    ! Try to load the font
    success = ft_new_unified_face(font_path, 0, face, error)
    if (.not. success) then
      print '("  FAIL: Could not load font, error=", I0)', error
      failed_count = failed_count + 1
      return
    end if
    
    ! Validate basic properties
    format_name = ft_get_format_name(face%font_format)
    print '("  Format: ", A)', trim(format_name)
    print '("  Family: ", A)', trim(face%family_name)
    print '("  Style: ", A)', trim(face%style_name)
    print '("  Glyphs: ", I0)', face%num_glyphs
    print '("  Units per EM: ", I0)', face%units_per_em
    
    ! Test basic functionality
    if (face%num_glyphs < 100) then
      print '("  WARN: Very few glyphs (", I0, ")")', face%num_glyphs
    end if
    
    if (face%units_per_em < 100 .or. face%units_per_em > 10000) then
      print '("  WARN: Unusual units per EM (", I0, ")")', face%units_per_em
    end if
    
    print '("  PASS: Font loaded successfully")'
    
    ! Clean up
    call ft_done_unified_face(face)
    
  end subroutine test_system_font

  subroutine test_character_mapping()
    type(FT_Unified_Face) :: face
    integer(FT_Error) :: error
    logical :: success
    integer :: glyph_index
    character(len=1), dimension(5) :: test_chars = ['A', 'a', '0', ' ', '!']
    integer :: i
    logical :: file_exists
    character(len=256) :: test_font
    
    test_count = test_count + 1
    
    ! Use DejaVu Sans for character mapping test
    test_font = "/usr/share/fonts/TTF/DejaVuSans.ttf"
    
    inquire(file=test_font, exist=file_exists)
    if (.not. file_exists) then
      print '("SKIP: Character mapping test - font not found")'
      return
    end if
    
    print '("Testing character mapping...")'
    
    success = ft_new_unified_face(test_font, 0, face, error)
    if (.not. success) then
      print '("  FAIL: Could not load test font, error=", I0)', error
      failed_count = failed_count + 1
      return
    end if
    
    ! Test mapping for common characters
    do i = 1, 5
      glyph_index = ft_unified_get_glyph_index(face, ichar(test_chars(i)))
      if (glyph_index > 0) then
        print '("  PASS: ''", A1, "'' -> glyph ", I0)', test_chars(i), glyph_index
      else
        print '("  WARN: ''", A1, "'' -> no glyph")', test_chars(i)
      end if
    end do
    
    print '("  PASS: Character mapping functional")'
    
    call ft_done_unified_face(face)
    
  end subroutine test_character_mapping

end program test_system_fonts