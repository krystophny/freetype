program test_glyph_loading
  use ft_types
  use ft_face_unified, only: FT_Unified_Face, ft_new_unified_face, ft_done_unified_face, &
                             ft_unified_get_glyph_index, ft_unified_load_glyph
  use ft_face, only: ft_get_char_index
  use ft_outline_mod, only: FT_Outline, ft_outline_done
  use ft_geometry, only: FT_Vector
  use ft_bitmap_mod, only: FT_Bitmap, ft_bitmap_new, ft_bitmap_done, ft_bitmap_get_pixel, &
                           FT_PIXEL_MODE_MONO
  use ft_raster, only: ft_raster_render_outline_scanline
  use ft_bitmap_io, only: ft_bitmap_write_pbm
  implicit none
  
  type(FT_Unified_Face) :: face
  type(FT_Outline) :: outline
  type(FT_Bitmap) :: bitmap
  integer(FT_Error) :: error
  logical :: success
  integer :: glyph_index
  character(len=256) :: font_file
  integer :: x, y, i
  integer :: direct_glyph_index
  character(len=1) :: pixel
  
  print '("Glyph Loading Test")'
  print '("==================")'
  print '()'
  
  ! Use a real system font
  font_file = "/usr/share/fonts/TTF/DejaVuSansMNerdFontMono-Bold.ttf"
  
  ! Load the font
  print '("Loading font: ", A)', trim(font_file)
  success = ft_new_unified_face(font_file, 0, face, error)
  if (.not. success) then
    print '("FAILED to load font, error=", I0)', error
    stop 1
  end if
  
  print '("SUCCESS: Loaded font")'
  print '("Font format: ", I0)', face%font_format
  print '("Number of glyphs: ", I0)', face%num_glyphs
  print '("TrueType face allocated: ", L1)', allocated(face%truetype_face)
  if (allocated(face%truetype_face)) then
    print '("TrueType face loaded tables:")'
    print '("  head_loaded: ", L1)', face%truetype_face%head_loaded
    print '("  maxp_loaded: ", L1)', face%truetype_face%maxp_loaded
    print '("  cmap_loaded: ", L1)', face%truetype_face%cmap_loaded
    print '("  loca_loaded: ", L1)', face%truetype_face%loca_loaded
    if (face%truetype_face%cmap_loaded) then
      print '("  cmap num_tables: ", I0)', face%truetype_face%tt_cmap%header%num_tables
      print '("  cmap has_format4: ", L1)', face%truetype_face%tt_cmap%has_format4
      print '("  num_charmaps: ", I0)', face%truetype_face%num_charmaps
      print '("  charmap_index: ", I0)', face%truetype_face%charmap_index
      if (allocated(face%truetype_face%tt_cmap%encodings) .and. &
          face%truetype_face%tt_cmap%header%num_tables > 0) then
        print '("  first encoding platform: ", I0)', face%truetype_face%tt_cmap%encodings(1)%platform_id
        print '("  first encoding encoding: ", I0)', face%truetype_face%tt_cmap%encodings(1)%encoding_id
      end if
    end if
  end if
  print '()'
  
  ! Test glyph index for character 'A' (ASCII 65)
  glyph_index = ft_unified_get_glyph_index(face, 65)
  print '("Glyph index for ''A'' (65): ", I0)', glyph_index
  
  ! Debug: Test direct call to TrueType character mapping if allocated
  if (allocated(face%truetype_face)) then
    direct_glyph_index = ft_get_char_index(face%truetype_face, 65)
    print '("Direct TrueType glyph index for ''A'': ", I0)', direct_glyph_index
  end if
  
  ! For testing, try a simple glyph first
  print '("Trying simpler glyph index 1 instead of complex A")'
  glyph_index = 1
  
  ! Load the glyph
  print '("Loading glyph index ", I0)', glyph_index
  success = ft_unified_load_glyph(face, glyph_index, outline, error)
  if (.not. success) then
    print '("FAILED to load glyph, error=", I0)', error
    call ft_done_unified_face(face)
    stop 1
  end if
  
  print '("SUCCESS: Loaded glyph")'
  print '("Number of points: ", I0)', outline%n_points
  print '("Number of contours: ", I0)', outline%n_contours
  
  ! Show some outline points
  if (outline%n_points > 0) then
    print '("First few points:")'
    do i = 1, min(5, outline%n_points)
      print '("  Point ", I0, ": (", I0, ",", I0, "), tag=", I0)', &
            i, outline%points(i)%x, outline%points(i)%y, outline%tags(i)
    end do
  end if
  
  ! Show contour structure  
  if (outline%n_contours > 0 .and. associated(outline%contours)) then
    print '("Contour endpoints (0-based):")'
    do i = 1, min(5, outline%n_contours)
      print '("  Contour ", I0, " ends at point: ", I0)', i, outline%contours(i)
    end do
  end if
  print '()'
  
  ! Create a bitmap and render the glyph
  print '("Rendering glyph to 32x32 bitmap...")'
  success = ft_bitmap_new(32, 32, FT_PIXEL_MODE_MONO, bitmap, error)
  if (.not. success) then
    print '("FAILED to create bitmap, error=", I0)', error
    call ft_outline_done(outline)
    call ft_done_unified_face(face)
    stop 1
  end if
  
  ! Render outline to bitmap
  success = ft_raster_render_outline_scanline(outline, bitmap, error)
  if (success) then
    print '("SUCCESS: Rendered glyph")'
    print '("Bitmap preview:")'
    
    ! Show first 16x16 of the bitmap
    do y = 0, min(15, bitmap%rows - 1)
      write(*, '("  ")', advance='no')
      do x = 0, min(15, bitmap%width - 1)
        if (ft_bitmap_get_pixel(bitmap, x, y)) then
          pixel = '*'
        else
          pixel = '.'
        end if
        write(*, '(A)', advance='no') pixel
      end do
      write(*, *)
    end do
    
    ! Save bitmap to file
    if (ft_bitmap_write_pbm(bitmap, "test_glyph_load.pbm", error)) then
      print '()'
      print '("Saved bitmap to test_glyph_load.pbm")'
    end if
  else
    print '("FAILED to render glyph, error=", I0)', error
  end if
  
  ! Cleanup
  call ft_bitmap_done(bitmap)
  call ft_outline_done(outline)
  call ft_done_unified_face(face)
  
  print '()'
  print '("Test completed")'
  
end program test_glyph_loading