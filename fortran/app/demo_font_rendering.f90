program demo_font_rendering
  use ft_face_unified
  use ft_types
  use ft_bitmap_mod
  use ft_outline_mod
  use ft_outline_transform_mod
  use ft_antialiasing_enhanced
  use ft_hint
  use ft_bitmap_io
  use ft_geometry, only: FT_Vector
  implicit none
  
  type(FT_Unified_Face) :: face
  type(FT_Outline) :: outline, transformed_outline
  type(FT_Bitmap) :: bitmap_no_aa, bitmap_aa, bitmap_aa_hinted
  type(FT_AA_Engine) :: aa_engine
  type(FT_AA_Quality) :: aa_quality
  type(FT_Hint_Engine) :: hint_engine
  integer(FT_Error) :: error
  logical :: success
  integer :: glyph_index
  character(len=256) :: font_file
  character :: test_char = 'A'
  integer :: bitmap_size = 64
  
  print *, "Font Rendering Demonstration: Antialiasing & Hinting"
  print *, "==================================================="
  print *
  
  ! Use DejaVu Sans as test font
  font_file = "/usr/share/fonts/TTF/DejaVuSans.ttf"
  
  ! Load font
  print *, "Loading font:", trim(font_file)
  success = ft_new_unified_face(font_file, 0, face, error)
  if (.not. success) then
    print *, "ERROR: Failed to load font:", error
    stop
  end if
  
  print *, "Font loaded successfully!"
  print *, "  Format:", face%font_format
  print *, "  Glyphs:", face%num_glyphs
  print *, "  Units per EM:", face%units_per_em
  print *
  
  ! Get glyph for character 'A'
  glyph_index = ft_unified_get_glyph_index(face, iachar(test_char))
  print *, "Glyph index for '", test_char, "':", glyph_index
  
  ! Load glyph outline
  success = ft_unified_load_glyph(face, glyph_index, outline, error)
  if (.not. success) then
    print *, "ERROR: Failed to load glyph:", error
    call ft_done_unified_face(face)
    stop
  end if
  
  print *, "Glyph loaded:"
  print *, "  Points:", outline%n_points
  print *, "  Contours:", outline%n_contours
  print *
  
  ! Transform outline coordinates to bitmap space
  print *, "Transforming outline coordinates to bitmap space..."
  success = ft_outline_transform_to_bitmap(outline, transformed_outline, &
                                          face%units_per_em, bitmap_size, bitmap_size, error)
  if (.not. success) then
    print *, "ERROR: Failed to transform outline:", error
    call ft_done_unified_face(face)
    stop
  end if
  print *, "Outline transformed successfully!"
  print *
  
  ! Create bitmaps
  success = ft_bitmap_new(bitmap_size, bitmap_size, FT_PIXEL_MODE_GRAY, bitmap_no_aa, error)
  if (.not. success) stop "Failed to create bitmap"
  
  success = ft_bitmap_new(bitmap_size, bitmap_size, FT_PIXEL_MODE_GRAY, bitmap_aa, error)
  if (.not. success) stop "Failed to create bitmap"
  
  success = ft_bitmap_new(bitmap_size, bitmap_size, FT_PIXEL_MODE_GRAY, bitmap_aa_hinted, error)
  if (.not. success) stop "Failed to create bitmap"
  
  ! Clear bitmaps
  call ft_bitmap_clear(bitmap_no_aa)
  call ft_bitmap_clear(bitmap_aa)
  call ft_bitmap_clear(bitmap_aa_hinted)
  
  ! 1. Render without antialiasing (placeholder for simple rendering)
  print *, "1. Rendering without antialiasing..."
  ! Note: This would use simple scanline rendering
  print *, "   (Simple rendering - placeholder)"
  
  ! 2. Render with antialiasing
  print *, "2. Rendering with antialiasing (Normal quality)..."
  
  ! Set up antialiasing engine
  aa_quality%level = FT_AA_QUALITY_NORMAL
  aa_quality%samples_per_pixel = 4
  aa_quality%gamma_correction = 2.2
  aa_quality%linear_filtering = .false.
  aa_quality%edge_enhancement = .false.
  
  success = ft_aa_engine_new(bitmap_size, bitmap_size, aa_quality, aa_engine, error)
  if (.not. success) then
    print *, "ERROR: Failed to create AA engine:", error
    stop
  end if
  
  ! Render with antialiasing using transformed outline
  success = ft_aa_render_outline(aa_engine, transformed_outline, bitmap_aa, error)
  if (success) then
    print *, "   SUCCESS: Antialiased rendering complete"
  else
    print *, "   ERROR: Antialiasing failed:", error
  end if
  
  call ft_aa_engine_done(aa_engine)
  
  ! 3. Render with antialiasing + hinting
  print *, "3. Rendering with antialiasing + hinting..."
  
  ! Set up hinting engine
  success = ft_hint_engine_new(face%units_per_em, real(bitmap_size), hint_engine, error)
  if (success) then
    print *, "   Hinting engine created"
    print *, "   Hint type:", hint_engine%hint_settings%hint_type
    print *, "   Grid fitting:", hint_engine%hint_settings%grid_fitting
    
    ! Apply hinting to transformed outline
    success = ft_hint_apply_outline(hint_engine, transformed_outline, error)
    if (success) then
      print *, "   Hinting applied successfully"
    else
      print *, "   WARNING: Hinting failed:", error
    end if
    
    call ft_hint_engine_done(hint_engine)
  end if
  
  ! Set up high-quality antialiasing
  aa_quality%level = FT_AA_QUALITY_HIGH
  aa_quality%samples_per_pixel = 8
  aa_quality%gamma_correction = 2.2
  aa_quality%linear_filtering = .true.
  aa_quality%edge_enhancement = .true.
  
  success = ft_aa_engine_new(bitmap_size, bitmap_size, aa_quality, aa_engine, error)
  if (.not. success) then
    print *, "ERROR: Failed to create high-quality AA engine:", error
    stop
  end if
  
  ! Render with high-quality antialiasing using transformed outline
  success = ft_aa_render_outline_hq(aa_engine, transformed_outline, bitmap_aa_hinted, error)
  if (success) then
    print *, "   SUCCESS: High-quality AA + hinted rendering complete"
  else
    print *, "   ERROR: High-quality rendering failed:", error
  end if
  
  call ft_aa_engine_done(aa_engine)
  
  ! Save results
  print *
  print *, "Saving results..."
  
  if (ft_bitmap_write_png(bitmap_no_aa, "demo_no_aa.png", error)) then
    print *, "  Saved: demo_no_aa.png (placeholder)"
  end if
  
  if (ft_bitmap_write_png(bitmap_aa, "demo_aa_normal.png", error)) then
    print *, "  Saved: demo_aa_normal.png"
  end if
  
  if (ft_bitmap_write_png(bitmap_aa_hinted, "demo_aa_hinted_hq.png", error)) then
    print *, "  Saved: demo_aa_hinted_hq.png"
  end if
  
  ! Also save as PBM for easy viewing
  if (ft_bitmap_write_pbm(bitmap_aa, "demo_aa_normal.pbm", error)) then
    print *, "  Saved: demo_aa_normal.pbm"
  end if
  
  if (ft_bitmap_write_pbm(bitmap_aa_hinted, "demo_aa_hinted_hq.pbm", error)) then
    print *, "  Saved: demo_aa_hinted_hq.pbm"
  end if
  
  print *
  print *, "Demonstration complete!"
  print *, "Check the output files to see the differences between:"
  print *, "- No antialiasing (placeholder)"
  print *, "- Normal antialiasing"
  print *, "- High-quality antialiasing with hinting and edge enhancement"
  
  ! Cleanup
  call ft_bitmap_done(bitmap_no_aa)
  call ft_bitmap_done(bitmap_aa)
  call ft_bitmap_done(bitmap_aa_hinted)
  
  if (associated(outline%points)) deallocate(outline%points)
  if (associated(outline%tags)) deallocate(outline%tags)
  if (associated(outline%contours)) deallocate(outline%contours)
  
  if (associated(transformed_outline%points)) deallocate(transformed_outline%points)
  if (associated(transformed_outline%tags)) deallocate(transformed_outline%tags)
  if (associated(transformed_outline%contours)) deallocate(transformed_outline%contours)
  
  call ft_done_unified_face(face)
  
end program demo_font_rendering