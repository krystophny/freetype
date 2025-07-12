program create_showcase
  use ft_face_unified
  use ft_types
  use ft_outline_mod
  use ft_outline_transform_mod
  use ft_antialiasing_enhanced
  use ft_bitmap_mod
  use ft_bitmap_io
  implicit none
  
  type(FT_Unified_Face) :: face
  type(FT_Outline) :: outline, transformed_outline
  type(FT_Bitmap) :: bitmap
  type(FT_AA_Engine) :: aa_engine
  type(FT_AA_Quality) :: aa_quality
  integer(FT_Error) :: error
  logical :: success
  integer :: glyph_index
  character(len=256) :: font_file
  integer :: bitmap_size = 64
  character :: letters(5) = ['A', 'B', 'C', 'X', 'Y']
  integer :: i
  character(len=50) :: filename
  
  print *, "Creating Font Rendering Showcase"
  print *, "==============================="
  print *, ""
  
  font_file = "/usr/share/fonts/TTF/DejaVuSans.ttf"
  
  ! Load font
  success = ft_new_unified_face(font_file, 0, face, error)
  if (.not. success) stop "Failed to load face"
  
  print *, "Font loaded successfully!"
  print *, "Generating showcase for letters:", letters
  print *, ""
  
  ! Set up high-quality antialiasing
  aa_quality%level = FT_AA_QUALITY_HIGH
  aa_quality%samples_per_pixel = 8
  aa_quality%gamma_correction = 2.2
  aa_quality%linear_filtering = .true.
  aa_quality%edge_enhancement = .true.
  
  ! Create bitmaps for each letter
  do i = 1, 5
    print *, "Rendering letter '", letters(i), "'..."
    
    ! Get glyph
    glyph_index = ft_unified_get_glyph_index(face, iachar(letters(i)))
    if (glyph_index == 0) then
      print *, "  WARNING: Glyph not found for '", letters(i), "'"
      cycle
    end if
    
    ! Load glyph outline
    success = ft_unified_load_glyph(face, glyph_index, outline, error)
    if (.not. success) then
      print *, "  ERROR: Failed to load glyph:", error
      cycle
    end if
    
    ! Transform outline
    success = ft_outline_transform_to_bitmap(outline, transformed_outline, &
                                            face%units_per_em, bitmap_size, bitmap_size, error)
    if (.not. success) then
      print *, "  ERROR: Failed to transform outline:", error
      cycle
    end if
    
    ! Create bitmap
    success = ft_bitmap_new(bitmap_size, bitmap_size, FT_PIXEL_MODE_GRAY, bitmap, error)
    if (.not. success) then
      print *, "  ERROR: Failed to create bitmap:", error
      cycle
    end if
    
    ! Create AA engine
    success = ft_aa_engine_new(bitmap_size, bitmap_size, aa_quality, aa_engine, error)
    if (.not. success) then
      print *, "  ERROR: Failed to create AA engine:", error
      call ft_bitmap_done(bitmap)
      cycle
    end if
    
    ! Render with high-quality antialiasing
    success = ft_aa_render_outline_hq(aa_engine, transformed_outline, bitmap, error)
    if (success) then
      ! Save as PNG
      write(filename, '("showcase_", A1, "_hq.png")') letters(i)
      if (ft_bitmap_write_png(bitmap, trim(filename), error)) then
        print *, "  ✓ Saved:", trim(filename)
      else
        print *, "  ✗ Failed to save PNG:", error
      end if
      
      ! Also save as PBM for text viewing
      write(filename, '("showcase_", A1, "_hq.pbm")') letters(i)
      if (ft_bitmap_write_pbm(bitmap, trim(filename), error)) then
        print *, "  ✓ Saved:", trim(filename)
      end if
    else
      print *, "  ERROR: Rendering failed:", error
    end if
    
    ! Clean up
    call ft_aa_engine_done(aa_engine)
    call ft_bitmap_done(bitmap)
    if (associated(outline%points)) deallocate(outline%points)
    if (associated(outline%tags)) deallocate(outline%tags)
    if (associated(outline%contours)) deallocate(outline%contours)
    if (associated(transformed_outline%points)) deallocate(transformed_outline%points)
    if (associated(transformed_outline%tags)) deallocate(transformed_outline%tags)
    if (associated(transformed_outline%contours)) deallocate(transformed_outline%contours)
  end do
  
  call ft_done_unified_face(face)
  
  print *, ""
  print *, "Showcase generation complete!"
  print *, "Check the showcase_*.png and showcase_*.pbm files"
  
end program create_showcase