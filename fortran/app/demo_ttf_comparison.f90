program demo_ttf_comparison
  use ft_face_unified
  use ft_types
  use ft_bitmap_mod
  use ft_outline_mod
  use ft_antialiasing_enhanced
  use ft_hint
  use ft_bitmap_io
  use ft_compare
  use ft_geometry, only: FT_Vector
  implicit none
  
  type(FT_Unified_Face) :: face
  type(FT_Outline) :: outline
  type(FT_Bitmap) :: bitmap_no_aa, bitmap_aa_normal, bitmap_aa_hinted
  type(FT_Bitmap) :: atlas_no_aa, atlas_aa_normal, atlas_aa_hinted
  type(FT_AA_Engine) :: aa_engine
  type(FT_AA_Quality) :: aa_quality
  type(FT_Hint_Engine) :: hint_engine
  integer(FT_Error) :: error
  logical :: success
  integer :: glyph_index
  character(len=256) :: font_file
  character(len=26) :: test_string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  integer :: i, char_idx
  integer :: letter_size = 48
  integer :: atlas_cols = 6
  integer :: atlas_rows = 5
  integer :: atlas_width, atlas_height
  integer :: x_pos, y_pos
  
  print *, "TrueType Font Rendering Comparison Demo"
  print *, "======================================="
  print *, ""
  print *, "This demo shows the same TrueType font rendered with:"
  print *, "1. No antialiasing"
  print *, "2. Normal antialiasing"
  print *, "3. High-quality antialiasing with hinting"
  print *, ""
  
  ! Use DejaVu Sans as test font
  font_file = "/usr/share/fonts/TTF/DejaVuSans.ttf"
  
  ! Load font
  print *, "Loading font:", trim(font_file)
  success = ft_new_unified_face(font_file, 0, face, error)
  if (.not. success) then
    print *, "ERROR: Failed to load font:", error
    print *, "Make sure DejaVu Sans is installed or specify a different font"
    stop
  end if
  
  print *, "Font loaded successfully!"
  print *, "  Format:", face%font_format
  print *, "  Glyphs:", face%num_glyphs
  print *, "  Units per EM:", face%units_per_em
  print *, ""
  
  ! Calculate atlas dimensions
  atlas_width = letter_size * atlas_cols
  atlas_height = letter_size * atlas_rows
  
  ! Create atlas bitmaps for each rendering mode
  success = ft_bitmap_new(atlas_width, atlas_height, FT_PIXEL_MODE_GRAY, atlas_no_aa, error)
  if (.not. success) stop "Failed to create no-AA atlas"
  
  success = ft_bitmap_new(atlas_width, atlas_height, FT_PIXEL_MODE_GRAY, atlas_aa_normal, error)
  if (.not. success) stop "Failed to create AA atlas"
  
  success = ft_bitmap_new(atlas_width, atlas_height, FT_PIXEL_MODE_GRAY, atlas_aa_hinted, error)
  if (.not. success) stop "Failed to create AA+hint atlas"
  
  ! Clear atlases
  call ft_bitmap_clear(atlas_no_aa)
  call ft_bitmap_clear(atlas_aa_normal)
  call ft_bitmap_clear(atlas_aa_hinted)
  
  ! Create individual letter bitmaps
  success = ft_bitmap_new(letter_size, letter_size, FT_PIXEL_MODE_GRAY, bitmap_no_aa, error)
  if (.not. success) stop "Failed to create bitmap"
  
  success = ft_bitmap_new(letter_size, letter_size, FT_PIXEL_MODE_GRAY, bitmap_aa_normal, error)
  if (.not. success) stop "Failed to create bitmap"
  
  success = ft_bitmap_new(letter_size, letter_size, FT_PIXEL_MODE_GRAY, bitmap_aa_hinted, error)
  if (.not. success) stop "Failed to create bitmap"
  
  ! Set up antialiasing engines
  
  ! Normal quality AA
  aa_quality%level = FT_AA_QUALITY_NORMAL
  aa_quality%samples_per_pixel = 4
  aa_quality%gamma_correction = 2.2
  aa_quality%linear_filtering = .false.
  aa_quality%edge_enhancement = .false.
  
  success = ft_aa_engine_new(letter_size, letter_size, aa_quality, aa_engine, error)
  if (.not. success) stop "Failed to create normal AA engine"
  
  ! Set up hinting engine
  success = ft_hint_engine_new(face%units_per_em, real(letter_size), hint_engine, error)
  if (.not. success) then
    print *, "WARNING: Failed to create hinting engine"
  end if
  
  print *, "Rendering alphabet..."
  print *, ""
  
  ! Render each letter
  do char_idx = 1, 26
    ! Get character and glyph index
    glyph_index = ft_unified_get_glyph_index(face, iachar(test_string(char_idx:char_idx)))
    
    if (glyph_index == 0) then
      print *, "WARNING: No glyph for character '", test_string(char_idx:char_idx), "'"
      cycle
    end if
    
    ! Load glyph outline
    success = ft_unified_load_glyph(face, glyph_index, outline, error)
    if (.not. success) then
      print *, "WARNING: Failed to load glyph for '", test_string(char_idx:char_idx), "'", error
      cycle
    end if
    
    ! Calculate position in atlas
    x_pos = mod(char_idx - 1, atlas_cols) * letter_size
    y_pos = ((char_idx - 1) / atlas_cols) * letter_size
    
    ! Clear individual bitmaps
    call ft_bitmap_clear(bitmap_no_aa)
    call ft_bitmap_clear(bitmap_aa_normal)
    call ft_bitmap_clear(bitmap_aa_hinted)
    
    ! 1. Render without antialiasing (using normal AA with 1 sample as placeholder)
    aa_quality%level = FT_AA_QUALITY_FAST
    aa_quality%samples_per_pixel = 1
    call ft_aa_engine_done(aa_engine)
    success = ft_aa_engine_new(letter_size, letter_size, aa_quality, aa_engine, error)
    if (success) then
      success = ft_aa_render_outline(aa_engine, outline, bitmap_no_aa, error)
      if (success) then
        call copy_bitmap_to_atlas(bitmap_no_aa, atlas_no_aa, x_pos, y_pos)
      end if
    end if
    
    ! 2. Render with normal antialiasing
    aa_quality%level = FT_AA_QUALITY_NORMAL
    aa_quality%samples_per_pixel = 4
    call ft_aa_engine_done(aa_engine)
    success = ft_aa_engine_new(letter_size, letter_size, aa_quality, aa_engine, error)
    if (success) then
      success = ft_aa_render_outline(aa_engine, outline, bitmap_aa_normal, error)
      if (success) then
        call copy_bitmap_to_atlas(bitmap_aa_normal, atlas_aa_normal, x_pos, y_pos)
      end if
    end if
    
    ! 3. Apply hinting and render with high-quality antialiasing
    if (hint_engine%initialized) then
      success = ft_hint_apply_outline(hint_engine, outline, error)
    end if
    
    aa_quality%level = FT_AA_QUALITY_HIGH
    aa_quality%samples_per_pixel = 8
    aa_quality%linear_filtering = .true.
    aa_quality%edge_enhancement = .true.
    
    call ft_aa_engine_done(aa_engine)
    success = ft_aa_engine_new(letter_size, letter_size, aa_quality, aa_engine, error)
    if (success) then
      success = ft_aa_render_outline_hq(aa_engine, outline, bitmap_aa_hinted, error)
      if (success) then
        call copy_bitmap_to_atlas(bitmap_aa_hinted, atlas_aa_hinted, x_pos, y_pos)
      end if
    end if
    
    ! Progress indicator
    write(*, '(".", $)')
    if (mod(char_idx, 10) == 0) write(*, '(" ", I0, "/26", $)') char_idx
    
    ! Clean up outline for next iteration
    if (associated(outline%points)) deallocate(outline%points)
    if (associated(outline%tags)) deallocate(outline%tags)
    if (associated(outline%contours)) deallocate(outline%contours)
  end do
  
  print *, ""
  print *, ""
  print *, "Saving results..."
  
  ! Save individual atlases
  if (ft_bitmap_write_png(atlas_no_aa, "ttf_alphabet_no_aa.png", error)) then
    print *, "  Saved: ttf_alphabet_no_aa.png"
  end if
  
  if (ft_bitmap_write_png(atlas_aa_normal, "ttf_alphabet_aa_normal.png", error)) then
    print *, "  Saved: ttf_alphabet_aa_normal.png"
  end if
  
  if (ft_bitmap_write_png(atlas_aa_hinted, "ttf_alphabet_aa_hinted.png", error)) then
    print *, "  Saved: ttf_alphabet_aa_hinted.png"
  end if
  
  ! Create comparison images
  print *, ""
  print *, "Creating comparison images..."
  
  ! Compare no-AA vs normal AA
  success = ft_compare_create_side_by_side(atlas_no_aa, atlas_aa_normal, &
                                         "ttf_comparison_no_aa_vs_normal.png", error)
  if (success) print *, "  Saved: ttf_comparison_no_aa_vs_normal.png"
  
  ! Compare normal AA vs hinted high-quality AA
  success = ft_compare_create_side_by_side(atlas_aa_normal, atlas_aa_hinted, &
                                         "ttf_comparison_normal_vs_hinted.png", error)
  if (success) print *, "  Saved: ttf_comparison_normal_vs_hinted.png"
  
  ! Create difference images
  success = ft_compare_create_diff_image(atlas_no_aa, atlas_aa_normal, &
                                       "ttf_diff_no_aa_vs_normal.png", error)
  if (success) print *, "  Saved: ttf_diff_no_aa_vs_normal.png"
  
  success = ft_compare_create_diff_image(atlas_aa_normal, atlas_aa_hinted, &
                                       "ttf_diff_normal_vs_hinted.png", error)
  if (success) print *, "  Saved: ttf_diff_normal_vs_hinted.png"
  
  ! Also create a sample with a common word
  print *, ""
  print *, "Creating word sample 'HELLO'..."
  call create_word_comparison(face, "HELLO")
  
  print *, ""
  print *, "Demo complete!"
  print *, ""
  print *, "Check the generated PNG files to see:"
  print *, "- ttf_alphabet_*.png: Full alphabet in each rendering mode"
  print *, "- ttf_comparison_*.png: Side-by-side comparisons"
  print *, "- ttf_diff_*.png: Pixel difference visualizations"
  print *, "- ttf_word_*.png: The word 'HELLO' in different modes"
  
  ! Cleanup
  call ft_aa_engine_done(aa_engine)
  if (hint_engine%initialized) call ft_hint_engine_done(hint_engine)
  
  call ft_bitmap_done(bitmap_no_aa)
  call ft_bitmap_done(bitmap_aa_normal)
  call ft_bitmap_done(bitmap_aa_hinted)
  
  call ft_bitmap_done(atlas_no_aa)
  call ft_bitmap_done(atlas_aa_normal)
  call ft_bitmap_done(atlas_aa_hinted)
  
  call ft_done_unified_face(face)

contains

  subroutine copy_bitmap_to_atlas(src, atlas, x_off, y_off)
    type(FT_Bitmap), intent(in) :: src
    type(FT_Bitmap), intent(inout) :: atlas
    integer, intent(in) :: x_off, y_off
    
    integer :: x, y, src_idx, dst_idx
    
    do y = 0, src%rows - 1
      do x = 0, src%width - 1
        if (x_off + x < atlas%width .and. y_off + y < atlas%rows) then
          src_idx = y * abs(src%pitch) + x + 1
          dst_idx = (y_off + y) * abs(atlas%pitch) + (x_off + x) + 1
          
          if (src_idx <= size(src%buffer) .and. dst_idx <= size(atlas%buffer)) then
            atlas%buffer(dst_idx) = src%buffer(src_idx)
          end if
        end if
      end do
    end do
    
  end subroutine copy_bitmap_to_atlas
  
  subroutine create_word_comparison(face, word)
    type(FT_Unified_Face), intent(inout) :: face
    character(len=*), intent(in) :: word
    
    type(FT_Bitmap) :: word_no_aa, word_aa_normal, word_aa_hinted
    type(FT_Outline) :: outline
    type(FT_AA_Engine) :: aa_eng
    type(FT_AA_Quality) :: aa_qual
    type(FT_Hint_Engine) :: hint_eng
    integer :: word_width, word_height
    integer :: i, glyph_index, x_offset
    logical :: success
    integer(FT_Error) :: err
    
    word_width = letter_size * len(word)
    word_height = letter_size
    
    ! Create word bitmaps
    success = ft_bitmap_new(word_width, word_height, FT_PIXEL_MODE_GRAY, word_no_aa, err)
    if (.not. success) return
    
    success = ft_bitmap_new(word_width, word_height, FT_PIXEL_MODE_GRAY, word_aa_normal, err)
    if (.not. success) then
      call ft_bitmap_done(word_no_aa)
      return
    end if
    
    success = ft_bitmap_new(word_width, word_height, FT_PIXEL_MODE_GRAY, word_aa_hinted, err)
    if (.not. success) then
      call ft_bitmap_done(word_no_aa)
      call ft_bitmap_done(word_aa_normal)
      return
    end if
    
    call ft_bitmap_clear(word_no_aa)
    call ft_bitmap_clear(word_aa_normal)
    call ft_bitmap_clear(word_aa_hinted)
    
    ! Set up engines
    aa_qual%level = FT_AA_QUALITY_NORMAL
    aa_qual%samples_per_pixel = 4
    aa_qual%gamma_correction = 2.2
    aa_qual%linear_filtering = .false.
    aa_qual%edge_enhancement = .false.
    
    success = ft_aa_engine_new(letter_size, letter_size, aa_qual, aa_eng, err)
    if (.not. success) goto 999
    
    success = ft_hint_engine_new(face%units_per_em, real(letter_size), hint_eng, err)
    
    ! Render each letter of the word
    x_offset = 0
    do i = 1, len(word)
      glyph_index = ft_unified_get_glyph_index(face, iachar(word(i:i)))
      if (glyph_index == 0) cycle
      
      success = ft_unified_load_glyph(face, glyph_index, outline, err)
      if (.not. success) cycle
      
      ! Create temporary bitmap
      block
        type(FT_Bitmap) :: temp_bitmap
        
        success = ft_bitmap_new(letter_size, letter_size, FT_PIXEL_MODE_GRAY, temp_bitmap, err)
        if (success) then
          ! Render and copy each variant
          call ft_bitmap_clear(temp_bitmap)
          
          ! No AA
          aa_qual%samples_per_pixel = 1
          call ft_aa_engine_done(aa_eng)
          success = ft_aa_engine_new(letter_size, letter_size, aa_qual, aa_eng, err)
          if (success) then
            success = ft_aa_render_outline(aa_eng, outline, temp_bitmap, err)
            if (success) call copy_bitmap_to_atlas(temp_bitmap, word_no_aa, x_offset, 0)
          end if
          
          ! Normal AA
          aa_qual%samples_per_pixel = 4
          call ft_aa_engine_done(aa_eng)
          success = ft_aa_engine_new(letter_size, letter_size, aa_qual, aa_eng, err)
          if (success) then
            call ft_bitmap_clear(temp_bitmap)
            success = ft_aa_render_outline(aa_eng, outline, temp_bitmap, err)
            if (success) call copy_bitmap_to_atlas(temp_bitmap, word_aa_normal, x_offset, 0)
          end if
          
          ! Hinted + High quality AA
          if (hint_eng%initialized) then
            success = ft_hint_apply_outline(hint_eng, outline, err)
          end if
          
          aa_qual%level = FT_AA_QUALITY_HIGH
          aa_qual%samples_per_pixel = 8
          aa_qual%linear_filtering = .true.
          aa_qual%edge_enhancement = .true.
          
          call ft_aa_engine_done(aa_eng)
          success = ft_aa_engine_new(letter_size, letter_size, aa_qual, aa_eng, err)
          if (success) then
            call ft_bitmap_clear(temp_bitmap)
            success = ft_aa_render_outline_hq(aa_eng, outline, temp_bitmap, err)
            if (success) call copy_bitmap_to_atlas(temp_bitmap, word_aa_hinted, x_offset, 0)
          end if
          
          call ft_bitmap_done(temp_bitmap)
        end if
      end block
      
      x_offset = x_offset + letter_size
      
      if (associated(outline%points)) deallocate(outline%points)
      if (associated(outline%tags)) deallocate(outline%tags)
      if (associated(outline%contours)) deallocate(outline%contours)
    end do
    
    ! Save word bitmaps
    success = ft_bitmap_write_png(word_no_aa, "ttf_word_no_aa.png", err)
    success = ft_bitmap_write_png(word_aa_normal, "ttf_word_aa_normal.png", err)
    success = ft_bitmap_write_png(word_aa_hinted, "ttf_word_aa_hinted.png", err)
    
    ! Create comparison
    success = ft_compare_create_side_by_side(word_aa_normal, word_aa_hinted, &
                                           "ttf_word_comparison.png", err)
    
999 continue
    call ft_aa_engine_done(aa_eng)
    if (hint_eng%initialized) call ft_hint_engine_done(hint_eng)
    
    call ft_bitmap_done(word_no_aa)
    call ft_bitmap_done(word_aa_normal)
    call ft_bitmap_done(word_aa_hinted)
    
  end subroutine create_word_comparison

end program demo_ttf_comparison