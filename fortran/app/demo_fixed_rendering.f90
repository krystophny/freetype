program demo_fixed_rendering
  use ft_face_unified
  use ft_types
  use ft_bitmap_mod
  use ft_outline_mod
  use ft_antialiasing_enhanced
  use ft_hint
  use ft_bitmap_io
  use ft_geometry, only: FT_Vector
  implicit none
  
  type(FT_Unified_Face) :: face
  type(FT_Outline) :: outline, transformed_outline
  type(FT_Bitmap) :: bitmap_aa_normal, bitmap_aa_hinted
  type(FT_AA_Engine) :: aa_engine
  type(FT_AA_Quality) :: aa_quality
  type(FT_Hint_Engine) :: hint_engine
  integer(FT_Error) :: error
  logical :: success
  integer :: glyph_index
  character(len=256) :: font_file
  character :: test_char = 'A'
  integer :: bitmap_size = 64
  
  print *, "FIXED Font Rendering Demonstration"
  print *, "=================================="
  print *, "This version properly transforms glyph coordinates"
  print *, ""
  
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
  print *, "  Units per EM:", face%units_per_em
  print *, ""
  
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
  
  print *, "Original glyph loaded:"
  print *, "  Points:", outline%n_points
  print *, "  Contours:", outline%n_contours
  
  ! Show original coordinate range
  block
    integer :: i, min_x, max_x, min_y, max_y
    
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
    
    print *, "  Coordinate range: (", min_x, ",", min_y, ") to (", max_x, ",", max_y, ")"
  end block
  print *, ""
  
  ! Transform glyph coordinates to fit bitmap
  call transform_glyph_to_bitmap(outline, transformed_outline, face%units_per_em, bitmap_size)
  
  print *, "Transformed glyph:"
  print *, "  Points:", transformed_outline%n_points
  
  ! Show transformed coordinate range
  block
    integer :: i, min_x, max_x, min_y, max_y
    
    min_x = transformed_outline%points(1)%x
    max_x = transformed_outline%points(1)%x
    min_y = transformed_outline%points(1)%y
    max_y = transformed_outline%points(1)%y
    
    do i = 2, transformed_outline%n_points
      min_x = min(min_x, transformed_outline%points(i)%x)
      max_x = max(max_x, transformed_outline%points(i)%x)
      min_y = min(min_y, transformed_outline%points(i)%y)
      max_y = max(max_y, transformed_outline%points(i)%y)
    end do
    
    print *, "  Coordinate range: (", min_x, ",", min_y, ") to (", max_x, ",", max_y, ")"
  end block
  print *, ""
  
  ! Create bitmaps
  success = ft_bitmap_new(bitmap_size, bitmap_size, FT_PIXEL_MODE_GRAY, bitmap_aa_normal, error)
  if (.not. success) stop "Failed to create bitmap"
  
  success = ft_bitmap_new(bitmap_size, bitmap_size, FT_PIXEL_MODE_GRAY, bitmap_aa_hinted, error)
  if (.not. success) stop "Failed to create bitmap"
  
  ! Clear bitmaps
  call ft_bitmap_clear(bitmap_aa_normal)
  call ft_bitmap_clear(bitmap_aa_hinted)
  
  ! 1. Render with normal antialiasing
  print *, "Rendering with normal antialiasing..."
  
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
  
  ! Render with antialiasing
  success = ft_aa_render_outline(aa_engine, transformed_outline, bitmap_aa_normal, error)
  if (success) then
    print *, "  SUCCESS: Normal antialiasing complete"
  else
    print *, "  ERROR: Normal antialiasing failed:", error
  end if
  
  call ft_aa_engine_done(aa_engine)
  
  ! 2. Render with hinting + high-quality antialiasing
  print *, ""
  print *, "Rendering with hinting + high-quality antialiasing..."
  
  ! Set up hinting engine
  success = ft_hint_engine_new(face%units_per_em, real(bitmap_size), hint_engine, error)
  if (success) then
    print *, "  Hinting engine created"
    
    ! Apply hinting to transformed outline
    success = ft_hint_apply_outline(hint_engine, transformed_outline, error)
    if (success) then
      print *, "  Hinting applied successfully"
    else
      print *, "  WARNING: Hinting failed:", error
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
  
  ! Render with high-quality antialiasing
  success = ft_aa_render_outline_hq(aa_engine, transformed_outline, bitmap_aa_hinted, error)
  if (success) then
    print *, "  SUCCESS: High-quality AA + hinting complete"
  else
    print *, "  ERROR: High-quality rendering failed:", error
  end if
  
  call ft_aa_engine_done(aa_engine)
  
  ! Save results
  print *, ""
  print *, "Saving results..."
  
  if (ft_bitmap_write_png(bitmap_aa_normal, "fixed_aa_normal.png", error)) then
    print *, "  Saved: fixed_aa_normal.png"
  end if
  
  if (ft_bitmap_write_png(bitmap_aa_hinted, "fixed_aa_hinted.png", error)) then
    print *, "  Saved: fixed_aa_hinted.png"
  end if
  
  ! Check if we actually rendered something
  block
    integer :: i, pixel_count = 0
    
    do i = 1, size(bitmap_aa_normal%buffer)
      if (bitmap_aa_normal%buffer(i) > 0) pixel_count = pixel_count + 1
    end do
    
    print *, ""
    print *, "Quality check - normal AA bitmap:"
    print *, "  Non-zero pixels:", pixel_count, "/", size(bitmap_aa_normal%buffer)
    
    pixel_count = 0
    do i = 1, size(bitmap_aa_hinted%buffer)
      if (bitmap_aa_hinted%buffer(i) > 0) pixel_count = pixel_count + 1
    end do
    
    print *, "Quality check - hinted AA bitmap:"
    print *, "  Non-zero pixels:", pixel_count, "/", size(bitmap_aa_hinted%buffer)
  end block
  
  print *, ""
  print *, "FIXED rendering complete!"
  print *, "If the pixel counts above are > 0, the rendering worked!"
  
  ! Cleanup
  call ft_bitmap_done(bitmap_aa_normal)
  call ft_bitmap_done(bitmap_aa_hinted)
  
  if (associated(outline%points)) deallocate(outline%points)
  if (associated(outline%tags)) deallocate(outline%tags)
  if (associated(outline%contours)) deallocate(outline%contours)
  
  if (associated(transformed_outline%points)) deallocate(transformed_outline%points)
  if (associated(transformed_outline%tags)) deallocate(transformed_outline%tags)
  if (associated(transformed_outline%contours)) deallocate(transformed_outline%contours)
  
  call ft_done_unified_face(face)

contains

  ! Transform glyph coordinates from font units to bitmap coordinates
  subroutine transform_glyph_to_bitmap(input_outline, output_outline, units_per_em, target_size)
    use ft_types, only: FT_Fixed
    type(FT_Outline), intent(in) :: input_outline
    type(FT_Outline), intent(out) :: output_outline
    integer, intent(in) :: units_per_em, target_size
    
    real :: scale_factor
    integer :: i, margin
    integer(FT_Fixed) :: min_x, max_x, min_y, max_y
    integer(FT_Fixed) :: glyph_width, glyph_height
    real :: center_x, center_y
    integer(FT_Fixed) :: offset_x, offset_y
    integer(FT_Error) :: error
    
    ! Calculate scale factor
    scale_factor = real(target_size) * 0.8 / real(units_per_em)  ! Use 80% of bitmap for safety
    
    ! Find bounding box of original outline
    min_x = input_outline%points(1)%x
    max_x = input_outline%points(1)%x
    min_y = input_outline%points(1)%y
    max_y = input_outline%points(1)%y
    
    do i = 2, input_outline%n_points
      min_x = min(min_x, input_outline%points(i)%x)
      max_x = max(max_x, input_outline%points(i)%x)
      min_y = min(min_y, input_outline%points(i)%y)
      max_y = max(max_y, input_outline%points(i)%y)
    end do
    
    ! Calculate dimensions and centering
    glyph_width = max_x - min_x
    glyph_height = max_y - min_y
    
    ! Calculate offset to center the glyph
    center_x = real(target_size) / 2.0
    center_y = real(target_size) / 2.0
    
    offset_x = int(center_x - (real(min_x + max_x) / 2.0) * scale_factor, FT_Fixed)
    offset_y = int(center_y - (real(min_y + max_y) / 2.0) * scale_factor, FT_Fixed)
    
    ! Create output outline
    if (.not. ft_outline_new(int(input_outline%n_points, 4), int(input_outline%n_contours, 4), output_outline, error)) then
      return
    end if
    
    output_outline%n_points = input_outline%n_points
    output_outline%n_contours = input_outline%n_contours
    
    ! Transform points
    do i = 1, input_outline%n_points
      ! Scale and translate coordinates
      output_outline%points(i)%x = int(real(input_outline%points(i)%x) * scale_factor, FT_Fixed) + offset_x
      ! Flip Y coordinate (font Y increases up, bitmap Y increases down)
      output_outline%points(i)%y = int(target_size, FT_Fixed) - (int(real(input_outline%points(i)%y) * scale_factor, FT_Fixed) + offset_y)
      
      ! Copy tags
      output_outline%tags(i) = input_outline%tags(i)
    end do
    
    ! Copy contours
    do i = 1, input_outline%n_contours
      output_outline%contours(i) = input_outline%contours(i)
    end do
    
  end subroutine transform_glyph_to_bitmap

end program demo_fixed_rendering