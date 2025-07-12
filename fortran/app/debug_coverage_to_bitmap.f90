program debug_coverage_to_bitmap
  use ft_face_unified
  use ft_types
  use ft_outline_mod
  use ft_outline_transform_mod
  use ft_antialiasing_enhanced
  use ft_bitmap_mod
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
  integer :: i, non_zero_pixels, max_value, min_value
  
  print *, "DEBUG: Coverage to Bitmap Conversion"
  print *, "==================================="
  print *, ""
  
  font_file = "/usr/share/fonts/TTF/DejaVuSans.ttf"
  
  ! Load font and glyph
  success = ft_new_unified_face(font_file, 0, face, error)
  if (.not. success) stop "Failed to load face"
  
  glyph_index = ft_unified_get_glyph_index(face, iachar('A'))
  success = ft_unified_load_glyph(face, glyph_index, outline, error)
  if (.not. success) stop "Failed to load glyph"
  
  ! Transform outline
  success = ft_outline_transform_to_bitmap(outline, transformed_outline, &
                                          face%units_per_em, bitmap_size, bitmap_size, error)
  if (.not. success) stop "Failed to transform outline"
  
  ! Create bitmap and AA engine
  success = ft_bitmap_new(bitmap_size, bitmap_size, FT_PIXEL_MODE_GRAY, bitmap, error)
  if (.not. success) stop "Failed to create bitmap"
  
  aa_quality%level = FT_AA_QUALITY_NORMAL
  aa_quality%samples_per_pixel = 4
  aa_quality%gamma_correction = 2.2
  
  success = ft_aa_engine_new(bitmap_size, bitmap_size, aa_quality, aa_engine, error)
  if (.not. success) stop "Failed to create AA engine"
  
  ! Render with antialiasing
  success = ft_aa_render_outline(aa_engine, transformed_outline, bitmap, error)
  
  ! Analyze the coverage buffer
  print *, "Antialiasing coverage buffer analysis:"
  print *, "  Buffer size:", size(aa_engine%coverage_buffer, 1), "x", size(aa_engine%coverage_buffer, 2)
  
  block
    integer :: j, non_zero_coverage = 0
    real :: max_coverage = 0.0, min_coverage = 1.0
    real :: total_coverage = 0.0
    
    do i = 1, size(aa_engine%coverage_buffer, 1)
      do j = 1, size(aa_engine%coverage_buffer, 2)
        if (aa_engine%coverage_buffer(i,j) > 0.0) then
          non_zero_coverage = non_zero_coverage + 1
          max_coverage = max(max_coverage, aa_engine%coverage_buffer(i,j))
          min_coverage = min(min_coverage, aa_engine%coverage_buffer(i,j))
          total_coverage = total_coverage + aa_engine%coverage_buffer(i,j)
        end if
      end do
    end do
    
    print *, "  Non-zero coverage entries:", non_zero_coverage
    print *, "  Coverage range:", min_coverage, "to", max_coverage
    print *, "  Average coverage:", total_coverage / real(max(1, non_zero_coverage))
  end block
  
  ! Analyze the final bitmap
  print *, ""
  print *, "Final bitmap analysis:"
  non_zero_pixels = 0
  max_value = 0
  min_value = 255
  
  do i = 1, size(bitmap%buffer)
    if (bitmap%buffer(i) > 0) then
      non_zero_pixels = non_zero_pixels + 1
      max_value = max(max_value, int(bitmap%buffer(i)))
      min_value = min(min_value, int(bitmap%buffer(i)))
    end if
  end do
  
  print *, "  Non-zero pixels:", non_zero_pixels, "/", size(bitmap%buffer)
  print *, "  Pixel value range:", min_value, "to", max_value
  print *, "  Percentage filled:", real(non_zero_pixels) / real(size(bitmap%buffer)) * 100.0, "%"
  
  ! Show first few pixels with their values
  print *, ""
  print *, "First 10 non-zero pixels:"
  block
    integer :: count = 0
    do i = 1, size(bitmap%buffer)
      if (bitmap%buffer(i) > 0 .and. count < 10) then
        count = count + 1
        print *, "  Pixel", i-1, ":", int(bitmap%buffer(i))
      end if
    end do
    
    if (count == 0) then
      print *, "  NO NON-ZERO PIXELS FOUND!"
    end if
  end block
  
  ! Clean up
  call ft_aa_engine_done(aa_engine)
  call ft_bitmap_done(bitmap)
  if (associated(outline%points)) deallocate(outline%points)
  if (associated(outline%tags)) deallocate(outline%tags)
  if (associated(outline%contours)) deallocate(outline%contours)
  if (associated(transformed_outline%points)) deallocate(transformed_outline%points)
  if (associated(transformed_outline%tags)) deallocate(transformed_outline%tags)
  if (associated(transformed_outline%contours)) deallocate(transformed_outline%contours)
  call ft_done_unified_face(face)
  
  print *, ""
  print *, "Analysis complete!"
  
end program debug_coverage_to_bitmap