program test_coverage_to_bitmap_direct
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
  integer :: i, non_zero_pixels
  
  print *, "TEST: Direct Coverage to Bitmap Function Call"
  print *, "============================================"
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
  
  ! First fill coverage buffer by rendering outline
  success = render_normal_aa(aa_engine, transformed_outline, error)
  if (.not. success) stop "Failed to render coverage"
  
  print *, "Coverage buffer filled. Now testing direct coverage_to_bitmap call..."
  
  ! Clear bitmap
  call ft_bitmap_clear(bitmap)
  
  ! Count non-zero coverage first
  block
    integer :: j, non_zero_coverage = 0
    do i = 1, size(aa_engine%coverage_buffer, 1)
      do j = 1, size(aa_engine%coverage_buffer, 2)
        if (aa_engine%coverage_buffer(i,j) > 0.0) then
          non_zero_coverage = non_zero_coverage + 1
        end if
      end do
    end do
    print *, "  Non-zero coverage entries:", non_zero_coverage
  end block
  
  ! Call coverage_to_bitmap directly
  success = coverage_to_bitmap(aa_engine, bitmap, error)
  print *, "Direct coverage_to_bitmap call:"
  print *, "  Success:", success
  print *, "  Error:", error
  
  ! Count final pixels
  non_zero_pixels = 0
  do i = 1, size(bitmap%buffer)
    if (bitmap%buffer(i) /= 0) non_zero_pixels = non_zero_pixels + 1
  end do
  
  print *, "  Non-zero pixels:", non_zero_pixels, "/", size(bitmap%buffer)
  
  if (non_zero_pixels > 0) then
    print *, "  ✓ SUCCESS: Direct function call works!"
  else
    print *, "  ✗ FAILURE: Direct function call failed"
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
  call ft_done_unified_face(face)
  
  print *, ""
  print *, "Test complete!"
  
end program test_coverage_to_bitmap_direct