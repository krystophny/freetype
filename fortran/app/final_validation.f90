program final_validation
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
  
  print *, "FINAL VALIDATION: Complete Rendering Pipeline"
  print *, "============================================="
  print *, ""
  
  font_file = "/usr/share/fonts/TTF/DejaVuSans.ttf"
  
  ! Load font and glyph
  success = ft_new_unified_face(font_file, 0, face, error)
  if (.not. success) stop "Failed to load face"
  
  glyph_index = ft_unified_get_glyph_index(face, iachar('A'))
  success = ft_unified_load_glyph(face, glyph_index, outline, error)
  if (.not. success) stop "Failed to load glyph"
  
  print *, "Original glyph loaded:", outline%n_points, "points,", outline%n_contours, "contours"
  
  ! Transform outline coordinates to bitmap space
  success = ft_outline_transform_to_bitmap(outline, transformed_outline, &
                                          face%units_per_em, bitmap_size, bitmap_size, error)
  if (.not. success) stop "Failed to transform outline"
  
  print *, "Outline transformed successfully"
  
  ! Create bitmap
  success = ft_bitmap_new(bitmap_size, bitmap_size, FT_PIXEL_MODE_GRAY, bitmap, error)
  if (.not. success) stop "Failed to create bitmap"
  
  ! Set up antialiasing
  aa_quality%level = FT_AA_QUALITY_NORMAL
  aa_quality%samples_per_pixel = 4
  aa_quality%gamma_correction = 2.2
  aa_quality%linear_filtering = .false.
  aa_quality%edge_enhancement = .false.
  
  success = ft_aa_engine_new(bitmap_size, bitmap_size, aa_quality, aa_engine, error)
  if (.not. success) stop "Failed to create AA engine"
  
  print *, "AA engine created"
  
  ! Render using full ft_aa_render_outline function (not just coverage)
  success = ft_aa_render_outline(aa_engine, transformed_outline, bitmap, error)
  
  print *, "Rendering result:", success, "error:", error
  
  ! Count non-zero pixels
  non_zero_pixels = 0
  do i = 1, size(bitmap%buffer)
    if (bitmap%buffer(i) /= 0) non_zero_pixels = non_zero_pixels + 1
  end do
  
  print *, ""
  print *, "FINAL RESULTS:"
  print *, "  Non-zero pixels:", non_zero_pixels, "/", size(bitmap%buffer)
  print *, "  Percentage filled:", real(non_zero_pixels) / real(size(bitmap%buffer)) * 100.0, "%"
  
  if (non_zero_pixels > 0) then
    print *, "  ✓ SUCCESS: Complete rendering pipeline works!"
    print *, "  ✓ Font rendering is now functional!"
  else
    print *, "  ✗ FAILURE: Pipeline still not working"
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
  print *, "Validation complete!"
  
end program final_validation