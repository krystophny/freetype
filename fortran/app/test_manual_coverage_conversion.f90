program test_manual_coverage_conversion
  use ft_face_unified
  use ft_types
  use ft_outline_mod
  use ft_outline_transform_mod
  use ft_antialiasing_enhanced
  use ft_bitmap_mod
  use, intrinsic :: iso_fortran_env, only: real32, int8
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
  integer :: x, y, non_zero_pixels
  real(real32) :: coverage, gamma_corrected
  integer(int8) :: pixel_value
  
  print *, "TEST: Manual Coverage to Bitmap Conversion"
  print *, "=========================================="
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
  
  ! Render with antialiasing to fill coverage buffer
  success = ft_aa_render_outline(aa_engine, transformed_outline, bitmap, error)
  print *, "Antialiasing render result:", success
  
  ! Clear bitmap again and manually convert coverage
  call ft_bitmap_clear(bitmap)
  print *, "Bitmap cleared. Starting manual conversion..."
  
  ! Manual conversion from coverage to bitmap
  do y = 1, min(aa_engine%height, bitmap%rows)
    do x = 1, min(aa_engine%width, bitmap%width)
      coverage = aa_engine%coverage_buffer(x, y)
      
      if (coverage > 0.0) then
        ! Apply gamma correction
        if (aa_quality%gamma_correction > 0.0) then
          gamma_corrected = coverage ** (1.0 / aa_quality%gamma_correction)
        else
          gamma_corrected = coverage
        end if
        
        ! Convert to 8-bit pixel value  
        pixel_value = int(gamma_corrected * 255.0, int8)
        
        ! Debug first few conversions
        if (coverage > 0.8) then
          print *, "  Pixel (", x-1, ",", y-1, "): coverage=", coverage, &
                   " gamma=", gamma_corrected, " pixel=", int(pixel_value)
        end if
        
        ! Set pixel in bitmap
        call ft_bitmap_set_pixel_gray(bitmap, x-1, y-1, int(pixel_value))
      end if
    end do
  end do
  
  ! Count final pixels
  non_zero_pixels = 0
  do x = 1, size(bitmap%buffer)
    if (bitmap%buffer(x) /= 0) non_zero_pixels = non_zero_pixels + 1
  end do
  
  print *, ""
  print *, "Manual conversion results:"
  print *, "  Non-zero pixels:", non_zero_pixels, "/", size(bitmap%buffer)
  print *, "  Percentage filled:", real(non_zero_pixels) / real(size(bitmap%buffer)) * 100.0, "%"
  
  if (non_zero_pixels > 0) then
    print *, "  ✓ SUCCESS: Manual conversion works!"
  else
    print *, "  ✗ FAILURE: Manual conversion failed"
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
  
end program test_manual_coverage_conversion