program debug_coordinate_mismatch
  use ft_face_unified
  use ft_types
  use ft_outline_mod
  use ft_antialiasing_enhanced
  use ft_bitmap_mod
  implicit none
  
  type(FT_Unified_Face) :: face
  type(FT_Outline) :: outline
  type(FT_Bitmap) :: bitmap
  type(FT_AA_Engine) :: aa_engine
  type(FT_AA_Quality) :: aa_quality
  integer(FT_Error) :: error
  logical :: success
  integer :: glyph_index
  character(len=256) :: font_file
  integer :: bitmap_size = 64
  
  print *, "DEBUG: Coordinate Mismatch Analysis"
  print *, "=================================="
  print *, ""
  
  font_file = "/usr/share/fonts/TTF/DejaVuSans.ttf"
  
  ! Load font and glyph
  success = ft_new_unified_face(font_file, 0, face, error)
  if (.not. success) stop "Failed to load face"
  
  glyph_index = ft_unified_get_glyph_index(face, iachar('A'))
  success = ft_unified_load_glyph(face, glyph_index, outline, error)
  if (.not. success) stop "Failed to load glyph"
  
  print *, "Outline coordinate ranges:"
  print *, "  X: [", outline%points(1)%x, " to ??? ]"
  print *, "  Y: [", outline%points(1)%y, " to ??? ]"
  
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
    
    print *, "  Actual X: [", min_x, " to ", max_x, "]"
    print *, "  Actual Y: [", min_y, " to ", max_y, "]"
    print *, ""
    print *, "Bitmap sampling ranges:"
    print *, "  X: [0 to ", bitmap_size-1, "]"
    print *, "  Y: [0 to ", bitmap_size-1, "]"
    print *, ""
    print *, "THE PROBLEM:"
    print *, "  Font coordinates are 10-20x larger than bitmap coordinates!"
    print *, "  No pixel samples will be inside the outline."
    print *, ""
    print *, "SOLUTION:"
    print *, "  Transform outline coordinates to bitmap space before rendering."
    print *, "  Scale factor needed: ~", real(bitmap_size) / real(max(max_x - min_x, max_y - min_y))
  end block
  
  ! Test the renderer with current coordinates
  print *, ""
  print *, "Testing current renderer (should produce empty bitmap):"
  
  success = ft_bitmap_new(bitmap_size, bitmap_size, FT_PIXEL_MODE_GRAY, bitmap, error)
  if (.not. success) stop "Failed to create bitmap"
  
  aa_quality%level = FT_AA_QUALITY_NORMAL
  aa_quality%samples_per_pixel = 4
  
  success = ft_aa_engine_new(bitmap_size, bitmap_size, aa_quality, aa_engine, error)
  if (.not. success) stop "Failed to create AA engine"
  
  success = ft_aa_render_outline(aa_engine, outline, bitmap, error)
  
  block
    integer :: i, non_zero_pixels = 0
    do i = 1, size(bitmap%buffer)
      if (bitmap%buffer(i) > 0) non_zero_pixels = non_zero_pixels + 1
    end do
    
    print *, "  Non-zero pixels:", non_zero_pixels, "/", size(bitmap%buffer)
    if (non_zero_pixels == 0) then
      print *, "  CONFIRMED: No pixels rendered (coordinate mismatch)"
    else
      print *, "  UNEXPECTED: Some pixels were rendered"
    end if
  end block
  
  ! Clean up
  call ft_aa_engine_done(aa_engine)
  call ft_bitmap_done(bitmap)
  if (associated(outline%points)) deallocate(outline%points)
  if (associated(outline%tags)) deallocate(outline%tags)
  if (associated(outline%contours)) deallocate(outline%contours)
  call ft_done_unified_face(face)
  
  print *, ""
  print *, "Analysis complete!"
  
end program debug_coordinate_mismatch