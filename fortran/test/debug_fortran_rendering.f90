program debug_fortran_rendering
  use ft_face_unified
  use ft_types
  use ft_bitmap_mod
  use ft_outline_mod
  use ft_antialiasing_enhanced
  use ft_bitmap_io
  use ft_geometry, only: FT_Vector
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
  character :: test_char = 'A'
  integer :: bitmap_size = 64
  integer :: i
  
  print *, "DEBUG: Fortran Font Rendering Step-by-Step"
  print *, "=========================================="
  print *, ""
  
  font_file = "/usr/share/fonts/TTF/DejaVuSans.ttf"
  
  ! Load font
  print *, "Step 1: Loading font..."
  success = ft_new_unified_face(font_file, 0, face, error)
  if (.not. success) then
    print *, "FAILED:", error
    stop 1
  end if
  print *, "SUCCESS: Font loaded, units_per_em =", face%units_per_em
  
  ! Get glyph
  print *, ""
  print *, "Step 2: Getting glyph for '", test_char, "'..."
  glyph_index = ft_unified_get_glyph_index(face, iachar(test_char))
  print *, "Glyph index:", glyph_index
  
  success = ft_unified_load_glyph(face, glyph_index, outline, error)
  if (.not. success) then
    print *, "FAILED:", error
    stop 1
  end if
  print *, "SUCCESS: Glyph loaded"
  print *, "  Points:", outline%n_points
  print *, "  Contours:", outline%n_contours
  
  ! Show first few points
  print *, "  First 5 points:"
  do i = 1, min(5, outline%n_points)
    print *, "    Point", i, ":", outline%points(i)%x, outline%points(i)%y, "tag:", outline%tags(i)
  end do
  
  ! Show contour endpoints
  if (outline%n_contours > 0) then
    print *, "  Contour endpoints:"
    do i = 1, outline%n_contours
      print *, "    Contour", i, "ends at point:", outline%contours(i)
    end do
  end if
  
  ! Transform coordinates
  print *, ""
  print *, "Step 3: Transforming coordinates..."
  call transform_glyph_to_bitmap(outline, transformed_outline, face%units_per_em, bitmap_size)
  
  print *, "Transformed glyph:"
  print *, "  Points:", transformed_outline%n_points
  print *, "  First 5 transformed points:"
  do i = 1, min(5, transformed_outline%n_points)
    print *, "    Point", i, ":", transformed_outline%points(i)%x, transformed_outline%points(i)%y
  end do
  
  ! Create bitmap
  print *, ""
  print *, "Step 4: Creating bitmap..."
  success = ft_bitmap_new(bitmap_size, bitmap_size, FT_PIXEL_MODE_GRAY, bitmap, error)
  if (.not. success) then
    print *, "FAILED:", error
    stop 1
  end if
  call ft_bitmap_clear(bitmap)
  print *, "SUCCESS: Bitmap created (", bitmap_size, "x", bitmap_size, ")"
  
  ! Set up antialiasing
  print *, ""
  print *, "Step 5: Setting up antialiasing..."
  aa_quality%level = FT_AA_QUALITY_NORMAL
  aa_quality%samples_per_pixel = 4
  aa_quality%gamma_correction = 2.2
  aa_quality%linear_filtering = .false.
  aa_quality%edge_enhancement = .false.
  
  success = ft_aa_engine_new(bitmap_size, bitmap_size, aa_quality, aa_engine, error)
  if (.not. success) then
    print *, "FAILED:", error
    stop 1
  end if
  print *, "SUCCESS: AA engine created"
  
  ! Render
  print *, ""
  print *, "Step 6: Rendering..."
  success = ft_aa_render_outline(aa_engine, transformed_outline, bitmap, error)
  if (.not. success) then
    print *, "FAILED:", error
    call ft_aa_engine_done(aa_engine)
    stop 1
  end if
  print *, "SUCCESS: Rendering completed"
  
  ! Analyze result
  print *, ""
  print *, "Step 7: Analyzing result..."
  block
    integer :: pixel_count = 0, max_val = 0, min_val = 255
    
    do i = 1, size(bitmap%buffer)
      if (bitmap%buffer(i) > 0) then
        pixel_count = pixel_count + 1
        max_val = max(max_val, int(bitmap%buffer(i)))
        min_val = min(min_val, int(bitmap%buffer(i)))
      end if
    end do
    
    print *, "  Non-zero pixels:", pixel_count, "/", size(bitmap%buffer)
    if (pixel_count > 0) then
      print *, "  Pixel value range:", min_val, "to", max_val
      print *, "  SUCCESS: Rendering produced visible output!"
    else
      print *, "  ERROR: No visible pixels rendered!"
      print *, ""
      print *, "Let's check the coordinate bounds:"
      
      block
        integer :: min_x, max_x, min_y, max_y, j
        
        min_x = transformed_outline%points(1)%x
        max_x = transformed_outline%points(1)%x
        min_y = transformed_outline%points(1)%y
        max_y = transformed_outline%points(1)%y
        
        do j = 2, transformed_outline%n_points
          min_x = min(min_x, transformed_outline%points(j)%x)
          max_x = max(max_x, transformed_outline%points(j)%x)
          min_y = min(min_y, transformed_outline%points(j)%y)
          max_y = max(max_y, transformed_outline%points(j)%y)
        end do
        
        print *, "  Transformed coordinate bounds:"
        print *, "    X: ", min_x, "to", max_x
        print *, "    Y: ", min_y, "to", max_y
        print *, "    Bitmap size: 0 to", bitmap_size-1
        
        if (min_x >= 0 .and. max_x < bitmap_size .and. min_y >= 0 .and. max_y < bitmap_size) then
          print *, "    Coordinates are within bitmap bounds"
        else
          print *, "    ERROR: Coordinates are outside bitmap bounds!"
        end if
      end block
    end if
  end block
  
  ! Save result
  print *, ""
  print *, "Step 8: Saving result..."
  success = ft_bitmap_write_png(bitmap, "debug_fortran_render.png", error)
  if (success) then
    print *, "SUCCESS: Saved debug_fortran_render.png"
  else
    print *, "FAILED to save PNG"
  end if
  
  ! Simple ASCII preview
  print *, ""
  print *, "Step 9: ASCII preview (first 16x16):"
  do i = 0, min(15, bitmap%rows - 1)
    write(*, '("  ")', advance='no')
    block
      integer :: j, pixel_idx
      character :: c
      
      do j = 0, min(15, bitmap%width - 1)
        pixel_idx = i * bitmap%width + j + 1
        if (pixel_idx <= size(bitmap%buffer)) then
          if (bitmap%buffer(pixel_idx) > 128) then
            c = '#'
          else if (bitmap%buffer(pixel_idx) > 64) then
            c = '*'
          else if (bitmap%buffer(pixel_idx) > 0) then
            c = '.'
          else
            c = ' '
          end if
          write(*, '(A)', advance='no') c
        end if
      end do
    end block
    write(*, *)
  end do
  
  ! Cleanup
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
  print *, "DEBUG COMPLETE!"

contains

  ! Transform glyph coordinates from font units to bitmap coordinates
  subroutine transform_glyph_to_bitmap(input_outline, output_outline, units_per_em, target_size)
    use ft_types, only: FT_Fixed
    type(FT_Outline), intent(in) :: input_outline
    type(FT_Outline), intent(out) :: output_outline
    integer, intent(in) :: units_per_em, target_size
    
    real :: scale_factor
    integer :: i
    integer(FT_Fixed) :: min_x, max_x, min_y, max_y
    real :: center_x, center_y
    integer(FT_Fixed) :: offset_x, offset_y
    integer(FT_Error) :: error
    
    ! Calculate scale factor for ~80% of bitmap
    scale_factor = real(target_size) * 0.6 / real(units_per_em)
    
    ! Find bounding box
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
    
    ! Calculate centering offset
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
      output_outline%points(i)%x = int(real(input_outline%points(i)%x) * scale_factor, FT_Fixed) + offset_x
      ! Flip Y coordinate (font Y increases up, bitmap Y increases down)
      output_outline%points(i)%y = int(target_size, FT_Fixed) - (int(real(input_outline%points(i)%y) * scale_factor, FT_Fixed) + offset_y)
      output_outline%tags(i) = input_outline%tags(i)
    end do
    
    ! Copy contours
    do i = 1, input_outline%n_contours
      output_outline%contours(i) = input_outline%contours(i)
    end do
    
  end subroutine transform_glyph_to_bitmap

end program debug_fortran_rendering