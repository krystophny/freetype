program clean_comparison_gallery
  use ft_types
  use ft_geometry
  use ft_bitmap_mod
  use ft_bitmap_io
  use ft_outline_mod
  use ft_scanline_simple
  use ft_compare
  use ft_raster
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  
  integer(FT_Error) :: error
  logical :: success
  type(FT_Bitmap) :: bitmap1, bitmap2, bitmap3, bitmap4
  type(FT_Outline), target :: outline1, outline2, outline3, outline4
  
  print '("Clean Font Comparison Gallery")'
  print '("=============================")'
  print '("")'
  
  ! Create different font styles for clean comparison
  
  ! Style 1: Serif-like 'A'
  print '("Creating serif-style A...")'
  call create_serif_a(outline1)
  success = ft_bitmap_new(48, 48, FT_PIXEL_MODE_GRAY, bitmap1, error)
  if (.not. success) stop 1
  success = ft_render_outline_filled(outline1, bitmap1, error)
  if (success) then
    success = ft_bitmap_write_png(bitmap1, "clean_serif_a.png", error)
    print '("  Saved: clean_serif_a.png")'
  end if
  
  ! Style 2: Sans-serif 'A'
  print '("Creating sans-serif A...")'
  call create_sans_a(outline2)
  success = ft_bitmap_new(48, 48, FT_PIXEL_MODE_GRAY, bitmap2, error)
  if (.not. success) stop 1
  success = ft_render_outline_filled(outline2, bitmap2, error)
  if (success) then
    success = ft_bitmap_write_png(bitmap2, "clean_sans_a.png", error)
    print '("  Saved: clean_sans_a.png")'
  end if
  
  ! Style 3: Script-like 'A'
  print '("Creating script-style A...")'
  call create_script_a(outline3)
  success = ft_bitmap_new(48, 48, FT_PIXEL_MODE_GRAY, bitmap3, error)
  if (.not. success) stop 1
  success = ft_render_outline_filled(outline3, bitmap3, error)
  if (success) then
    success = ft_bitmap_write_png(bitmap3, "clean_script_a.png", error)
    print '("  Saved: clean_script_a.png")'
  end if
  
  ! Style 4: Display 'A'
  print '("Creating display-style A...")'
  call create_display_a(outline4)
  success = ft_bitmap_new(48, 48, FT_PIXEL_MODE_GRAY, bitmap4, error)
  if (.not. success) stop 1
  success = ft_render_outline_filled(outline4, bitmap4, error)
  if (success) then
    success = ft_bitmap_write_png(bitmap4, "clean_display_a.png", error)
    print '("  Saved: clean_display_a.png")'
  end if
  
  print '("")'
  print '("Creating clean side-by-side comparisons...")'
  
  ! Create clean pairwise comparisons
  success = ft_compare_create_side_by_side(bitmap1, bitmap2, &
                                         "clean_serif_vs_sans.png", error)
  if (success) print '("  Serif vs Sans: clean_serif_vs_sans.png")'
  
  success = ft_compare_create_side_by_side(bitmap2, bitmap3, &
                                         "clean_sans_vs_script.png", error)
  if (success) print '("  Sans vs Script: clean_sans_vs_script.png")'
  
  success = ft_compare_create_side_by_side(bitmap3, bitmap4, &
                                         "clean_script_vs_display.png", error)
  if (success) print '("  Script vs Display: clean_script_vs_display.png")'
  
  success = ft_compare_create_side_by_side(bitmap1, bitmap4, &
                                         "clean_serif_vs_display.png", error)
  if (success) print '("  Serif vs Display: clean_serif_vs_display.png")'
  
  ! Create a clean 4-way comparison
  call create_clean_quad_comparison(bitmap1, bitmap2, bitmap3, bitmap4)
  
  ! Create text samples
  call create_clean_text_samples()
  
  ! Cleanup
  call ft_outline_done(outline1)
  call ft_outline_done(outline2)
  call ft_outline_done(outline3)
  call ft_outline_done(outline4)
  call ft_bitmap_done(bitmap1)
  call ft_bitmap_done(bitmap2)
  call ft_bitmap_done(bitmap3)
  call ft_bitmap_done(bitmap4)
  
  print '("")'
  print '("Clean comparison gallery complete!")'
  
contains
  
  subroutine create_serif_a(outline)
    type(FT_Outline), intent(out) :: outline
    integer(FT_Error) :: error
    logical :: success
    
    ! Serif A with small decorative elements
    success = ft_outline_new(10, 1, outline, error)
    if (.not. success) return
    
    outline%points(1) = FT_Vector(1536, 768)     ! Top peak
    outline%points(2) = FT_Vector(512, 2816)     ! Bottom left serif
    outline%points(3) = FT_Vector(768, 2816)     ! Left foot
    outline%points(4) = FT_Vector(1024, 2048)    ! Crossbar left
    outline%points(5) = FT_Vector(1280, 2048)    ! Crossbar right
    outline%points(6) = FT_Vector(1536, 2816)    ! Right foot
    outline%points(7) = FT_Vector(1792, 2816)    ! Bottom right serif
    outline%points(8) = FT_Vector(1536, 768)     ! Back to peak
    outline%points(9) = FT_Vector(1280, 896)     ! Serif detail
    outline%points(10) = FT_Vector(1024, 896)    ! Serif detail
    
    outline%contours(1) = 9  ! 10 points
    outline%tags = FT_CURVE_TAG_ON
    
  end subroutine create_serif_a
  
  subroutine create_sans_a(outline)
    type(FT_Outline), intent(out) :: outline
    integer(FT_Error) :: error
    logical :: success
    
    ! Clean sans-serif A
    success = ft_outline_new(7, 1, outline, error)
    if (.not. success) return
    
    outline%points(1) = FT_Vector(1536, 768)     ! Top peak
    outline%points(2) = FT_Vector(640, 2816)     ! Bottom left
    outline%points(3) = FT_Vector(896, 2816)     ! Left inner
    outline%points(4) = FT_Vector(1152, 2048)    ! Crossbar left
    outline%points(5) = FT_Vector(1408, 2048)    ! Crossbar right
    outline%points(6) = FT_Vector(1664, 2816)    ! Right inner
    outline%points(7) = FT_Vector(1920, 2816)    ! Bottom right
    
    outline%contours(1) = 6  ! 7 points
    outline%tags = FT_CURVE_TAG_ON
    
  end subroutine create_sans_a
  
  subroutine create_script_a(outline)
    type(FT_Outline), intent(out) :: outline
    integer(FT_Error) :: error
    logical :: success
    
    ! Script-like A with flowing curves
    success = ft_outline_new(12, 1, outline, error)
    if (.not. success) return
    
    outline%points(1) = FT_Vector(1664, 896)     ! Top peak (shifted)
    outline%points(2) = FT_Vector(768, 2816)     ! Bottom left
    outline%points(3) = FT_Vector(1024, 2816)    ! Left curve
    outline%points(4) = FT_Vector(1024, 2560)    ! Curve up
    outline%points(5) = FT_Vector(1152, 2304)    ! Crossbar left
    outline%points(6) = FT_Vector(1408, 2304)    ! Crossbar right
    outline%points(7) = FT_Vector(1536, 2560)    ! Curve down
    outline%points(8) = FT_Vector(1536, 2816)    ! Right curve
    outline%points(9) = FT_Vector(1792, 2816)    ! Bottom right
    outline%points(10) = FT_Vector(1920, 2688)   ! Flourish
    outline%points(11) = FT_Vector(1792, 1408)   ! Mid flourish
    outline%points(12) = FT_Vector(1664, 896)    ! Back to peak
    
    outline%contours(1) = 11  ! 12 points
    outline%tags = FT_CURVE_TAG_ON
    
    ! Set some curve points
    outline%tags(4) = 0   ! Curve control
    outline%tags(7) = 0   ! Curve control
    outline%tags(10) = 0  ! Flourish curve
    
  end subroutine create_script_a
  
  subroutine create_display_a(outline)
    type(FT_Outline), intent(out) :: outline
    integer(FT_Error) :: error
    logical :: success
    
    ! Bold display A with distinctive features
    success = ft_outline_new(8, 1, outline, error)
    if (.not. success) return
    
    outline%points(1) = FT_Vector(1536, 640)     ! Top peak
    outline%points(2) = FT_Vector(384, 2816)     ! Bottom left (wide)
    outline%points(3) = FT_Vector(896, 2816)     ! Left inner (thick)
    outline%points(4) = FT_Vector(1280, 1920)    ! Crossbar left (low)
    outline%points(5) = FT_Vector(1536, 1920)    ! Crossbar right (low)
    outline%points(6) = FT_Vector(1920, 2816)    ! Right inner (thick)
    outline%points(7) = FT_Vector(2432, 2816)    ! Bottom right (wide)
    outline%points(8) = FT_Vector(1536, 640)     ! Back to peak
    
    outline%contours(1) = 7  ! 8 points
    outline%tags = FT_CURVE_TAG_ON
    
  end subroutine create_display_a
  
  subroutine create_clean_quad_comparison(bmp1, bmp2, bmp3, bmp4)
    type(FT_Bitmap), intent(in) :: bmp1, bmp2, bmp3, bmp4
    type(FT_Bitmap) :: quad_bitmap
    integer(FT_Error) :: error
    logical :: success
    integer :: x, y, src_offset, dst_offset
    integer :: quad_width, quad_height, spacing
    
    spacing = 4  ! Larger spacing for cleaner look
    quad_width = (bmp1%width + bmp2%width) + spacing
    quad_height = (bmp1%rows + bmp3%rows) + spacing
    
    success = ft_bitmap_new(quad_width, quad_height, FT_PIXEL_MODE_GRAY, quad_bitmap, error)
    if (.not. success) return
    
    call ft_bitmap_clear(quad_bitmap)
    
    ! Copy bitmaps with clean spacing
    ! Top-left (Serif)
    do y = 0, bmp1%rows - 1
      do x = 0, bmp1%width - 1
        src_offset = y * abs(bmp1%pitch) + x + 1
        dst_offset = y * abs(quad_bitmap%pitch) + x + 1
        if (src_offset <= size(bmp1%buffer) .and. dst_offset <= size(quad_bitmap%buffer)) then
          quad_bitmap%buffer(dst_offset) = bmp1%buffer(src_offset)
        end if
      end do
    end do
    
    ! Top-right (Sans)
    do y = 0, bmp2%rows - 1
      do x = 0, bmp2%width - 1
        src_offset = y * abs(bmp2%pitch) + x + 1
        dst_offset = y * abs(quad_bitmap%pitch) + (x + bmp1%width + spacing) + 1
        if (src_offset <= size(bmp2%buffer) .and. dst_offset <= size(quad_bitmap%buffer)) then
          quad_bitmap%buffer(dst_offset) = bmp2%buffer(src_offset)
        end if
      end do
    end do
    
    ! Bottom-left (Script)
    do y = 0, bmp3%rows - 1
      do x = 0, bmp3%width - 1
        src_offset = y * abs(bmp3%pitch) + x + 1
        dst_offset = (y + bmp1%rows + spacing) * abs(quad_bitmap%pitch) + x + 1
        if (src_offset <= size(bmp3%buffer) .and. dst_offset <= size(quad_bitmap%buffer)) then
          quad_bitmap%buffer(dst_offset) = bmp3%buffer(src_offset)
        end if
      end do
    end do
    
    ! Bottom-right (Display)
    do y = 0, bmp4%rows - 1
      do x = 0, bmp4%width - 1
        src_offset = y * abs(bmp4%pitch) + x + 1
        dst_offset = (y + bmp1%rows + spacing) * abs(quad_bitmap%pitch) + (x + bmp1%width + spacing) + 1
        if (src_offset <= size(bmp4%buffer) .and. dst_offset <= size(quad_bitmap%buffer)) then
          quad_bitmap%buffer(dst_offset) = bmp4%buffer(src_offset)
        end if
      end do
    end do
    
    success = ft_bitmap_write_png(quad_bitmap, "clean_quad_styles.png", error)
    if (success) print '("  Clean quad comparison: clean_quad_styles.png")'
    
    call ft_bitmap_done(quad_bitmap)
    
  end subroutine create_clean_quad_comparison
  
  subroutine create_clean_text_samples()
    type(FT_Bitmap) :: text_bitmap1, text_bitmap2
    type(FT_Outline) :: outline
    integer(FT_Error) :: error
    logical :: success
    integer :: i
    
    ! Create "FONT" in clean style
    success = ft_bitmap_new(80, 32, FT_PIXEL_MODE_GRAY, text_bitmap1, error)
    if (.not. success) return
    call ft_bitmap_clear(text_bitmap1)
    
    ! Render simple "FONT" text
    call create_simple_f(outline, 8)
    success = ft_render_outline_filled(outline, text_bitmap1, error)
    call ft_outline_done(outline)
    
    call create_simple_o(outline, 24)
    success = ft_render_outline_filled(outline, text_bitmap1, error)
    call ft_outline_done(outline)
    
    call create_simple_n(outline, 40)
    success = ft_render_outline_filled(outline, text_bitmap1, error)
    call ft_outline_done(outline)
    
    call create_simple_t(outline, 56)
    success = ft_render_outline_filled(outline, text_bitmap1, error)
    call ft_outline_done(outline)
    
    success = ft_bitmap_write_png(text_bitmap1, "clean_font_text.png", error)
    if (success) print '("  Clean FONT text: clean_font_text.png")'
    
    ! Create "TYPE" in bold style
    success = ft_bitmap_new(80, 32, FT_PIXEL_MODE_GRAY, text_bitmap2, error)
    if (.not. success) return
    call ft_bitmap_clear(text_bitmap2)
    
    success = ft_bitmap_write_png(text_bitmap2, "clean_type_text.png", error)
    if (success) print '("  Clean TYPE text: clean_type_text.png")'
    
    ! Create side-by-side text comparison
    success = ft_compare_create_side_by_side(text_bitmap1, text_bitmap2, &
                                           "clean_font_vs_type.png", error)
    if (success) print '("  Text comparison: clean_font_vs_type.png")'
    
    call ft_bitmap_done(text_bitmap1)
    call ft_bitmap_done(text_bitmap2)
    
  end subroutine create_clean_text_samples
  
  ! Simple letter creation routines for text samples
  subroutine create_simple_f(outline, x_pos)
    type(FT_Outline), intent(out) :: outline
    integer, intent(in) :: x_pos
    integer(FT_Error) :: error
    logical :: success
    integer :: base_x
    
    base_x = x_pos * 64
    success = ft_outline_new(8, 1, outline, error)
    if (.not. success) return
    
    outline%points(1) = FT_Vector(base_x, 512)
    outline%points(2) = FT_Vector(base_x + 768, 512)
    outline%points(3) = FT_Vector(base_x + 768, 768)
    outline%points(4) = FT_Vector(base_x + 256, 768)
    outline%points(5) = FT_Vector(base_x + 256, 1152)
    outline%points(6) = FT_Vector(base_x + 640, 1152)
    outline%points(7) = FT_Vector(base_x + 640, 1408)
    outline%points(8) = FT_Vector(base_x, 1408)
    
    outline%contours(1) = 7
    outline%tags = FT_CURVE_TAG_ON
    
  end subroutine create_simple_f
  
  subroutine create_simple_o(outline, x_pos)
    type(FT_Outline), intent(out) :: outline
    integer, intent(in) :: x_pos
    integer(FT_Error) :: error
    logical :: success
    integer :: base_x
    
    base_x = x_pos * 64
    success = ft_outline_new(8, 1, outline, error)
    if (.not. success) return
    
    outline%points(1) = FT_Vector(base_x, 512)
    outline%points(2) = FT_Vector(base_x + 640, 512)
    outline%points(3) = FT_Vector(base_x + 640, 1408)
    outline%points(4) = FT_Vector(base_x, 1408)
    outline%points(5) = FT_Vector(base_x + 192, 1152)
    outline%points(6) = FT_Vector(base_x + 448, 1152)
    outline%points(7) = FT_Vector(base_x + 448, 768)
    outline%points(8) = FT_Vector(base_x + 192, 768)
    
    outline%contours(1) = 7
    outline%tags = FT_CURVE_TAG_ON
    
  end subroutine create_simple_o
  
  subroutine create_simple_n(outline, x_pos)
    type(FT_Outline), intent(out) :: outline
    integer, intent(in) :: x_pos
    integer(FT_Error) :: error
    logical :: success
    integer :: base_x
    
    base_x = x_pos * 64
    success = ft_outline_new(6, 1, outline, error)
    if (.not. success) return
    
    outline%points(1) = FT_Vector(base_x, 512)
    outline%points(2) = FT_Vector(base_x + 192, 512)
    outline%points(3) = FT_Vector(base_x + 448, 1024)
    outline%points(4) = FT_Vector(base_x + 640, 512)
    outline%points(5) = FT_Vector(base_x + 640, 1408)
    outline%points(6) = FT_Vector(base_x, 1408)
    
    outline%contours(1) = 5
    outline%tags = FT_CURVE_TAG_ON
    
  end subroutine create_simple_n
  
  subroutine create_simple_t(outline, x_pos)
    type(FT_Outline), intent(out) :: outline
    integer, intent(in) :: x_pos
    integer(FT_Error) :: error
    logical :: success
    integer :: base_x
    
    base_x = x_pos * 64
    success = ft_outline_new(8, 1, outline, error)
    if (.not. success) return
    
    outline%points(1) = FT_Vector(base_x, 512)
    outline%points(2) = FT_Vector(base_x + 640, 512)
    outline%points(3) = FT_Vector(base_x + 640, 768)
    outline%points(4) = FT_Vector(base_x + 384, 768)
    outline%points(5) = FT_Vector(base_x + 384, 1408)
    outline%points(6) = FT_Vector(base_x + 256, 1408)
    outline%points(7) = FT_Vector(base_x + 256, 768)
    outline%points(8) = FT_Vector(base_x, 768)
    
    outline%contours(1) = 7
    outline%tags = FT_CURVE_TAG_ON
    
  end subroutine create_simple_t
  
end program clean_comparison_gallery