program simple_font_showcase
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
  
  print '("Simple Font Showcase")'
  print '("===================")'
  print '("")'
  
  ! Create various font demonstrations
  call create_size_comparisons()
  call create_style_comparisons()
  call create_rendering_comparisons()
  
  print '("")'
  print '("Font showcase complete! Check the showcase_*.png files.")'
  
contains
  
  subroutine create_size_comparisons()
    type(FT_Bitmap) :: small_bitmap, medium_bitmap, large_bitmap
    type(FT_Outline) :: outline
    integer(FT_Error) :: error
    logical :: success
    
    print '("Creating size comparisons...")'
    
    ! Small (24x24)
    call create_standard_letter_g(outline, 0.6)
    success = ft_bitmap_new(24, 24, FT_PIXEL_MODE_GRAY, small_bitmap, error)
    if (success) then
      success = ft_render_outline_filled(outline, small_bitmap, error)
      success = ft_bitmap_write_png(small_bitmap, "showcase_small_g.png", error)
      if (success) print '("  Small G: showcase_small_g.png")'
    end if
    call ft_outline_done(outline)
    
    ! Medium (32x32)
    call create_standard_letter_g(outline, 1.0)
    success = ft_bitmap_new(32, 32, FT_PIXEL_MODE_GRAY, medium_bitmap, error)
    if (success) then
      success = ft_render_outline_filled(outline, medium_bitmap, error)
      success = ft_bitmap_write_png(medium_bitmap, "showcase_medium_g.png", error)
      if (success) print '("  Medium G: showcase_medium_g.png")'
    end if
    call ft_outline_done(outline)
    
    ! Large (48x48)
    call create_standard_letter_g(outline, 1.5)
    success = ft_bitmap_new(48, 48, FT_PIXEL_MODE_GRAY, large_bitmap, error)
    if (success) then
      success = ft_render_outline_filled(outline, large_bitmap, error)
      success = ft_bitmap_write_png(large_bitmap, "showcase_large_g.png", error)
      if (success) print '("  Large G: showcase_large_g.png")'
    end if
    call ft_outline_done(outline)
    
    ! Create size comparison
    success = ft_compare_create_side_by_side(small_bitmap, medium_bitmap, &
                                           "showcase_sizes_sm_med.png", error)
    if (success) print '("  Small vs Medium: showcase_sizes_sm_med.png")'
    
    success = ft_compare_create_side_by_side(medium_bitmap, large_bitmap, &
                                           "showcase_sizes_med_lg.png", error)
    if (success) print '("  Medium vs Large: showcase_sizes_med_lg.png")'
    
    call ft_bitmap_done(small_bitmap)
    call ft_bitmap_done(medium_bitmap)
    call ft_bitmap_done(large_bitmap)
    
  end subroutine create_size_comparisons
  
  subroutine create_style_comparisons()
    type(FT_Bitmap) :: regular_bitmap, bold_bitmap, italic_bitmap
    type(FT_Outline) :: outline
    integer(FT_Error) :: error
    logical :: success
    
    print '("Creating style comparisons...")'
    
    ! Regular
    call create_letter_b_regular(outline)
    success = ft_bitmap_new(32, 32, FT_PIXEL_MODE_GRAY, regular_bitmap, error)
    if (success) then
      success = ft_render_outline_filled(outline, regular_bitmap, error)
      success = ft_bitmap_write_png(regular_bitmap, "showcase_regular_b.png", error)
      if (success) print '("  Regular B: showcase_regular_b.png")'
    end if
    call ft_outline_done(outline)
    
    ! Bold
    call create_letter_b_bold(outline)
    success = ft_bitmap_new(32, 32, FT_PIXEL_MODE_GRAY, bold_bitmap, error)
    if (success) then
      success = ft_render_outline_filled(outline, bold_bitmap, error)
      success = ft_bitmap_write_png(bold_bitmap, "showcase_bold_b.png", error)
      if (success) print '("  Bold B: showcase_bold_b.png")'
    end if
    call ft_outline_done(outline)
    
    ! Italic
    call create_letter_b_italic(outline)
    success = ft_bitmap_new(32, 32, FT_PIXEL_MODE_GRAY, italic_bitmap, error)
    if (success) then
      success = ft_render_outline_filled(outline, italic_bitmap, error)
      success = ft_bitmap_write_png(italic_bitmap, "showcase_italic_b.png", error)
      if (success) print '("  Italic B: showcase_italic_b.png")'
    end if
    call ft_outline_done(outline)
    
    ! Create style comparisons
    success = ft_compare_create_side_by_side(regular_bitmap, bold_bitmap, &
                                           "showcase_regular_vs_bold.png", error)
    if (success) print '("  Regular vs Bold: showcase_regular_vs_bold.png")'
    
    success = ft_compare_create_side_by_side(regular_bitmap, italic_bitmap, &
                                           "showcase_regular_vs_italic.png", error)
    if (success) print '("  Regular vs Italic: showcase_regular_vs_italic.png")'
    
    call ft_bitmap_done(regular_bitmap)
    call ft_bitmap_done(bold_bitmap)
    call ft_bitmap_done(italic_bitmap)
    
  end subroutine create_style_comparisons
  
  subroutine create_rendering_comparisons()
    type(FT_Bitmap) :: mono_bitmap, aa_bitmap
    type(FT_Outline) :: outline
    integer(FT_Error) :: error
    logical :: success
    
    print '("Creating rendering comparisons...")'
    
    call create_detailed_letter_p(outline)
    
    ! Monochrome rendering
    success = ft_bitmap_new(40, 40, FT_PIXEL_MODE_MONO, mono_bitmap, error)
    if (success) then
      success = ft_raster_render_outline_scanline(outline, mono_bitmap, error)
      success = ft_bitmap_write_png(mono_bitmap, "showcase_mono_p.png", error)
      if (success) print '("  Monochrome P: showcase_mono_p.png")'
    end if
    
    ! Antialiased rendering
    success = ft_bitmap_new(40, 40, FT_PIXEL_MODE_GRAY, aa_bitmap, error)
    if (success) then
      success = ft_render_outline_filled(outline, aa_bitmap, error)
      success = ft_bitmap_write_png(aa_bitmap, "showcase_aa_p.png", error)
      if (success) print '("  Antialiased P: showcase_aa_p.png")'
    end if
    
    ! Create rendering comparison
    success = ft_compare_create_side_by_side(mono_bitmap, aa_bitmap, &
                                           "showcase_mono_vs_aa.png", error)
    if (success) print '("  Mono vs AA: showcase_mono_vs_aa.png")'
    
    call ft_outline_done(outline)
    call ft_bitmap_done(mono_bitmap)
    call ft_bitmap_done(aa_bitmap)
    
  end subroutine create_rendering_comparisons
  
  subroutine create_standard_letter_g(outline, scale)
    type(FT_Outline), intent(out) :: outline
    real, intent(in) :: scale
    integer(FT_Error) :: error
    logical :: success
    integer :: base_scale
    
    base_scale = int(scale * 64)
    
    success = ft_outline_new(12, 1, outline, error)
    if (.not. success) return
    
    ! Letter G with horizontal bar
    outline%points(1) = FT_Vector(16 * base_scale, 8 * base_scale)   ! Top left
    outline%points(2) = FT_Vector(24 * base_scale, 8 * base_scale)   ! Top right
    outline%points(3) = FT_Vector(24 * base_scale, 16 * base_scale)  ! Right top
    outline%points(4) = FT_Vector(20 * base_scale, 16 * base_scale)  ! Bar left
    outline%points(5) = FT_Vector(20 * base_scale, 18 * base_scale)  ! Bar bottom
    outline%points(6) = FT_Vector(24 * base_scale, 18 * base_scale)  ! Right mid
    outline%points(7) = FT_Vector(24 * base_scale, 24 * base_scale)  ! Bottom right
    outline%points(8) = FT_Vector(16 * base_scale, 24 * base_scale)  ! Bottom left
    outline%points(9) = FT_Vector(16 * base_scale, 20 * base_scale)  ! Left bottom
    outline%points(10) = FT_Vector(20 * base_scale, 20 * base_scale) ! Inner bottom
    outline%points(11) = FT_Vector(20 * base_scale, 12 * base_scale) ! Inner top
    outline%points(12) = FT_Vector(16 * base_scale, 12 * base_scale) ! Left top
    
    outline%contours(1) = 11  ! 12 points
    outline%tags = FT_CURVE_TAG_ON
    
  end subroutine create_standard_letter_g
  
  subroutine create_letter_b_regular(outline)
    type(FT_Outline), intent(out) :: outline
    integer(FT_Error) :: error
    logical :: success
    
    success = ft_outline_new(16, 1, outline, error)
    if (.not. success) return
    
    ! Letter B with two bowls
    outline%points(1) = FT_Vector(512, 512)      ! Top left
    outline%points(2) = FT_Vector(1536, 512)     ! Top right
    outline%points(3) = FT_Vector(1536, 1024)    ! Top bowl bottom
    outline%points(4) = FT_Vector(768, 1024)     ! Back to stem
    outline%points(5) = FT_Vector(768, 1280)     ! Middle
    outline%points(6) = FT_Vector(1664, 1280)    ! Bottom bowl top
    outline%points(7) = FT_Vector(1664, 1920)    ! Bottom bowl bottom
    outline%points(8) = FT_Vector(512, 1920)     ! Bottom left
    outline%points(9) = FT_Vector(512, 1664)     ! Left inner bottom
    outline%points(10) = FT_Vector(1408, 1664)   ! Inner bowl bottom
    outline%points(11) = FT_Vector(1408, 1536)   ! Inner bowl top
    outline%points(12) = FT_Vector(768, 1536)    ! Back to stem
    outline%points(13) = FT_Vector(768, 1024)    ! Middle
    outline%points(14) = FT_Vector(1280, 1024)   ! Top bowl inner
    outline%points(15) = FT_Vector(1280, 768)    ! Top bowl top
    outline%points(16) = FT_Vector(512, 768)     ! Left inner top
    
    outline%contours(1) = 15  ! 16 points
    outline%tags = FT_CURVE_TAG_ON
    
  end subroutine create_letter_b_regular
  
  subroutine create_letter_b_bold(outline)
    type(FT_Outline), intent(out) :: outline
    integer(FT_Error) :: error
    logical :: success
    
    success = ft_outline_new(12, 1, outline, error)
    if (.not. success) return
    
    ! Bold B (simplified with thicker strokes)
    outline%points(1) = FT_Vector(384, 512)      ! Top left
    outline%points(2) = FT_Vector(1664, 512)     ! Top right (wider)
    outline%points(3) = FT_Vector(1664, 1152)    ! Top bowl bottom
    outline%points(4) = FT_Vector(896, 1152)     ! Back to stem (thicker)
    outline%points(5) = FT_Vector(896, 1280)     ! Middle
    outline%points(6) = FT_Vector(1792, 1280)    ! Bottom bowl top (wider)
    outline%points(7) = FT_Vector(1792, 1920)    ! Bottom bowl bottom
    outline%points(8) = FT_Vector(384, 1920)     ! Bottom left
    outline%points(9) = FT_Vector(384, 1536)     ! Left inner bottom (thicker)
    outline%points(10) = FT_Vector(1408, 1536)   ! Inner bowl bottom
    outline%points(11) = FT_Vector(1408, 896)    ! Inner bowl top (larger)
    outline%points(12) = FT_Vector(384, 896)     ! Left inner top (thicker)
    
    outline%contours(1) = 11  ! 12 points
    outline%tags = FT_CURVE_TAG_ON
    
  end subroutine create_letter_b_bold
  
  subroutine create_letter_b_italic(outline)
    type(FT_Outline), intent(out) :: outline
    integer(FT_Error) :: error
    logical :: success
    
    success = ft_outline_new(12, 1, outline, error)
    if (.not. success) return
    
    ! Italic B (slanted)
    outline%points(1) = FT_Vector(640, 512)      ! Top left (shifted)
    outline%points(2) = FT_Vector(1664, 512)     ! Top right
    outline%points(3) = FT_Vector(1664, 1024)    ! Top bowl bottom
    outline%points(4) = FT_Vector(896, 1024)     ! Back to stem
    outline%points(5) = FT_Vector(832, 1280)     ! Middle (shifted)
    outline%points(6) = FT_Vector(1600, 1280)    ! Bottom bowl top (shifted)
    outline%points(7) = FT_Vector(1600, 1920)    ! Bottom bowl bottom
    outline%points(8) = FT_Vector(448, 1920)     ! Bottom left (shifted)
    outline%points(9) = FT_Vector(512, 1664)     ! Left inner bottom (shifted)
    outline%points(10) = FT_Vector(1344, 1664)   ! Inner bowl bottom (shifted)
    outline%points(11) = FT_Vector(1344, 768)    ! Inner bowl top (shifted)
    outline%points(12) = FT_Vector(704, 768)     ! Left inner top (shifted)
    
    outline%contours(1) = 11  ! 12 points
    outline%tags = FT_CURVE_TAG_ON
    
  end subroutine create_letter_b_italic
  
  subroutine create_detailed_letter_p(outline)
    type(FT_Outline), intent(out) :: outline
    integer(FT_Error) :: error
    logical :: success
    
    success = ft_outline_new(10, 1, outline, error)
    if (.not. success) return
    
    ! Letter P with bowl
    outline%points(1) = FT_Vector(512, 512)      ! Top left
    outline%points(2) = FT_Vector(1536, 512)     ! Top right
    outline%points(3) = FT_Vector(1536, 1280)    ! Bowl bottom right
    outline%points(4) = FT_Vector(768, 1280)     ! Bowl bottom left
    outline%points(5) = FT_Vector(768, 2048)     ! Stem bottom right
    outline%points(6) = FT_Vector(512, 2048)     ! Stem bottom left
    outline%points(7) = FT_Vector(512, 1536)     ! Inner bottom
    outline%points(8) = FT_Vector(1280, 1536)    ! Inner bowl right
    outline%points(9) = FT_Vector(1280, 768)     ! Inner bowl top
    outline%points(10) = FT_Vector(512, 768)     ! Inner top
    
    outline%contours(1) = 9   ! 10 points
    outline%tags = FT_CURVE_TAG_ON
    
  end subroutine create_detailed_letter_p
  
end program simple_font_showcase