program create_font_gallery
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
  integer :: i
  
  print '("Font Gallery Creator")'
  print '("===================")'
  print '("")'
  
  ! Create different font styles/sizes for comparison
  
  ! Style 1: Clean letter 'A' (32x32)
  print '("Creating Style 1: Clean A...")'
  call create_letter_a_clean(outline1)
  success = ft_bitmap_new(32, 32, FT_PIXEL_MODE_GRAY, bitmap1, error)
  if (.not. success) stop 1
  success = ft_render_outline_filled(outline1, bitmap1, error)
  if (success) then
    success = ft_bitmap_write_png(bitmap1, "gallery_clean_a.png", error)
    print '("  Saved: gallery_clean_a.png")'
  end if
  
  ! Style 2: Bold letter 'A' (32x32)
  print '("Creating Style 2: Bold A...")'
  call create_letter_a_bold(outline2)
  success = ft_bitmap_new(32, 32, FT_PIXEL_MODE_GRAY, bitmap2, error)
  if (.not. success) stop 1
  success = ft_render_outline_filled(outline2, bitmap2, error)
  if (success) then
    success = ft_bitmap_write_png(bitmap2, "gallery_bold_a.png", error)
    print '("  Saved: gallery_bold_a.png")'
  end if
  
  ! Style 3: Italic letter 'A' (32x32)
  print '("Creating Style 3: Italic A...")'
  call create_letter_a_italic(outline3)
  success = ft_bitmap_new(32, 32, FT_PIXEL_MODE_GRAY, bitmap3, error)
  if (.not. success) stop 1
  success = ft_render_outline_filled(outline3, bitmap3, error)
  if (success) then
    success = ft_bitmap_write_png(bitmap3, "gallery_italic_a.png", error)
    print '("  Saved: gallery_italic_a.png")'
  end if
  
  ! Style 4: Monospace letter 'A' (32x32)
  print '("Creating Style 4: Mono A...")'
  call create_letter_a_mono(outline4)
  success = ft_bitmap_new(32, 32, FT_PIXEL_MODE_GRAY, bitmap4, error)
  if (.not. success) stop 1
  success = ft_render_outline_filled(outline4, bitmap4, error)
  if (success) then
    success = ft_bitmap_write_png(bitmap4, "gallery_mono_a.png", error)
    print '("  Saved: gallery_mono_a.png")'
  end if
  
  print '("")'
  print '("Creating side-by-side comparisons...")'
  
  ! Create pairwise comparisons
  success = ft_compare_create_side_by_side(bitmap1, bitmap2, &
                                         "gallery_clean_vs_bold.png", error)
  if (success) print '("  Clean vs Bold: gallery_clean_vs_bold.png")'
  
  success = ft_compare_create_side_by_side(bitmap1, bitmap3, &
                                         "gallery_clean_vs_italic.png", error)
  if (success) print '("  Clean vs Italic: gallery_clean_vs_italic.png")'
  
  success = ft_compare_create_side_by_side(bitmap2, bitmap4, &
                                         "gallery_bold_vs_mono.png", error)
  if (success) print '("  Bold vs Mono: gallery_bold_vs_mono.png")'
  
  success = ft_compare_create_side_by_side(bitmap3, bitmap4, &
                                         "gallery_italic_vs_mono.png", error)
  if (success) print '("  Italic vs Mono: gallery_italic_vs_mono.png")'
  
  ! Create a 4-way comparison
  call create_quad_comparison(bitmap1, bitmap2, bitmap3, bitmap4)
  
  ! Create difference images
  success = ft_compare_create_diff_image(bitmap1, bitmap2, &
                                       "gallery_clean_bold_diff.png", error)
  if (success) print '("  Difference (Clean vs Bold): gallery_clean_bold_diff.png")'
  
  success = ft_compare_create_diff_image(bitmap1, bitmap3, &
                                       "gallery_clean_italic_diff.png", error)
  if (success) print '("  Difference (Clean vs Italic): gallery_clean_italic_diff.png")'
  
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
  print '("Font gallery complete! Check the generated PNG files.")'
  
contains
  
  subroutine create_letter_a_clean(outline)
    type(FT_Outline), intent(out) :: outline
    integer(FT_Error) :: error
    logical :: success
    
    ! Create clean letter A (triangle with crossbar)
    success = ft_outline_new(7, 1, outline, error)
    if (.not. success) return
    
    ! Clean A shape (26.6 fixed point: 64 units = 1 pixel)
    outline%points(1) = FT_Vector(1024, 2048)   ! Top peak
    outline%points(2) = FT_Vector(384, 3584)    ! Bottom left
    outline%points(3) = FT_Vector(640, 3584)    ! Left leg inner
    outline%points(4) = FT_Vector(896, 2560)    ! Crossbar left
    outline%points(5) = FT_Vector(1152, 2560)   ! Crossbar right
    outline%points(6) = FT_Vector(1408, 3584)   ! Right leg inner
    outline%points(7) = FT_Vector(1664, 3584)   ! Bottom right
    
    outline%contours(1) = 6  ! 7 points (0-indexed end)
    outline%tags = FT_CURVE_TAG_ON
    
  end subroutine create_letter_a_clean
  
  subroutine create_letter_a_bold(outline)
    type(FT_Outline), intent(out) :: outline
    integer(FT_Error) :: error
    logical :: success
    
    ! Create bold letter A (wider strokes)
    success = ft_outline_new(7, 1, outline, error)
    if (.not. success) return
    
    ! Bold A shape - wider at base
    outline%points(1) = FT_Vector(1024, 1920)   ! Top peak (wider)
    outline%points(2) = FT_Vector(256, 3584)    ! Bottom left (wider)
    outline%points(3) = FT_Vector(704, 3584)    ! Left leg inner (wider)
    outline%points(4) = FT_Vector(832, 2432)    ! Crossbar left (lower)
    outline%points(5) = FT_Vector(1216, 2432)   ! Crossbar right (lower)
    outline%points(6) = FT_Vector(1344, 3584)   ! Right leg inner (wider)
    outline%points(7) = FT_Vector(1792, 3584)   ! Bottom right (wider)
    
    outline%contours(1) = 6  ! 7 points
    outline%tags = FT_CURVE_TAG_ON
    
  end subroutine create_letter_a_bold
  
  subroutine create_letter_a_italic(outline)
    type(FT_Outline), intent(out) :: outline
    integer(FT_Error) :: error
    logical :: success
    
    ! Create italic letter A (slanted)
    success = ft_outline_new(7, 1, outline, error)
    if (.not. success) return
    
    ! Italic A shape - slanted to the right
    outline%points(1) = FT_Vector(1280, 2048)   ! Top peak (shifted right)
    outline%points(2) = FT_Vector(512, 3584)    ! Bottom left
    outline%points(3) = FT_Vector(768, 3584)    ! Left leg inner
    outline%points(4) = FT_Vector(1088, 2560)   ! Crossbar left (shifted)
    outline%points(5) = FT_Vector(1344, 2560)   ! Crossbar right (shifted)
    outline%points(6) = FT_Vector(1536, 3584)   ! Right leg inner
    outline%points(7) = FT_Vector(1792, 3584)   ! Bottom right
    
    outline%contours(1) = 6  ! 7 points
    outline%tags = FT_CURVE_TAG_ON
    
  end subroutine create_letter_a_italic
  
  subroutine create_letter_a_mono(outline)
    type(FT_Outline), intent(out) :: outline
    integer(FT_Error) :: error
    logical :: success
    
    ! Create monospace letter A (fixed width, more geometric)
    success = ft_outline_new(7, 1, outline, error)
    if (.not. success) return
    
    ! Monospace A shape - more uniform, geometric
    outline%points(1) = FT_Vector(1024, 2176)   ! Top peak
    outline%points(2) = FT_Vector(448, 3584)    ! Bottom left
    outline%points(3) = FT_Vector(640, 3584)    ! Left leg inner
    outline%points(4) = FT_Vector(896, 2688)    ! Crossbar left (higher)
    outline%points(5) = FT_Vector(1152, 2688)   ! Crossbar right (higher)
    outline%points(6) = FT_Vector(1408, 3584)   ! Right leg inner
    outline%points(7) = FT_Vector(1600, 3584)   ! Bottom right
    
    outline%contours(1) = 6  ! 7 points
    outline%tags = FT_CURVE_TAG_ON
    
  end subroutine create_letter_a_mono
  
  subroutine create_quad_comparison(bmp1, bmp2, bmp3, bmp4)
    type(FT_Bitmap), intent(in) :: bmp1, bmp2, bmp3, bmp4
    type(FT_Bitmap) :: quad_bitmap
    integer(FT_Error) :: error
    logical :: success
    integer :: x, y, src_offset, dst_offset
    integer :: quad_width, quad_height
    
    ! Create a 2x2 grid of the four bitmaps
    quad_width = (bmp1%width + bmp2%width) + 4  ! 2 pixel separator
    quad_height = (bmp1%rows + bmp3%rows) + 4   ! 2 pixel separator
    
    success = ft_bitmap_new(quad_width, quad_height, FT_PIXEL_MODE_GRAY, quad_bitmap, error)
    if (.not. success) return
    
    call ft_bitmap_clear(quad_bitmap)
    
    ! Copy top-left (bmp1)
    do y = 0, bmp1%rows - 1
      do x = 0, bmp1%width - 1
        src_offset = y * abs(bmp1%pitch) + x + 1
        dst_offset = y * abs(quad_bitmap%pitch) + x + 1
        if (src_offset <= size(bmp1%buffer) .and. dst_offset <= size(quad_bitmap%buffer)) then
          quad_bitmap%buffer(dst_offset) = bmp1%buffer(src_offset)
        end if
      end do
    end do
    
    ! Copy top-right (bmp2)
    do y = 0, bmp2%rows - 1
      do x = 0, bmp2%width - 1
        src_offset = y * abs(bmp2%pitch) + x + 1
        dst_offset = y * abs(quad_bitmap%pitch) + (x + bmp1%width + 2) + 1
        if (src_offset <= size(bmp2%buffer) .and. dst_offset <= size(quad_bitmap%buffer)) then
          quad_bitmap%buffer(dst_offset) = bmp2%buffer(src_offset)
        end if
      end do
    end do
    
    ! Copy bottom-left (bmp3)
    do y = 0, bmp3%rows - 1
      do x = 0, bmp3%width - 1
        src_offset = y * abs(bmp3%pitch) + x + 1
        dst_offset = (y + bmp1%rows + 2) * abs(quad_bitmap%pitch) + x + 1
        if (src_offset <= size(bmp3%buffer) .and. dst_offset <= size(quad_bitmap%buffer)) then
          quad_bitmap%buffer(dst_offset) = bmp3%buffer(src_offset)
        end if
      end do
    end do
    
    ! Copy bottom-right (bmp4)
    do y = 0, bmp4%rows - 1
      do x = 0, bmp4%width - 1
        src_offset = y * abs(bmp4%pitch) + x + 1
        dst_offset = (y + bmp1%rows + 2) * abs(quad_bitmap%pitch) + (x + bmp1%width + 2) + 1
        if (src_offset <= size(bmp4%buffer) .and. dst_offset <= size(quad_bitmap%buffer)) then
          quad_bitmap%buffer(dst_offset) = bmp4%buffer(src_offset)
        end if
      end do
    end do
    
    ! Save quad comparison
    success = ft_bitmap_write_png(quad_bitmap, "gallery_quad_comparison.png", error)
    if (success) print '("  Quad comparison: gallery_quad_comparison.png")'
    
    call ft_bitmap_done(quad_bitmap)
    
  end subroutine create_quad_comparison
  
end program create_font_gallery