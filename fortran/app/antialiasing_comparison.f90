program antialiasing_comparison
  use ft_types
  use ft_geometry
  use ft_bitmap_mod
  use ft_bitmap_io
  use ft_outline_mod
  use ft_scanline_simple
  use ft_raster
  use ft_compare
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  
  integer(FT_Error) :: error
  logical :: success
  type(FT_Bitmap) :: bitmap_mono, bitmap_gray, bitmap_smooth
  type(FT_Outline), target :: outline
  
  print '("Antialiasing Comparison Demo")'
  print '("============================")'
  print '("")'
  
  ! Create a detailed letter for antialiasing comparison
  call create_detailed_letter_r(outline)
  
  ! Render in monochrome (1-bit)
  print '("Rendering monochrome (1-bit)...")'
  success = ft_bitmap_new(48, 48, FT_PIXEL_MODE_MONO, bitmap_mono, error)
  if (.not. success) stop 1
  success = ft_raster_render_outline_scanline(outline, bitmap_mono, error)
  if (success) then
    success = ft_bitmap_write_png(bitmap_mono, "aa_mono.png", error)
    if (success) print '("  Monochrome: aa_mono.png")'
  end if
  
  ! Render in grayscale (8-bit antialiased)
  print '("Rendering grayscale (8-bit antialiased)...")'
  success = ft_bitmap_new(48, 48, FT_PIXEL_MODE_GRAY, bitmap_gray, error)
  if (.not. success) stop 1
  success = ft_render_outline_filled(outline, bitmap_gray, error)
  if (success) then
    success = ft_bitmap_write_png(bitmap_gray, "aa_gray.png", error)
    if (success) print '("  Grayscale: aa_gray.png")'
  end if
  
  ! Create smooth antialiased version (simulated)
  print '("Creating smooth antialiased version...")'
  call create_smooth_version(bitmap_gray, bitmap_smooth)
  success = ft_bitmap_write_png(bitmap_smooth, "aa_smooth.png", error)
  if (success) print '("  Smooth: aa_smooth.png")'
  
  print '("")'
  print '("Creating antialiasing comparisons...")'
  
  ! Create side-by-side comparisons
  success = ft_compare_create_side_by_side(bitmap_mono, bitmap_gray, &
                                         "aa_mono_vs_gray.png", error)
  if (success) print '("  Mono vs Gray: aa_mono_vs_gray.png")'
  
  success = ft_compare_create_side_by_side(bitmap_gray, bitmap_smooth, &
                                         "aa_gray_vs_smooth.png", error)
  if (success) print '("  Gray vs Smooth: aa_gray_vs_smooth.png")'
  
  success = ft_compare_create_side_by_side(bitmap_mono, bitmap_smooth, &
                                         "aa_mono_vs_smooth.png", error)
  if (success) print '("  Mono vs Smooth: aa_mono_vs_smooth.png")'
  
  ! Create triple comparison
  call create_triple_comparison(bitmap_mono, bitmap_gray, bitmap_smooth)
  
  ! Create difference images
  success = ft_compare_create_diff_image(bitmap_mono, bitmap_gray, &
                                       "aa_mono_gray_diff.png", error)
  if (success) print '("  Mono-Gray Difference: aa_mono_gray_diff.png")'
  
  success = ft_compare_create_diff_image(bitmap_gray, bitmap_smooth, &
                                       "aa_gray_smooth_diff.png", error)
  if (success) print '("  Gray-Smooth Difference: aa_gray_smooth_diff.png")'
  
  ! Cleanup
  call ft_outline_done(outline)
  call ft_bitmap_done(bitmap_mono)
  call ft_bitmap_done(bitmap_gray)
  call ft_bitmap_done(bitmap_smooth)
  
  print '("")'
  print '("Antialiasing comparison complete! Check the aa_*.png files.")'
  
contains
  
  subroutine create_detailed_letter_r(outline)
    type(FT_Outline), intent(out) :: outline
    integer(FT_Error) :: error
    logical :: success
    
    ! Create a detailed letter R with curves for better antialiasing demo
    success = ft_outline_new(16, 1, outline, error)
    if (.not. success) return
    
    ! Letter R (simplified but with more detail than basic shapes)
    ! Vertical stroke
    outline%points(1) = FT_Vector(512, 512)      ! Top left
    outline%points(2) = FT_Vector(768, 512)      ! Top of stem
    outline%points(3) = FT_Vector(768, 1152)     ! Middle of stem
    outline%points(4) = FT_Vector(1280, 1152)    ! Top bowl right
    outline%points(5) = FT_Vector(1408, 1280)    ! Bowl curve 1
    outline%points(6) = FT_Vector(1408, 1536)    ! Bowl curve 2
    outline%points(7) = FT_Vector(1280, 1664)    ! Bowl bottom right
    outline%points(8) = FT_Vector(768, 1664)     ! Bowl bottom left
    outline%points(9) = FT_Vector(768, 1920)     ! Middle stem
    outline%points(10) = FT_Vector(1152, 1920)   ! Leg start
    outline%points(11) = FT_Vector(1408, 2560)   ! Leg end
    outline%points(12) = FT_Vector(1152, 2688)   ! Leg bottom
    outline%points(13) = FT_Vector(896, 2176)    ! Leg connection
    outline%points(14) = FT_Vector(768, 2176)    ! Stem bottom right
    outline%points(15) = FT_Vector(768, 2880)    ! Stem bottom
    outline%points(16) = FT_Vector(512, 2880)    ! Bottom left
    
    outline%contours(1) = 15  ! 16 points
    outline%tags = FT_CURVE_TAG_ON
    
    ! Set some points as off-curve for smoother curves
    outline%tags(5) = 0   ! Bowl curve control point
    outline%tags(6) = 0   ! Bowl curve control point
    
  end subroutine create_detailed_letter_r
  
  subroutine create_smooth_version(source_bitmap, smooth_bitmap)
    type(FT_Bitmap), intent(in) :: source_bitmap
    type(FT_Bitmap), intent(out) :: smooth_bitmap
    integer(FT_Error) :: error
    logical :: success
    integer :: x, y, src_offset, dst_offset
    integer :: neighbors(9), sum_val, avg_val
    integer :: dx, dy, nx, ny, neighbor_count
    
    ! Create smooth bitmap (same size and format as source)
    success = ft_bitmap_new(source_bitmap%width, source_bitmap%rows, &
                           source_bitmap%pixel_mode, smooth_bitmap, error)
    if (.not. success) return
    
    call ft_bitmap_clear(smooth_bitmap)
    
    ! Apply simple smoothing filter (3x3 average)
    do y = 1, source_bitmap%rows - 2
      do x = 1, source_bitmap%width - 2
        
        neighbor_count = 0
        sum_val = 0
        
        ! Sample 3x3 neighborhood
        do dy = -1, 1
          do dx = -1, 1
            nx = x + dx
            ny = y + dy
            
            if (nx >= 0 .and. nx < source_bitmap%width .and. &
                ny >= 0 .and. ny < source_bitmap%rows) then
              
              if (source_bitmap%pixel_mode == FT_PIXEL_MODE_MONO) then
                if (ft_bitmap_get_pixel(source_bitmap, nx, ny)) then
                  sum_val = sum_val + 255
                end if
              else
                src_offset = ny * abs(source_bitmap%pitch) + nx + 1
                if (src_offset <= size(source_bitmap%buffer)) then
                  ! Convert signed int8 to unsigned for averaging
                  sum_val = sum_val + int(source_bitmap%buffer(src_offset)) + 128
                end if
              end if
              neighbor_count = neighbor_count + 1
            end if
          end do
        end do
        
        ! Calculate average and apply smoothing
        if (neighbor_count > 0) then
          avg_val = sum_val / neighbor_count
          
          if (smooth_bitmap%pixel_mode == FT_PIXEL_MODE_MONO) then
            call ft_bitmap_set_pixel(smooth_bitmap, x, y, avg_val > 127)
          else
            dst_offset = y * abs(smooth_bitmap%pitch) + x + 1
            if (dst_offset <= size(smooth_bitmap%buffer)) then
              ! Convert back to signed int8
              smooth_bitmap%buffer(dst_offset) = int(avg_val - 128, int8)
            end if
          end if
        end if
      end do
    end do
    
  end subroutine create_smooth_version
  
  subroutine create_triple_comparison(bmp1, bmp2, bmp3)
    type(FT_Bitmap), intent(in) :: bmp1, bmp2, bmp3
    type(FT_Bitmap) :: triple_bitmap
    integer(FT_Error) :: error
    logical :: success
    integer :: x, y, src_offset, dst_offset
    integer :: triple_width, triple_height, spacing
    
    spacing = 2
    triple_width = bmp1%width + bmp2%width + bmp3%width + 2 * spacing
    triple_height = max(max(bmp1%rows, bmp2%rows), bmp3%rows)
    
    success = ft_bitmap_new(triple_width, triple_height, FT_PIXEL_MODE_GRAY, triple_bitmap, error)
    if (.not. success) return
    
    call ft_bitmap_clear(triple_bitmap)
    
    ! Copy first bitmap (monochrome)
    do y = 0, bmp1%rows - 1
      do x = 0, bmp1%width - 1
        if (bmp1%pixel_mode == FT_PIXEL_MODE_MONO) then
          if (ft_bitmap_get_pixel(bmp1, x, y)) then
            dst_offset = y * abs(triple_bitmap%pitch) + x + 1
            if (dst_offset <= size(triple_bitmap%buffer)) then
              triple_bitmap%buffer(dst_offset) = int(127, int8)  ! Full intensity
            end if
          end if
        end if
      end do
    end do
    
    ! Copy second bitmap (grayscale)
    do y = 0, bmp2%rows - 1
      do x = 0, bmp2%width - 1
        src_offset = y * abs(bmp2%pitch) + x + 1
        dst_offset = y * abs(triple_bitmap%pitch) + (x + bmp1%width + spacing) + 1
        if (src_offset <= size(bmp2%buffer) .and. dst_offset <= size(triple_bitmap%buffer)) then
          triple_bitmap%buffer(dst_offset) = bmp2%buffer(src_offset)
        end if
      end do
    end do
    
    ! Copy third bitmap (smooth)
    do y = 0, bmp3%rows - 1
      do x = 0, bmp3%width - 1
        src_offset = y * abs(bmp3%pitch) + x + 1
        dst_offset = y * abs(triple_bitmap%pitch) + (x + bmp1%width + bmp2%width + 2*spacing) + 1
        if (src_offset <= size(bmp3%buffer) .and. dst_offset <= size(triple_bitmap%buffer)) then
          triple_bitmap%buffer(dst_offset) = bmp3%buffer(src_offset)
        end if
      end do
    end do
    
    success = ft_bitmap_write_png(triple_bitmap, "aa_triple_comparison.png", error)
    if (success) print '("  Triple comparison: aa_triple_comparison.png")'
    
    call ft_bitmap_done(triple_bitmap)
    
  end subroutine create_triple_comparison
  
end program antialiasing_comparison