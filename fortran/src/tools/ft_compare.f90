module ft_compare
  use ft_types
  use ft_bitmap_mod
  use ft_bitmap_io
  use fortplot_png, only: write_png_file
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  private
  
  ! Public functions
  public :: ft_compare_create_side_by_side
  public :: ft_compare_create_diff_image
  public :: ft_compare_pixel_difference
  
contains

  ! Create side-by-side comparison image from two bitmaps
  function ft_compare_create_side_by_side(bitmap1, bitmap2, output_filename, error) result(success)
    type(FT_Bitmap), intent(in) :: bitmap1, bitmap2
    character(len=*), intent(in) :: output_filename
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    type(FT_Bitmap) :: combined
    integer :: combined_width, combined_height
    integer :: x, y, src_offset, dst_offset
    integer :: separator_width = 2
    
    success = .false.
    error = FT_Err_Ok
    
    ! Calculate combined dimensions
    combined_width = bitmap1%width + separator_width + bitmap2%width
    combined_height = max(bitmap1%rows, bitmap2%rows)
    
    ! Create combined bitmap
    if (.not. ft_bitmap_new(combined_width, combined_height, bitmap1%pixel_mode, combined, error)) then
      return
    end if
    
    ! Clear bitmap (white background for mono, 0 for gray)
    call ft_bitmap_clear(combined)
    
    ! Copy first bitmap to left side
    do y = 0, bitmap1%rows - 1
      do x = 0, bitmap1%width - 1
        if (bitmap1%pixel_mode == FT_PIXEL_MODE_MONO) then
          if (ft_bitmap_get_pixel(bitmap1, x, y)) then
            call ft_bitmap_set_pixel(combined, x, y, .true.)
          end if
        else
          src_offset = y * abs(bitmap1%pitch) + x + 1
          dst_offset = y * abs(combined%pitch) + x + 1
          if (src_offset <= size(bitmap1%buffer) .and. dst_offset <= size(combined%buffer)) then
            combined%buffer(dst_offset) = bitmap1%buffer(src_offset)
          end if
        end if
      end do
    end do
    
    ! Draw separator line (if grayscale)
    if (bitmap1%pixel_mode == FT_PIXEL_MODE_GRAY) then
      do y = 0, combined_height - 1
        do x = bitmap1%width, bitmap1%width + separator_width - 1
          dst_offset = y * abs(combined%pitch) + x + 1
          if (dst_offset <= size(combined%buffer)) then
            combined%buffer(dst_offset) = int(-128, int8)  ! Gray separator (128 as signed int8)
          end if
        end do
      end do
    end if
    
    ! Copy second bitmap to right side
    do y = 0, bitmap2%rows - 1
      do x = 0, bitmap2%width - 1
        if (bitmap2%pixel_mode == FT_PIXEL_MODE_MONO) then
          if (ft_bitmap_get_pixel(bitmap2, x, y)) then
            call ft_bitmap_set_pixel(combined, x + bitmap1%width + separator_width, y, .true.)
          end if
        else
          src_offset = y * abs(bitmap2%pitch) + x + 1
          dst_offset = y * abs(combined%pitch) + (x + bitmap1%width + separator_width) + 1
          if (src_offset <= size(bitmap2%buffer) .and. dst_offset <= size(combined%buffer)) then
            combined%buffer(dst_offset) = bitmap2%buffer(src_offset)
          end if
        end if
      end do
    end do
    
    ! Write combined image
    if (index(output_filename, '.png') > 0) then
      success = ft_bitmap_write_png(combined, output_filename, error)
    else if (index(output_filename, '.pbm') > 0) then
      success = ft_bitmap_write_pbm(combined, output_filename, error)
    else if (index(output_filename, '.pgm') > 0) then
      success = ft_bitmap_write_pgm(combined, output_filename, error)
    else if (index(output_filename, '.ppm') > 0) then
      success = ft_bitmap_write_ppm(combined, output_filename, error)
    else
      error = FT_Err_Invalid_Argument
    end if
    
    ! Cleanup
    call ft_bitmap_done(combined)
    
  end function ft_compare_create_side_by_side
  
  ! Create difference image highlighting pixels that differ
  function ft_compare_create_diff_image(bitmap1, bitmap2, output_filename, error) result(success)
    type(FT_Bitmap), intent(in) :: bitmap1, bitmap2
    character(len=*), intent(in) :: output_filename
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    type(FT_Bitmap) :: diff_map
    integer :: width, height
    integer :: x, y
    logical :: pixel1, pixel2
    integer :: gray1, gray2, diff_val
    integer :: src_offset1, src_offset2, dst_offset
    
    success = .false.
    error = FT_Err_Ok
    
    ! Use maximum dimensions
    width = max(bitmap1%width, bitmap2%width)
    height = max(bitmap1%rows, bitmap2%rows)
    
    ! Create difference bitmap (always grayscale for better visualization)
    if (.not. ft_bitmap_new(width, height, FT_PIXEL_MODE_GRAY, diff_map, error)) then
      return
    end if
    
    ! Compare pixels
    do y = 0, height - 1
      do x = 0, width - 1
        ! Get pixel values from both bitmaps
        if (bitmap1%pixel_mode == FT_PIXEL_MODE_MONO) then
          pixel1 = .false.
          if (x < bitmap1%width .and. y < bitmap1%rows) then
            pixel1 = ft_bitmap_get_pixel(bitmap1, x, y)
          end if
          gray1 = merge(255, 0, pixel1)
        else
          gray1 = 0
          if (x < bitmap1%width .and. y < bitmap1%rows) then
            src_offset1 = y * abs(bitmap1%pitch) + x + 1
            if (src_offset1 <= size(bitmap1%buffer)) then
              gray1 = iand(int(bitmap1%buffer(src_offset1)), 255)
            end if
          end if
        end if
        
        if (bitmap2%pixel_mode == FT_PIXEL_MODE_MONO) then
          pixel2 = .false.
          if (x < bitmap2%width .and. y < bitmap2%rows) then
            pixel2 = ft_bitmap_get_pixel(bitmap2, x, y)
          end if
          gray2 = merge(255, 0, pixel2)
        else
          gray2 = 0
          if (x < bitmap2%width .and. y < bitmap2%rows) then
            src_offset2 = y * abs(bitmap2%pitch) + x + 1
            if (src_offset2 <= size(bitmap2%buffer)) then
              gray2 = iand(int(bitmap2%buffer(src_offset2)), 255)
            end if
          end if
        end if
        
        ! Calculate difference (0 = same, 255 = maximum difference)
        diff_val = abs(gray1 - gray2)
        
        ! Write to difference map
        dst_offset = y * abs(diff_map%pitch) + x + 1
        if (dst_offset <= size(diff_map%buffer)) then
          diff_map%buffer(dst_offset) = int(diff_val, int8)
        end if
      end do
    end do
    
    ! Write difference image
    if (index(output_filename, '.png') > 0) then
      success = ft_bitmap_write_png(diff_map, output_filename, error)
    else if (index(output_filename, '.pgm') > 0) then
      success = ft_bitmap_write_pgm(diff_map, output_filename, error)
    else
      error = FT_Err_Invalid_Argument
    end if
    
    ! Cleanup
    call ft_bitmap_done(diff_map)
    
  end function ft_compare_create_diff_image
  
  ! Calculate pixel difference statistics between two bitmaps
  function ft_compare_pixel_difference(bitmap1, bitmap2, total_pixels, different_pixels, error) result(success)
    type(FT_Bitmap), intent(in) :: bitmap1, bitmap2
    integer, intent(out) :: total_pixels, different_pixels
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: width, height
    integer :: x, y
    logical :: pixel1, pixel2
    integer :: gray1, gray2
    integer :: src_offset1, src_offset2
    
    success = .true.
    error = FT_Err_Ok
    total_pixels = 0
    different_pixels = 0
    
    ! Use maximum dimensions
    width = max(bitmap1%width, bitmap2%width)
    height = max(bitmap1%rows, bitmap2%rows)
    total_pixels = width * height
    
    ! Compare all pixels
    do y = 0, height - 1
      do x = 0, width - 1
        ! Get pixel from first bitmap
        if (x < bitmap1%width .and. y < bitmap1%rows) then
          if (bitmap1%pixel_mode == FT_PIXEL_MODE_MONO) then
            pixel1 = ft_bitmap_get_pixel(bitmap1, x, y)
            gray1 = merge(255, 0, pixel1)
          else
            src_offset1 = y * abs(bitmap1%pitch) + x + 1
            if (src_offset1 <= size(bitmap1%buffer)) then
              gray1 = iand(int(bitmap1%buffer(src_offset1)), 255)
            else
              gray1 = 0
            end if
          end if
        else
          gray1 = 0  ! Outside bounds
        end if
        
        ! Get pixel from second bitmap
        if (x < bitmap2%width .and. y < bitmap2%rows) then
          if (bitmap2%pixel_mode == FT_PIXEL_MODE_MONO) then
            pixel2 = ft_bitmap_get_pixel(bitmap2, x, y)
            gray2 = merge(255, 0, pixel2)
          else
            src_offset2 = y * abs(bitmap2%pitch) + x + 1
            if (src_offset2 <= size(bitmap2%buffer)) then
              gray2 = iand(int(bitmap2%buffer(src_offset2)), 255)
            else
              gray2 = 0
            end if
          end if
        else
          gray2 = 0  ! Outside bounds
        end if
        
        ! Count if different
        if (gray1 /= gray2) then
          different_pixels = different_pixels + 1
        end if
      end do
    end do
    
  end function ft_compare_pixel_difference

end module ft_compare