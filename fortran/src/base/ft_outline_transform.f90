module ft_outline_transform_mod
  use ft_types
  use ft_outline_mod
  use ft_geometry, only: FT_Vector
  use, intrinsic :: iso_fortran_env, only: int64
  implicit none
  private

  public :: ft_outline_transform_to_bitmap
  public :: ft_outline_get_bbox_transform

contains

  ! Transform outline coordinates from font units to bitmap pixel coordinates
  function ft_outline_transform_to_bitmap(input_outline, output_outline, &
                                        units_per_em, bitmap_width, bitmap_height, error) result(success)
    type(FT_Outline), intent(in) :: input_outline
    type(FT_Outline), intent(out) :: output_outline
    integer, intent(in) :: units_per_em, bitmap_width, bitmap_height
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    real :: scale_factor, center_x, center_y
    integer :: i, glyph_width, glyph_height, target_size
    integer :: offset_x, offset_y
    integer(int64) :: min_x, max_x, min_y, max_y
    
    success = .false.
    error = FT_Err_Ok
    
    ! Handle empty outline
    if (input_outline%n_points == 0) then
      success = ft_outline_new(0, 0, output_outline, error)
      return
    end if
    
    ! Find bounding box of input outline
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
    
    ! Calculate glyph dimensions
    glyph_width = max_x - min_x
    glyph_height = max_y - min_y
    
    ! Calculate scale factor to fit in bitmap (use 80% of bitmap for margin)
    target_size = min(bitmap_width, bitmap_height)
    if (glyph_width > 0 .and. glyph_height > 0) then
      scale_factor = real(target_size) * 0.8 / real(max(glyph_width, glyph_height))
    else
      scale_factor = 1.0
    end if
    
    ! Calculate centering offset
    center_x = real(bitmap_width) / 2.0
    center_y = real(bitmap_height) / 2.0
    offset_x = int(center_x - (real(min_x + max_x) / 2.0) * scale_factor)
    offset_y = int(center_y - (real(min_y + max_y) / 2.0) * scale_factor)
    
    ! Create output outline
    success = ft_outline_new(int(input_outline%n_points), int(input_outline%n_contours), output_outline, error)
    if (.not. success) return
    
    ! Transform points
    do i = 1, input_outline%n_points
      ! Scale and translate X coordinate
      output_outline%points(i)%x = int(real(input_outline%points(i)%x) * scale_factor) + offset_x
      
      ! Scale, translate, and flip Y coordinate (font Y up -> bitmap Y down)
      output_outline%points(i)%y = bitmap_height - &
                                   (int(real(input_outline%points(i)%y) * scale_factor) + offset_y)
      
      ! Copy tags
      output_outline%tags(i) = input_outline%tags(i)
    end do
    
    ! Copy contours
    do i = 1, input_outline%n_contours
      output_outline%contours(i) = input_outline%contours(i)
    end do
    
    success = .true.
    
  end function ft_outline_transform_to_bitmap

  ! Get bounding box of outline
  subroutine ft_outline_get_bbox_transform(outline, min_x, min_y, max_x, max_y)
    type(FT_Outline), intent(in) :: outline
    integer(int64), intent(out) :: min_x, min_y, max_x, max_y
    
    integer :: i
    
    if (outline%n_points == 0) then
      min_x = 0; max_x = 0; min_y = 0; max_y = 0
      return
    end if
    
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
    
  end subroutine ft_outline_get_bbox_transform

end module ft_outline_transform_mod