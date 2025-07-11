program test_visual_png
  use ft_types
  use ft_geometry
  use ft_bitmap_mod
  use ft_bitmap_io
  use ft_outline_mod
  use ft_scanline_simple
  use, intrinsic :: iso_fortran_env, only: int8
  implicit none
  
  type(FT_Bitmap) :: bitmap
  type(FT_Outline), target :: outline
  integer(FT_Error) :: error
  logical :: success
  integer :: x, y
  
  print '("Creating visual PNG test...")'
  
  ! Test 1: Create bitmap with cross pattern
  print '("Test 1: Cross pattern")'
  success = ft_bitmap_new(16, 16, FT_PIXEL_MODE_MONO, bitmap, error)
  if (success) then
    ! Draw a cross
    do y = 0, 15
      do x = 0, 15
        if (x == 7 .or. x == 8 .or. y == 7 .or. y == 8) then
          call ft_bitmap_set_pixel(bitmap, x, y, .true.)
        end if
      end do
    end do
    
    success = ft_bitmap_write_png(bitmap, "test_cross.png", error)
    if (success) print '("  Created: test_cross.png")'
    
    success = ft_bitmap_write_pbm(bitmap, "test_cross.pbm", error)
    if (success) print '("  Created: test_cross.pbm")'
    
    call ft_bitmap_done(bitmap)
  end if
  
  ! Test 2: Triangle with grayscale
  print '("Test 2: Triangle shape")'
  success = ft_outline_new(3, 1, outline, error)
  if (success) then
    ! Triangle
    outline%points(1) = FT_Vector(512, 512)    ! Top (8, 8)
    outline%points(2) = FT_Vector(896, 896)    ! Bottom-right (14, 14)
    outline%points(3) = FT_Vector(128, 896)    ! Bottom-left (2, 14)
    outline%contours(1) = 2
    outline%n_points = 3
    outline%n_contours = 1
    outline%tags = FT_CURVE_TAG_ON
    
    success = ft_bitmap_new(16, 16, FT_PIXEL_MODE_GRAY, bitmap, error)
    if (success) then
      success = ft_render_outline_filled(outline, bitmap, error)
      if (success) then
        success = ft_bitmap_write_png(bitmap, "test_triangle.png", error)
        if (success) print '("  Created: test_triangle.png")'
        
        success = ft_bitmap_write_pgm(bitmap, "test_triangle.pgm", error)
        if (success) print '("  Created: test_triangle.pgm")'
      end if
      call ft_bitmap_done(bitmap)
    end if
    call ft_outline_done(outline)
  end if
  
  ! Test 3: Larger bitmap with text-like pattern
  print '("Test 3: Text-like pattern")'
  success = ft_bitmap_new(32, 16, FT_PIXEL_MODE_GRAY, bitmap, error)
  if (success) then
    ! Create pattern that looks like "HI"
    ! Letter H
    do y = 2, 12
      do x = 2, 4
        bitmap%buffer(y * bitmap%pitch + x + 1) = 0_int8  ! Black
      end do
      do x = 8, 10
        bitmap%buffer(y * bitmap%pitch + x + 1) = 0_int8  ! Black
      end do
    end do
    do y = 6, 8
      do x = 4, 8
        bitmap%buffer(y * bitmap%pitch + x + 1) = 0_int8  ! Black
      end do
    end do
    
    ! Letter I
    do y = 2, 12
      do x = 14, 16
        bitmap%buffer(y * bitmap%pitch + x + 1) = 0_int8  ! Black
      end do
    end do
    
    success = ft_bitmap_write_png(bitmap, "test_hi.png", error)
    if (success) print '("  Created: test_hi.png")'
    
    call ft_bitmap_done(bitmap)
  end if
  
  print '("Visual PNG tests complete!")'

end program test_visual_png