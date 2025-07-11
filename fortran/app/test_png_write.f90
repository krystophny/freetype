program test_png_write
  use ft_types
  use ft_bitmap_mod
  use ft_bitmap_io
  use, intrinsic :: iso_fortran_env, only: int8
  implicit none
  
  type(FT_Bitmap) :: bitmap
  integer(FT_Error) :: error
  logical :: success
  integer :: x, y
  
  print '("Testing PNG writing...")'
  
  ! Create a small test bitmap with a pattern
  success = ft_bitmap_new(8, 8, FT_PIXEL_MODE_GRAY, bitmap, error)
  if (.not. success) then
    print '("Failed to create bitmap")'
    stop 1
  end if
  
  ! Create a gradient pattern
  do y = 0, 7
    do x = 0, 7
      ! Create a diagonal gradient
      bitmap%buffer(y * bitmap%pitch + x + 1) = int((x + y) * 17, int8)
    end do
  end do
  
  ! Write as PNG
  success = ft_bitmap_write_png(bitmap, "test_gradient.png", error)
  if (success) then
    print '("PNG written successfully")'
  else
    print '("Failed to write PNG")'
  end if
  
  ! Also write as PGM for comparison
  success = ft_bitmap_write_pgm(bitmap, "test_gradient.pgm", error)
  if (success) then
    print '("PGM written successfully")'
  end if
  
  ! Check the buffer content
  print '("Buffer content (first row):")'
  do x = 1, 8
    print '(I4)', iand(int(bitmap%buffer(x)), 255)
  end do
  
  call ft_bitmap_done(bitmap)

end program test_png_write