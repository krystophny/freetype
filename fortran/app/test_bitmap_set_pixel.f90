program test_bitmap_set_pixel
  use ft_bitmap_mod
  use ft_types
  implicit none
  
  type(FT_Bitmap) :: bitmap
  integer(FT_Error) :: error
  logical :: success
  integer :: i
  
  print *, "TEST: Bitmap Set Pixel Function"
  print *, "==============================="
  print *, ""
  
  ! Create a 8x8 grayscale bitmap
  success = ft_bitmap_new(8, 8, FT_PIXEL_MODE_GRAY, bitmap, error)
  if (.not. success) then
    print *, "Failed to create bitmap, error:", error
    stop
  end if
  
  print *, "Created 8x8 grayscale bitmap"
  print *, "  Width:", bitmap%width
  print *, "  Height:", bitmap%rows
  print *, "  Pixel mode:", bitmap%pixel_mode
  print *, "  Pitch:", bitmap%pitch
  print *, "  Buffer size:", size(bitmap%buffer)
  print *, ""
  
  ! Clear bitmap
  call ft_bitmap_clear(bitmap)
  
  ! Set some test pixels
  print *, "Setting test pixels..."
  call ft_bitmap_set_pixel_gray(bitmap, 0, 0, 255)  ! Top-left
  call ft_bitmap_set_pixel_gray(bitmap, 7, 7, 128)  ! Bottom-right
  call ft_bitmap_set_pixel_gray(bitmap, 3, 3, 64)   ! Center
  call ft_bitmap_set_pixel_gray(bitmap, 1, 1, 200)  ! Near top-left
  
  ! Check buffer contents
  print *, "Buffer contents after setting pixels:"
  do i = 1, size(bitmap%buffer)
    if (bitmap%buffer(i) /= 0) then
      print *, "  Buffer[", i-1, "] =", int(bitmap%buffer(i))
    end if
  end do
  
  ! Count non-zero pixels
  block
    integer :: non_zero = 0
    do i = 1, size(bitmap%buffer)
      if (bitmap%buffer(i) /= 0) non_zero = non_zero + 1
    end do
    print *, "  Non-zero pixels:", non_zero, "/", size(bitmap%buffer)
  end block
  
  ! Clean up
  call ft_bitmap_done(bitmap)
  
  print *, ""
  print *, "Test complete!"
  
end program test_bitmap_set_pixel