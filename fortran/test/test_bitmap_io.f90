program test_bitmap_io
  use ft_types
  use ft_geometry
  use ft_bitmap_mod
  use ft_bitmap_io
  use ft_outline_mod
  use ft_scanline_simple
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Run all tests
  call test_pbm_writer()
  call test_pgm_writer()
  call test_shape_rendering()
  call test_png_writer()
  
  ! Print test summary
  print '(/, "FT_Bitmap_IO Tests - Tests run: ", I0)', test_count
  print '("FT_Bitmap_IO Tests - Tests failed: ", I0)', failed_count
  
  if (failed_count == 0) then
    print '(/, "All FT_Bitmap_IO tests passed!")'
    stop 0
  else
    print '(/, "Some FT_Bitmap_IO tests failed!")'
    stop 1
  end if

contains

  subroutine test_pbm_writer()
    type(FT_Bitmap) :: bitmap
    integer(FT_Error) :: error
    logical :: success
    integer :: x, y
    logical :: file_exists
    
    print '(/, "Testing PBM file writer...")'
    
    ! Create test bitmap with pattern
    success = ft_bitmap_new(8, 8, FT_PIXEL_MODE_MONO, bitmap, error)
    if (.not. success) return
    
    ! Create checkerboard pattern
    do y = 0, 7
      do x = 0, 7
        if (mod(x + y, 2) == 0) then
          call ft_bitmap_set_pixel(bitmap, x, y, .true.)
        end if
      end do
    end do
    
    ! Write to PBM file
    test_count = test_count + 1
    success = ft_bitmap_write_pbm(bitmap, "test_checkerboard.pbm", error)
    
    if (success) then
      print '("PASS: PBM file written successfully")'
      
      ! Check if file exists
      inquire(file="test_checkerboard.pbm", exist=file_exists)
      test_count = test_count + 1
      if (file_exists) then
        print '("PASS: PBM file exists")'
      else
        print '("FAIL: PBM file not created")'
        failed_count = failed_count + 1
      end if
    else
      print '("FAIL: Could not write PBM file")'
      failed_count = failed_count + 1
    end if
    
    ! Cleanup
    call ft_bitmap_done(bitmap)
    
  end subroutine test_pbm_writer
  
  subroutine test_pgm_writer()
    type(FT_Bitmap) :: bitmap
    integer(FT_Error) :: error
    logical :: success
    integer :: x, y, gray
    logical :: file_exists
    
    print '(/, "Testing PGM file writer...")'
    
    ! Create grayscale bitmap with gradient
    success = ft_bitmap_new(16, 16, FT_PIXEL_MODE_GRAY, bitmap, error)
    if (.not. success) return
    
    ! Create gradient pattern
    do y = 0, 15
      do x = 0, 15
        gray = (x * 255) / 15
        if (associated(bitmap%buffer)) then
          bitmap%buffer(y * bitmap%pitch + x + 1) = int(gray, int8)
        end if
      end do
    end do
    
    ! Write to PGM file
    test_count = test_count + 1
    success = ft_bitmap_write_pgm(bitmap, "test_gradient.pgm", error)
    
    if (success) then
      print '("PASS: PGM file written successfully")'
      
      ! Check if file exists
      inquire(file="test_gradient.pgm", exist=file_exists)
      test_count = test_count + 1
      if (file_exists) then
        print '("PASS: PGM file exists")'
      else
        print '("FAIL: PGM file not created")'
        failed_count = failed_count + 1
      end if
    else
      print '("FAIL: Could not write PGM file")'
      failed_count = failed_count + 1
    end if
    
    ! Cleanup
    call ft_bitmap_done(bitmap)
    
  end subroutine test_pgm_writer
  
  subroutine test_shape_rendering()
    type(FT_Outline), target :: outline
    type(FT_Bitmap), target :: bitmap
    integer(FT_Error) :: error
    logical :: success
    logical :: file_exists
    
    print '(/, "Testing shape rendering to file...")'
    
    ! Create star outline
    success = ft_outline_new(10, 1, outline, error)
    if (.not. success) return
    
    ! Star points (alternating outer and inner vertices)
    ! Outer points
    outline%points(1) = FT_Vector(2048, 512)    ! Top
    outline%points(3) = FT_Vector(3328, 1536)   ! Top-right
    outline%points(5) = FT_Vector(2816, 2816)   ! Bottom-right
    outline%points(7) = FT_Vector(1280, 2816)   ! Bottom-left
    outline%points(9) = FT_Vector(768, 1536)    ! Top-left
    
    ! Inner points
    outline%points(2) = FT_Vector(2304, 1280)
    outline%points(4) = FT_Vector(2560, 1792)
    outline%points(6) = FT_Vector(2048, 2048)
    outline%points(8) = FT_Vector(1536, 1792)
    outline%points(10) = FT_Vector(1792, 1280)
    
    outline%contours(1) = 9  ! 10 points, ending at index 9
    outline%tags = FT_CURVE_TAG_ON
    
    ! Create bitmap
    success = ft_bitmap_new(16, 16, FT_PIXEL_MODE_MONO, bitmap, error)
    if (.not. success) then
      call ft_outline_done(outline)
      return
    end if
    
    ! Render star
    success = ft_render_outline_filled(outline, bitmap, error)
    
    if (success) then
      ! Write to PBM file
      test_count = test_count + 1
      success = ft_bitmap_write_pbm(bitmap, "test_star.pbm", error)
      
      if (success) then
        print '("PASS: Star shape rendered to file")'
        
        ! Also write as PPM for easier viewing
        success = ft_bitmap_write_ppm(bitmap, "test_star.ppm", error)
        test_count = test_count + 1
        if (success) then
          print '("PASS: PPM version also created")'
        else
          print '("FAIL: Could not create PPM version")'
          failed_count = failed_count + 1
        end if
      else
        print '("FAIL: Could not write star shape")'
        failed_count = failed_count + 1
      end if
    end if
    
    ! Cleanup
    call ft_outline_done(outline)
    call ft_bitmap_done(bitmap)
    
  end subroutine test_shape_rendering
  
  subroutine test_png_writer()
    type(FT_Outline), target :: outline
    type(FT_Bitmap), target :: bitmap
    integer(FT_Error) :: error
    logical :: success
    logical :: file_exists
    
    print '(/, "Testing PNG file writer...")'
    
    ! Create a nice shape - letter 'A' outline
    success = ft_outline_new(7, 2, outline, error)
    if (.not. success) return
    
    ! Outer contour of 'A'
    outline%points(1) = FT_Vector(512, 3584)    ! Bottom left
    outline%points(2) = FT_Vector(1024, 512)    ! Top left
    outline%points(3) = FT_Vector(1536, 512)    ! Top right
    outline%points(4) = FT_Vector(2048, 3584)   ! Bottom right
    
    ! Inner triangle (crossbar hole)
    outline%points(5) = FT_Vector(1024, 2048)   ! Left
    outline%points(6) = FT_Vector(1536, 2048)   ! Right
    outline%points(7) = FT_Vector(1280, 1536)   ! Top
    
    outline%contours(1) = 3  ! Outer contour
    outline%contours(2) = 6  ! Inner contour
    outline%tags = FT_CURVE_TAG_ON
    
    ! Create larger bitmap for better quality
    success = ft_bitmap_new(32, 32, FT_PIXEL_MODE_MONO, bitmap, error)
    if (.not. success) then
      call ft_outline_done(outline)
      return
    end if
    
    ! Render the letter
    success = ft_render_outline_filled(outline, bitmap, error)
    
    if (success) then
      ! Write to PNG file
      test_count = test_count + 1
      success = ft_bitmap_write_png(bitmap, "test_letter_A.png", error)
      
      if (success) then
        print '("PASS: PNG file written successfully")'
        
        ! Check if file exists
        inquire(file="test_letter_A.png", exist=file_exists)
        test_count = test_count + 1
        if (file_exists) then
          print '("PASS: PNG file exists")'
        else
          print '("FAIL: PNG file not created")'
          failed_count = failed_count + 1
        end if
        
        ! Also create a grayscale version
        call ft_bitmap_done(bitmap)
        success = ft_bitmap_new(32, 32, FT_PIXEL_MODE_GRAY, bitmap, error)
        if (success) then
          ! Simple gradient fill for testing
          call create_gradient_letter(bitmap)
          
          test_count = test_count + 1
          success = ft_bitmap_write_png(bitmap, "test_gradient_A.png", error)
          if (success) then
            print '("PASS: Grayscale PNG written")'
          else
            print '("FAIL: Could not write grayscale PNG")'
            failed_count = failed_count + 1
          end if
        end if
      else
        print '("FAIL: Could not write PNG file")'
        failed_count = failed_count + 1
      end if
    end if
    
    ! Cleanup
    call ft_outline_done(outline)
    call ft_bitmap_done(bitmap)
    
  end subroutine test_png_writer
  
  ! Helper to create a gradient effect
  subroutine create_gradient_letter(bitmap)
    type(FT_Bitmap), intent(inout) :: bitmap
    integer :: x, y, gray
    
    do y = 0, bitmap%rows - 1
      do x = 0, bitmap%width - 1
        ! Simple diagonal gradient
        gray = ((x + y) * 255) / (bitmap%width + bitmap%rows)
        if (associated(bitmap%buffer)) then
          bitmap%buffer(y * bitmap%pitch + x + 1) = int(gray, int8)
        end if
      end do
    end do
    
  end subroutine create_gradient_letter

end program test_bitmap_io