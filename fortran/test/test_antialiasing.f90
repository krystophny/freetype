program test_antialiasing
  use ft_types
  use ft_bitmap_mod
  use ft_outline_mod
  use ft_raster
  use ft_raster_types
  use ft_geometry, only: FT_Vector
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Run all tests
  call test_antialiased_line()
  call test_antialiased_triangle()
  call test_coverage_values()
  
  ! Print test summary
  print '(/, "Antialiasing Tests - Tests run: ", I0)', test_count
  print '("Antialiasing Tests - Tests failed: ", I0)', failed_count
  
  if (failed_count == 0) then
    print '(/, "All antialiasing tests passed!")'
    stop 0
  else
    print '(/, "Some antialiasing tests failed!")'
    stop 1
  end if

contains

  subroutine test_antialiased_line()
    type(FT_Outline), target :: outline
    type(FT_Bitmap), target :: bitmap
    type(FT_Raster_State) :: raster
    type(FT_Raster_Params) :: params
    integer(FT_Error) :: error
    logical :: success
    integer :: i, gray_count, total_pixels
    
    print '(/, "Testing antialiased line rendering...")'
    
    ! Create a simple square outline for testing (4 points)
    success = ft_outline_new(4, 1, outline, error)
    if (.not. success) then
      print '("FAIL: Could not create outline")'
      failed_count = failed_count + 1
      return
    end if
    
    ! Square from (1,1) to (3,3) in pixel units
    ! Convert to 26.6 fixed point
    outline%points(1)%x = 1 * 64
    outline%points(1)%y = 1 * 64
    outline%points(2)%x = 3 * 64
    outline%points(2)%y = 1 * 64
    outline%points(3)%x = 3 * 64
    outline%points(3)%y = 3 * 64
    outline%points(4)%x = 1 * 64
    outline%points(4)%y = 3 * 64
    
    outline%tags = FT_CURVE_TAG_ON
    
    outline%contours(1) = 3  ! End of first contour (0-based)
    outline%n_points = 4
    outline%n_contours = 1
    
    ! Create grayscale bitmap
    success = ft_bitmap_new(12, 12, FT_PIXEL_MODE_GRAY, bitmap, error)
    if (.not. success) then
      print '("FAIL: Could not create bitmap")'
      failed_count = failed_count + 1
      call ft_outline_done(outline)
      return
    end if
    
    ! Create rasterizer
    success = ft_raster_new(raster, error)
    if (.not. success) then
      print '("FAIL: Could not create rasterizer")'
      failed_count = failed_count + 1
      call ft_outline_done(outline)
      call ft_bitmap_done(bitmap)
      return
    end if
    
    ! Set outline in raster state
    raster%outline => outline
    
    ! Set up params
    params%target => bitmap
    params%flags = FT_RASTER_FLAG_AA  ! Enable antialiasing
    
    ! Render outline
    success = ft_raster_render_outline(raster, params, error)
    
    test_count = test_count + 1
    if (success) then
      ! Count all non-zero pixels
      gray_count = 0
      total_pixels = 0
      do i = 1, size(bitmap%buffer)
        if (bitmap%buffer(i) /= 0_int8) then
          total_pixels = total_pixels + 1
          ! 128 as signed int8 is -128, so check for values in middle range
          if (bitmap%buffer(i) /= -1_int8 .and. bitmap%buffer(i) /= 0_int8) then
            gray_count = gray_count + 1
          end if
        end if
      end do
      
      if (total_pixels > 0 .and. gray_count > 0) then
        print '("PASS: Rendered ", I0, " pixels (", I0, " gray, ", I0, " black)")', &
              total_pixels, gray_count, total_pixels - gray_count
      else
        print '("FAIL: No pixels rendered at all")'
        failed_count = failed_count + 1
      end if
    else
      print '("FAIL: Could not render antialiased line")'
      failed_count = failed_count + 1
    end if
    
    ! Cleanup
    call ft_raster_done(raster)
    call ft_bitmap_done(bitmap)
    call ft_outline_done(outline)
    
  end subroutine test_antialiased_line
  
  subroutine test_antialiased_triangle()
    type(FT_Outline), target :: outline
    type(FT_Bitmap), target :: bitmap
    type(FT_Raster_State) :: raster
    type(FT_Raster_Params) :: params
    integer(FT_Error) :: error
    logical :: success
    integer :: i, gray_count, black_count
    
    print '(/, "Testing antialiased triangle rendering...")'
    
    ! Create triangle outline
    success = ft_outline_new(3, 1, outline, error)
    if (.not. success) return
    
    ! Triangle vertices in 26.6 fixed point
    outline%points(1)%x = 5 * 64
    outline%points(1)%y = 2 * 64
    outline%points(2)%x = 2 * 64
    outline%points(2)%y = 8 * 64
    outline%points(3)%x = 8 * 64
    outline%points(3)%y = 8 * 64
    
    outline%tags = FT_CURVE_TAG_ON
    outline%contours(1) = 2  ! End of first contour
    outline%n_points = 3
    outline%n_contours = 1
    
    ! Create grayscale bitmap
    success = ft_bitmap_new(10, 10, FT_PIXEL_MODE_GRAY, bitmap, error)
    if (.not. success) then
      call ft_outline_done(outline)
      return
    end if
    
    ! Create and setup rasterizer
    success = ft_raster_new(raster, error)
    if (.not. success) then
      call ft_outline_done(outline)
      call ft_bitmap_done(bitmap)
      return
    end if
    
    ! Set outline in raster state
    raster%outline => outline
    
    params%target => bitmap
    params%flags = FT_RASTER_FLAG_AA
    
    ! Render
    success = ft_raster_render_outline(raster, params, error)
    
    test_count = test_count + 1
    if (success) then
      ! Count pixel types
      gray_count = 0
      black_count = 0
      do i = 1, size(bitmap%buffer)
        if (bitmap%buffer(i) == -1_int8) then  ! 255
          black_count = black_count + 1
        else if (bitmap%buffer(i) > 0_int8) then
          gray_count = gray_count + 1
        end if
      end do
      
      if (gray_count > 0) then
        print '("PASS: Triangle has ", I0, " gray and ", I0, " black pixels")', &
              gray_count, black_count
      else
        print '("FAIL: Triangle antialiasing incorrect")'
        print '("  Gray pixels: ", I0)', gray_count
        print '("  Black pixels: ", I0)', black_count
        failed_count = failed_count + 1
      end if
    else
      print '("FAIL: Could not render antialiased triangle")'
      failed_count = failed_count + 1
    end if
    
    ! Cleanup
    call ft_raster_done(raster)
    call ft_bitmap_done(bitmap)
    call ft_outline_done(outline)
    
  end subroutine test_antialiased_triangle
  
  subroutine test_coverage_values()
    type(FT_Bitmap), target :: bitmap
    integer(FT_Error) :: error
    logical :: success
    integer :: x, coverage
    
    print '(/, "Testing coverage value distribution...")'
    
    ! Create small grayscale bitmap
    success = ft_bitmap_new(10, 1, FT_PIXEL_MODE_GRAY, bitmap, error)
    if (.not. success) return
    
    ! Set pixels with different coverage values
    do x = 0, 9
      coverage = x * 25  ! 0, 25, 50, 75, 100, 125, 150, 175, 200, 225
      if (coverage > 255) coverage = 255
      call ft_bitmap_set_pixel_gray(bitmap, x, 0, coverage)
    end do
    
    test_count = test_count + 1
    ! Verify gradient
    if (bitmap%buffer(1) == 0_int8 .and. &
        bitmap%buffer(5) == 100_int8 .and. &
        bitmap%buffer(10) == -31_int8) then  ! 225 as signed int8
      print '("PASS: Coverage values correctly distributed")'
    else
      print '("FAIL: Coverage values incorrect")'
      failed_count = failed_count + 1
    end if
    
    call ft_bitmap_done(bitmap)
    
  end subroutine test_coverage_values

end program test_antialiasing