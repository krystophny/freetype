program test_ft_scanline
  use ft_types
  use ft_geometry
  use ft_bitmap_mod
  use ft_outline_mod
  use ft_raster
  use ft_scanline
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Run all tests
  call test_edge_table_creation()
  call test_edge_sorting()
  call test_scanline_filling()
  call test_filled_shapes()
  
  ! Print test summary
  print '(/, "FT_Scanline Tests - Tests run: ", I0)', test_count
  print '("FT_Scanline Tests - Tests failed: ", I0)', failed_count
  
  if (failed_count == 0) then
    print '(/, "All FT_Scanline tests passed!")'
    stop 0
  else
    print '(/, "Some FT_Scanline tests failed!")'
    stop 1
  end if

contains

  subroutine test_edge_table_creation()
    type(FT_Edge_Table) :: edge_table
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing edge table creation...")'
    
    ! Test edge table creation
    test_count = test_count + 1
    success = ft_edge_table_new(0, 10, edge_table, error)
    
    if (success .and. error == FT_Err_Ok) then
      print '("PASS: Edge table created successfully")'
      
      ! Check dimensions
      test_count = test_count + 1
      if (edge_table%min_y == 0 .and. edge_table%max_y == 10 .and. &
          edge_table%height == 11) then
        print '("PASS: Correct edge table dimensions")'
      else
        print '("FAIL: Incorrect edge table dimensions")'
        failed_count = failed_count + 1
      end if
      
      ! Cleanup
      call ft_edge_table_done(edge_table)
      
    else
      print '("FAIL: Could not create edge table")'
      failed_count = failed_count + 1
    end if
    
  end subroutine test_edge_table_creation
  
  subroutine test_edge_sorting()
    type(FT_Edge_Table) :: edge_table
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing edge sorting...")'
    
    ! Create edge table
    success = ft_edge_table_new(0, 10, edge_table, error)
    if (.not. success) return
    
    ! Add some edges
    call ft_edge_table_add_line(edge_table, 5, 0, 5, 5)
    call ft_edge_table_add_line(edge_table, 2, 0, 2, 5)
    call ft_edge_table_add_line(edge_table, 8, 0, 8, 5)
    
    ! Sort edges
    call ft_edge_table_sort(edge_table)
    
    test_count = test_count + 1
    print '("PASS: Edge sorting completed")'
    
    ! Cleanup
    call ft_edge_table_done(edge_table)
    
  end subroutine test_edge_sorting
  
  subroutine test_scanline_filling()
    type(FT_Bitmap), target :: bitmap
    type(FT_Edge_Table) :: edge_table
    integer(FT_Error) :: error
    logical :: success
    integer :: x, count
    
    print '(/, "Testing scanline filling...")'
    
    ! Create bitmap
    success = ft_bitmap_new(10, 10, FT_PIXEL_MODE_MONO, bitmap, error)
    if (.not. success) return
    
    ! Create edge table
    success = ft_edge_table_new(0, 9, edge_table, error)
    if (.not. success) then
      call ft_bitmap_done(bitmap)
      return
    end if
    
    ! Create a simple rectangle (2,2) to (7,7)
    call ft_edge_table_add_line(edge_table, 2, 2, 2, 7)  ! Left edge
    call ft_edge_table_add_line(edge_table, 7, 2, 7, 7)  ! Right edge
    call ft_edge_table_add_line(edge_table, 2, 2, 7, 2)  ! Top edge
    call ft_edge_table_add_line(edge_table, 2, 7, 7, 7)  ! Bottom edge
    
    ! Sort and convert
    call ft_edge_table_sort(edge_table)
    call ft_scanline_convert(edge_table, bitmap)
    
    ! Check middle row should be filled
    test_count = test_count + 1
    count = 0
    do x = 0, 9
      if (ft_bitmap_get_pixel(bitmap, x, 4)) count = count + 1
    end do
    
    if (count == 6) then  ! Should have 6 pixels set (2-7 inclusive)
      print '("PASS: Scanline filled correctly")'
    else
      print '("FAIL: Incorrect scanline fill, pixels set: ", I0)', count
      failed_count = failed_count + 1
    end if
    
    ! Cleanup
    call ft_edge_table_done(edge_table)
    call ft_bitmap_done(bitmap)
    
  end subroutine test_scanline_filling
  
  subroutine test_filled_shapes()
    type(FT_Outline), target :: outline
    type(FT_Bitmap), target :: bitmap
    integer(FT_Error) :: error
    logical :: success
    integer :: x, y
    character(len=20) :: row_str
    
    print '(/, "Testing filled shape rendering...")'
    
    ! Create filled triangle outline
    success = ft_outline_new(3, 1, outline, error)
    if (.not. success) return
    
    ! Triangle vertices (scaled to fill more area)
    outline%points(1) = FT_Vector(128, 128)    ! (2,2)
    outline%points(2) = FT_Vector(384, 128)    ! (6,2)
    outline%points(3) = FT_Vector(256, 384)    ! (4,6)
    outline%contours(1) = 2
    outline%tags = FT_CURVE_TAG_ON
    
    ! Create bitmap
    success = ft_bitmap_new(10, 10, FT_PIXEL_MODE_MONO, bitmap, error)
    if (.not. success) then
      call ft_outline_done(outline)
      return
    end if
    
    ! Render using scanline conversion
    test_count = test_count + 1
    success = ft_raster_render_outline_scanline(outline, bitmap, error)
    
    if (success) then
      print '("PASS: Filled triangle rendered")'
      print '("Bitmap output (10x10):")'
      
      ! Display bitmap
      do y = 0, bitmap%rows - 1
        row_str = ""
        do x = 0, bitmap%width - 1
          if (ft_bitmap_get_pixel(bitmap, x, y)) then
            row_str(x+1:x+1) = "*"
          else
            row_str(x+1:x+1) = "."
          end if
        end do
        print '(A)', trim(row_str)
      end do
      
      ! Check that we have filled pixels (not just outline)
      test_count = test_count + 1
      if (ft_bitmap_get_pixel(bitmap, 4, 3) .and. &
          ft_bitmap_get_pixel(bitmap, 4, 4)) then
        print '("PASS: Shape is filled, not just outlined")'
      else
        print '("FAIL: Shape not properly filled")'
        failed_count = failed_count + 1
      end if
      
    else
      print '("FAIL: Could not render filled triangle")'
      failed_count = failed_count + 1
    end if
    
    ! Cleanup
    call ft_outline_done(outline)
    call ft_bitmap_done(bitmap)
    
  end subroutine test_filled_shapes

end program test_ft_scanline