program test_ft_raster
  use ft_types
  use ft_geometry
  use ft_bitmap_mod
  use ft_outline_mod
  use ft_raster_types
  use ft_raster
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Run all tests
  call test_raster_creation()
  call test_raster_reset()
  call test_raster_cells()
  call test_raster_render()
  
  ! Print test summary
  print '(/, "FT_Raster Tests - Tests run: ", I0)', test_count
  print '("FT_Raster Tests - Tests failed: ", I0)', failed_count
  
  if (failed_count == 0) then
    print '(/, "All FT_Raster tests passed!")'
    stop 0
  else
    print '(/, "Some FT_Raster tests failed!")'
    stop 1
  end if

contains

  subroutine test_raster_creation()
    type(FT_Raster_State) :: raster
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing rasterizer creation...")'
    
    ! Test rasterizer creation
    test_count = test_count + 1
    success = ft_raster_new(raster, error)
    
    if (success .and. error == FT_Err_Ok) then
      print '("PASS: Rasterizer created successfully")'
      
      ! Check cell pool allocation
      test_count = test_count + 1
      if (associated(raster%cells) .and. raster%num_cells > 0) then
        print '("PASS: Cell pool allocated")'
      else
        print '("FAIL: Cell pool not allocated")'
        failed_count = failed_count + 1
      end if
      
      ! Cleanup
      call ft_raster_done(raster)
      
      test_count = test_count + 1
      if (.not. associated(raster%cells)) then
        print '("PASS: Memory freed correctly")'
      else
        print '("FAIL: Memory not freed correctly")'
        failed_count = failed_count + 1
      end if
      
    else
      print '("FAIL: Could not create rasterizer")'
      failed_count = failed_count + 1
    end if
    
  end subroutine test_raster_creation
  
  subroutine test_raster_reset()
    type(FT_Raster_State) :: raster
    type(FT_Outline), target :: outline
    type(FT_Bitmap), target :: bitmap
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing rasterizer reset...")'
    
    ! Create test outline (square)
    success = ft_outline_new(4, 1, outline, error)
    if (.not. success) return
    
    outline%points(1) = FT_Vector(0, 0)
    outline%points(2) = FT_Vector(64, 0)       ! 1 pixel in 26.6 fixed
    outline%points(3) = FT_Vector(64, 64)
    outline%points(4) = FT_Vector(0, 64)
    outline%contours(1) = 3
    
    ! Create bitmap
    success = ft_bitmap_new(10, 10, FT_PIXEL_MODE_MONO, bitmap, error)
    if (.not. success) then
      call ft_outline_done(outline)
      return
    end if
    
    ! Create rasterizer
    success = ft_raster_new(raster, error)
    if (.not. success) then
      call ft_outline_done(outline)
      call ft_bitmap_done(bitmap)
      return
    end if
    
    ! Reset rasterizer
    test_count = test_count + 1
    success = ft_raster_reset(raster, outline, bitmap, error)
    
    if (success) then
      print '("PASS: Rasterizer reset successfully")'
      
      ! Check bounds calculation
      test_count = test_count + 1
      if (raster%min_ex == 0 .and. raster%max_ex == 1 .and. &
          raster%min_ey == 0 .and. raster%max_ey == 1) then
        print '("PASS: Correct pixel bounds")'
      else
        print '("FAIL: Incorrect pixel bounds")'
        print '("Expected: (0,0)-(1,1), Got: (", I0, ",", I0, ")-(", I0, ",", I0, ")")', &
          raster%min_ex, raster%min_ey, raster%max_ex, raster%max_ey
        failed_count = failed_count + 1
      end if
      
      ! Check ycells allocation
      test_count = test_count + 1
      if (associated(raster%ycells) .and. raster%count_ey == 1) then
        print '("PASS: Y-cells allocated correctly")'
      else
        print '("FAIL: Y-cells not allocated correctly")'
        failed_count = failed_count + 1
      end if
      
    else
      print '("FAIL: Could not reset rasterizer")'
      failed_count = failed_count + 1
    end if
    
    ! Cleanup
    call ft_raster_done(raster)
    call ft_outline_done(outline)
    call ft_bitmap_done(bitmap)
    
  end subroutine test_raster_reset
  
  subroutine test_raster_cells()
    type(FT_Raster_State) :: raster
    type(FT_Outline), target :: outline
    type(FT_Bitmap), target :: bitmap
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing rasterizer cell management...")'
    
    ! Create simple outline
    success = ft_outline_new(2, 1, outline, error)
    if (.not. success) return
    
    outline%points(1) = FT_Vector(0, 0)
    outline%points(2) = FT_Vector(512, 512)  ! 2 pixels diagonal
    outline%contours(1) = 1
    
    ! Create bitmap
    success = ft_bitmap_new(5, 5, FT_PIXEL_MODE_MONO, bitmap, error)
    if (.not. success) then
      call ft_outline_done(outline)
      return
    end if
    
    ! Create and reset rasterizer
    success = ft_raster_new(raster, error)
    if (.not. success) then
      call ft_outline_done(outline)
      call ft_bitmap_done(bitmap)
      return
    end if
    
    success = ft_raster_reset(raster, outline, bitmap, error)
    if (.not. success) then
      call ft_raster_done(raster)
      call ft_outline_done(outline)
      call ft_bitmap_done(bitmap)
      return
    end if
    
    ! Test cell creation
    call ft_raster_set_cell(raster, 0, 0)
    
    test_count = test_count + 1
    if (associated(raster%cell) .and. raster%cell%x == 0) then
      print '("PASS: Cell created at correct position")'
    else
      print '("FAIL: Cell not created correctly")'
      failed_count = failed_count + 1
    end if
    
    ! Test duplicate cell
    call ft_raster_set_cell(raster, 0, 0)
    
    test_count = test_count + 1
    if (raster%cell_index == 2) then
      print '("PASS: Duplicate cell reused")'
    else
      print '("FAIL: Duplicate cell created")'
      failed_count = failed_count + 1
    end if
    
    ! Cleanup
    call ft_raster_done(raster)
    call ft_outline_done(outline)
    call ft_bitmap_done(bitmap)
    
  end subroutine test_raster_cells
  
  subroutine test_raster_render()
    type(FT_Raster_State) :: raster
    type(FT_Outline), target :: outline
    type(FT_Bitmap), target :: bitmap
    type(FT_Raster_Params) :: params
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing basic rendering...")'
    
    ! Create triangle outline
    success = ft_outline_new(3, 1, outline, error)
    if (.not. success) return
    
    outline%points(1) = FT_Vector(256, 256)    ! (1,1) in pixels
    outline%points(2) = FT_Vector(768, 256)    ! (3,1)
    outline%points(3) = FT_Vector(512, 768)    ! (2,3)
    outline%contours(1) = 2
    
    ! Create bitmap
    success = ft_bitmap_new(5, 5, FT_PIXEL_MODE_MONO, bitmap, error)
    if (.not. success) then
      call ft_outline_done(outline)
      return
    end if
    
    ! Create rasterizer
    success = ft_raster_new(raster, error)
    if (.not. success) then
      call ft_outline_done(outline)
      call ft_bitmap_done(bitmap)
      return
    end if
    
    ! Set up rasterizer state
    raster%outline => outline
    
    ! Set up render params
    params%target => bitmap
    params%flags = 0  ! Default flags
    
    ! Render outline
    test_count = test_count + 1
    success = ft_raster_render_outline(raster, params, error)
    
    if (success) then
      print '("PASS: Rendering completed without error")'
    else
      print '("FAIL: Rendering failed with error: ", I0)', error
      failed_count = failed_count + 1
    end if
    
    ! Cleanup
    call ft_raster_done(raster)
    call ft_outline_done(outline)
    call ft_bitmap_done(bitmap)
    
  end subroutine test_raster_render

end program test_ft_raster