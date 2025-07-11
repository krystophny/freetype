program test_rendering_performance
  use ft_types
  use ft_bitmap_mod
  use ft_outline_mod
  use ft_raster
  use ft_raster_types
  use ft_geometry, only: FT_Vector
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real64
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Run performance tests
  call test_basic_rendering_speed()
  call test_antialiasing_overhead()
  call test_memory_allocation_speed()
  call test_different_sizes()
  
  ! Print test summary
  print '(/, "Rendering Performance Tests - Tests run: ", I0)', test_count
  print '("Rendering Performance Tests - Tests failed: ", I0)', failed_count
  
  if (failed_count == 0) then
    print '(/, "All rendering performance tests passed!")'
    stop 0
  else
    print '(/, "Some rendering performance tests failed!")'
    stop 1
  end if

contains

  subroutine test_basic_rendering_speed()
    integer, parameter :: ITERATIONS = 500
    type(FT_Outline), target :: outline
    type(FT_Bitmap), target :: bitmap
    type(FT_Raster_State) :: raster
    type(FT_Raster_Params) :: params
    integer(FT_Error) :: error
    logical :: success
    integer :: i
    integer(int64) :: start_time, end_time, freq
    real(real64) :: elapsed_time, fps
    
    print '(/, "Testing basic rendering speed...")'
    
    ! Create simple rectangle outline
    success = ft_outline_new(4, 1, outline, error)
    if (.not. success) return
    
    outline%points(1)%x = 10 * 64
    outline%points(1)%y = 10 * 64
    outline%points(2)%x = 30 * 64
    outline%points(2)%y = 10 * 64
    outline%points(3)%x = 30 * 64
    outline%points(3)%y = 30 * 64
    outline%points(4)%x = 10 * 64
    outline%points(4)%y = 30 * 64
    
    outline%tags = FT_CURVE_TAG_ON
    outline%contours(1) = 3
    outline%n_points = 4
    outline%n_contours = 1
    
    ! Create bitmap and rasterizer
    success = ft_bitmap_new(64, 64, FT_PIXEL_MODE_MONO, bitmap, error)
    if (.not. success) then
      call ft_outline_done(outline)
      return
    end if
    
    success = ft_raster_new(raster, error)
    if (.not. success) then
      call ft_outline_done(outline)
      call ft_bitmap_done(bitmap)
      return
    end if
    
    raster%outline => outline
    params%target => bitmap
    params%flags = 0
    
    ! Benchmark
    call system_clock(count_rate=freq)
    call system_clock(start_time)
    
    do i = 1, ITERATIONS
      success = ft_raster_render_outline(raster, params, error)
      if (.not. success) exit
    end do
    
    call system_clock(end_time)
    elapsed_time = real(end_time - start_time, real64) / real(freq, real64)
    fps = real(ITERATIONS, real64) / elapsed_time
    
    test_count = test_count + 1
    if (success .and. fps > 1000.0_real64) then
      print '("PASS: Rendered ", I0, " glyphs in ", F6.3, "s (", F8.1, " fps)")', &
            ITERATIONS, elapsed_time, fps
    else
      print '("FAIL: Performance too slow: ", F8.1, " fps")', fps
      failed_count = failed_count + 1
    end if
    
    ! Cleanup
    call ft_raster_done(raster)
    call ft_bitmap_done(bitmap)
    call ft_outline_done(outline)
    
  end subroutine test_basic_rendering_speed
  
  subroutine test_antialiasing_overhead()
    integer, parameter :: ITERATIONS = 200
    type(FT_Outline), target :: outline
    type(FT_Bitmap), target :: bitmap
    type(FT_Raster_State) :: raster
    type(FT_Raster_Params) :: params
    integer(FT_Error) :: error
    logical :: success
    integer :: i
    integer(int64) :: start_time, end_time, freq
    real(real64) :: mono_time, aa_time, overhead
    
    print '(/, "Testing antialiasing overhead...")'
    
    ! Create test outline
    success = ft_outline_new(4, 1, outline, error)
    if (.not. success) return
    
    outline%points(1)%x = 5 * 64
    outline%points(1)%y = 5 * 64
    outline%points(2)%x = 25 * 64
    outline%points(2)%y = 5 * 64
    outline%points(3)%x = 25 * 64
    outline%points(3)%y = 25 * 64
    outline%points(4)%x = 5 * 64
    outline%points(4)%y = 25 * 64
    
    outline%tags = FT_CURVE_TAG_ON
    outline%contours(1) = 3
    outline%n_points = 4
    outline%n_contours = 1
    
    success = ft_raster_new(raster, error)
    if (.not. success) then
      call ft_outline_done(outline)
      return
    end if
    raster%outline => outline
    
    call system_clock(count_rate=freq)
    
    ! Test monochrome rendering
    success = ft_bitmap_new(32, 32, FT_PIXEL_MODE_MONO, bitmap, error)
    if (.not. success) then
      call ft_outline_done(outline)
      call ft_raster_done(raster)
      return
    end if
    
    params%target => bitmap
    params%flags = 0  ! Monochrome
    
    call system_clock(start_time)
    do i = 1, ITERATIONS
      success = ft_raster_render_outline(raster, params, error)
      if (.not. success) exit
    end do
    call system_clock(end_time)
    mono_time = real(end_time - start_time, real64) / real(freq, real64)
    
    call ft_bitmap_done(bitmap)
    
    ! Test antialiased rendering
    success = ft_bitmap_new(32, 32, FT_PIXEL_MODE_GRAY, bitmap, error)
    if (.not. success) then
      call ft_outline_done(outline)
      call ft_raster_done(raster)
      return
    end if
    
    params%target => bitmap
    params%flags = FT_RASTER_FLAG_AA  ! Antialiased
    
    call system_clock(start_time)
    do i = 1, ITERATIONS
      success = ft_raster_render_outline(raster, params, error)
      if (.not. success) exit
    end do
    call system_clock(end_time)
    aa_time = real(end_time - start_time, real64) / real(freq, real64)
    
    if (mono_time > 0.0_real64) then
      overhead = (aa_time - mono_time) / mono_time * 100.0_real64
    else
      overhead = 0.0_real64
    end if
    
    test_count = test_count + 1
    if (success .and. overhead < 500.0_real64) then  ! Allow up to 5x overhead
      print '("PASS: AA overhead: ", F5.1, "% (mono: ", F6.3, "s, aa: ", F6.3, "s)")', &
            overhead, mono_time, aa_time
    else
      print '("FAIL: AA overhead too high: ", F5.1, "%")', overhead
      failed_count = failed_count + 1
    end if
    
    ! Cleanup
    call ft_bitmap_done(bitmap)
    call ft_raster_done(raster)
    call ft_outline_done(outline)
    
  end subroutine test_antialiasing_overhead
  
  subroutine test_memory_allocation_speed()
    integer, parameter :: NUM_ALLOCS = 1000
    type(FT_Bitmap) :: bitmaps(NUM_ALLOCS)
    integer(FT_Error) :: error
    logical :: success
    integer :: i, successful_allocs
    integer(int64) :: start_time, end_time, freq
    real(real64) :: elapsed_time, allocs_per_sec
    
    print '(/, "Testing memory allocation speed...")'
    
    call system_clock(count_rate=freq)
    call system_clock(start_time)
    
    successful_allocs = 0
    do i = 1, NUM_ALLOCS
      success = ft_bitmap_new(32, 32, FT_PIXEL_MODE_GRAY, bitmaps(i), error)
      if (success) then
        successful_allocs = successful_allocs + 1
      else
        exit
      end if
    end do
    
    call system_clock(end_time)
    elapsed_time = real(end_time - start_time, real64) / real(freq, real64)
    
    if (elapsed_time > 0.0_real64) then
      allocs_per_sec = real(successful_allocs, real64) / elapsed_time
    else
      allocs_per_sec = 0.0_real64
    end if
    
    test_count = test_count + 1
    if (successful_allocs == NUM_ALLOCS .and. allocs_per_sec > 10000.0_real64) then
      print '("PASS: ", I0, " allocations in ", F6.3, "s (", F8.1, " allocs/sec)")', &
            successful_allocs, elapsed_time, allocs_per_sec
    else
      print '("FAIL: Allocation speed too slow: ", F8.1, " allocs/sec")', allocs_per_sec
      failed_count = failed_count + 1
    end if
    
    ! Cleanup
    do i = 1, successful_allocs
      call ft_bitmap_done(bitmaps(i))
    end do
    
  end subroutine test_memory_allocation_speed
  
  subroutine test_different_sizes()
    integer, parameter :: ITERATIONS = 100
    integer, parameter :: NUM_SIZES = 4
    integer :: sizes(NUM_SIZES) = [16, 32, 64, 128]
    type(FT_Outline), target :: outline
    type(FT_Bitmap), target :: bitmap
    type(FT_Raster_State) :: raster
    type(FT_Raster_Params) :: params
    integer(FT_Error) :: error
    logical :: success
    integer :: i, j, size_idx
    integer(int64) :: start_time, end_time, freq
    real(real64) :: elapsed_time, fps
    
    print '(/, "Testing different bitmap sizes...")'
    
    ! Create test outline
    success = ft_outline_new(4, 1, outline, error)
    if (.not. success) return
    
    success = ft_raster_new(raster, error)
    if (.not. success) then
      call ft_outline_done(outline)
      return
    end if
    
    call system_clock(count_rate=freq)
    
    do size_idx = 1, NUM_SIZES
      ! Scale outline to fit bitmap size
      outline%points(1)%x = (sizes(size_idx) / 4) * 64
      outline%points(1)%y = (sizes(size_idx) / 4) * 64
      outline%points(2)%x = (sizes(size_idx) * 3 / 4) * 64
      outline%points(2)%y = (sizes(size_idx) / 4) * 64
      outline%points(3)%x = (sizes(size_idx) * 3 / 4) * 64
      outline%points(3)%y = (sizes(size_idx) * 3 / 4) * 64
      outline%points(4)%x = (sizes(size_idx) / 4) * 64
      outline%points(4)%y = (sizes(size_idx) * 3 / 4) * 64
      
      outline%tags = FT_CURVE_TAG_ON
      outline%contours(1) = 3
      outline%n_points = 4
      outline%n_contours = 1
      
      raster%outline => outline
      
      success = ft_bitmap_new(sizes(size_idx), sizes(size_idx), FT_PIXEL_MODE_MONO, bitmap, error)
      if (.not. success) cycle
      
      params%target => bitmap
      params%flags = 0
      
      call system_clock(start_time)
      do i = 1, ITERATIONS
        success = ft_raster_render_outline(raster, params, error)
        if (.not. success) exit
      end do
      call system_clock(end_time)
      
      elapsed_time = real(end_time - start_time, real64) / real(freq, real64)
      if (elapsed_time > 0.0_real64) then
        fps = real(ITERATIONS, real64) / elapsed_time
      else
        fps = 0.0_real64
      end if
      
      test_count = test_count + 1
      if (success .and. fps > 100.0_real64) then
        print '("PASS: ", I0, "x", I0, " bitmap: ", F8.1, " fps")', &
              sizes(size_idx), sizes(size_idx), fps
      else
        print '("FAIL: ", I0, "x", I0, " bitmap too slow: ", F8.1, " fps")', &
              sizes(size_idx), sizes(size_idx), fps
        failed_count = failed_count + 1
      end if
      
      call ft_bitmap_done(bitmap)
    end do
    
    ! Cleanup
    call ft_raster_done(raster)
    call ft_outline_done(outline)
    
  end subroutine test_different_sizes

end program test_rendering_performance