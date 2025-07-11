program profile_antialiasing
  use ft_types
  use ft_bitmap_mod
  use ft_outline_mod
  use ft_raster
  use ft_raster_types
  use ft_geometry, only: FT_Vector
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real64
  implicit none
  
  integer, parameter :: ITERATIONS = 100
  integer, parameter :: BITMAP_SIZE = 64
  
  ! Timing variables
  integer(int64) :: start_time, end_time, freq
  real(real64) :: total_time, setup_time, render_time, cleanup_time
  
  ! Test data
  type(FT_Outline), target :: outline
  type(FT_Bitmap), target :: bitmap
  type(FT_Raster_State) :: raster
  type(FT_Raster_Params) :: params
  integer(FT_Error) :: error
  logical :: success
  integer :: i
  
  print '("Antialiasing Performance Profiler")'
  print '("=================================")'
  print '()'
  
  call system_clock(count_rate=freq)
  
  ! Profile setup overhead
  call system_clock(start_time)
  
  ! Create test outline
  success = ft_outline_new(4, 1, outline, error)
  if (.not. success) stop 1
  
  outline%points(1)%x = 16 * 64
  outline%points(1)%y = 16 * 64
  outline%points(2)%x = 48 * 64
  outline%points(2)%y = 16 * 64
  outline%points(3)%x = 48 * 64
  outline%points(3)%y = 48 * 64
  outline%points(4)%x = 16 * 64
  outline%points(4)%y = 48 * 64
  
  outline%tags = FT_CURVE_TAG_ON
  outline%contours(1) = 3
  outline%n_points = 4
  outline%n_contours = 1
  
  ! Create bitmap and rasterizer
  success = ft_bitmap_new(BITMAP_SIZE, BITMAP_SIZE, FT_PIXEL_MODE_GRAY, bitmap, error)
  if (.not. success) stop 1
  
  success = ft_raster_new(raster, error)
  if (.not. success) stop 1
  
  raster%outline => outline
  params%target => bitmap
  params%flags = FT_RASTER_FLAG_AA
  
  call system_clock(end_time)
  setup_time = real(end_time - start_time, real64) / real(freq, real64)
  
  ! Profile rendering loop
  call system_clock(start_time)
  
  do i = 1, ITERATIONS
    success = ft_raster_render_outline(raster, params, error)
    if (.not. success) then
      print '("ERROR: Render failed at iteration ", I0)', i
      exit
    end if
  end do
  
  call system_clock(end_time)
  render_time = real(end_time - start_time, real64) / real(freq, real64)
  
  ! Profile cleanup
  call system_clock(start_time)
  call ft_raster_done(raster)
  call ft_bitmap_done(bitmap)
  call ft_outline_done(outline)
  call system_clock(end_time)
  cleanup_time = real(end_time - start_time, real64) / real(freq, real64)
  
  total_time = setup_time + render_time + cleanup_time
  
  ! Print profiling results
  print '("Profiling Results (", I0, " iterations):")', ITERATIONS
  print '("=========================================")'
  print '("Setup time:    ", F8.3, " ms (", F5.1, "%)")', &
        setup_time * 1000, setup_time / total_time * 100
  print '("Render time:   ", F8.3, " ms (", F5.1, "%)")', &
        render_time * 1000, render_time / total_time * 100
  print '("Cleanup time:  ", F8.3, " ms (", F5.1, "%)")', &
        cleanup_time * 1000, cleanup_time / total_time * 100
  print '("Total time:    ", F8.3, " ms")', total_time * 1000
  print '()'
  print '("Performance:")'
  print '("Shapes/second: ", F10.1)', real(ITERATIONS, real64) / render_time
  print '("Time per shape:", F8.3, " μs")', render_time / real(ITERATIONS, real64) * 1000000
  print '()'
  print '("Optimization Targets:")'
  print '("1. Focus on render loop optimization (", F5.1, "% of time)")', &
        render_time / total_time * 100
  if (setup_time / total_time > 0.1) then
    print '("2. Consider setup optimization (", F5.1, "% of time)")', &
          setup_time / total_time * 100
  end if
  
  ! Test different optimization approaches
  call test_optimizations()

contains

  subroutine test_optimizations()
    print '(/, "Testing Optimization Approaches:")'
    print '("=================================")'
    
    ! Test 1: Reduced iterations to see overhead
    call test_reduced_complexity()
    
    ! Test 2: Monochrome comparison
    call test_monochrome_comparison()
    
  end subroutine test_optimizations
  
  subroutine test_reduced_complexity()
    integer, parameter :: SMALL_ITER = 10
    real(real64) :: elapsed
    
    print '(/, "Test 1: Overhead Analysis")'
    print '("--------------------------")'
    
    ! Setup again for small test
    success = ft_outline_new(4, 1, outline, error)
    success = ft_bitmap_new(BITMAP_SIZE, BITMAP_SIZE, FT_PIXEL_MODE_GRAY, bitmap, error)
    success = ft_raster_new(raster, error)
    
    raster%outline => outline
    params%target => bitmap
    params%flags = FT_RASTER_FLAG_AA
    
    ! Time small number of iterations
    call system_clock(start_time)
    do i = 1, SMALL_ITER
      success = ft_raster_render_outline(raster, params, error)
    end do
    call system_clock(end_time)
    
    elapsed = real(end_time - start_time, real64) / real(freq, real64)
    
    print '("Small test (", I0, " iter): ", F8.3, " ms")', SMALL_ITER, elapsed * 1000
    print '("Per iteration: ", F8.3, " μs")', elapsed / real(SMALL_ITER, real64) * 1000000
    print '("Overhead factor: ", F6.2, "x vs large test")', &
          (elapsed / real(SMALL_ITER, real64)) / (render_time / real(ITERATIONS, real64))
    
    call ft_raster_done(raster)
    call ft_bitmap_done(bitmap)
    call ft_outline_done(outline)
    
  end subroutine test_reduced_complexity
  
  subroutine test_monochrome_comparison()
    real(real64) :: elapsed
    
    print '(/, "Test 2: Monochrome vs AA Comparison")'
    print '("------------------------------------")'
    
    ! Setup monochrome test
    success = ft_outline_new(4, 1, outline, error)
    success = ft_bitmap_new(BITMAP_SIZE, BITMAP_SIZE, FT_PIXEL_MODE_MONO, bitmap, error)
    success = ft_raster_new(raster, error)
    
    raster%outline => outline
    params%target => bitmap
    params%flags = 0  ! Monochrome
    
    ! Time monochrome rendering
    call system_clock(start_time)
    do i = 1, ITERATIONS
      success = ft_raster_render_outline(raster, params, error)
    end do
    call system_clock(end_time)
    
    elapsed = real(end_time - start_time, real64) / real(freq, real64)
    
    print '("Monochrome time: ", F8.3, " ms")', elapsed * 1000
    print '("AA time:         ", F8.3, " ms")', render_time * 1000
    print '("AA overhead:     ", F6.1, "x slower")', render_time / elapsed
    print '("Target ratio:    2-3x (C FreeType typical)")'
    
    if (render_time / elapsed > 5.0) then
      print '("CONCLUSION: Significant AA optimization needed")'
    else if (render_time / elapsed > 3.0) then
      print '("CONCLUSION: Moderate AA optimization needed")'
    else
      print '("CONCLUSION: AA overhead is reasonable")'
    end if
    
    call ft_raster_done(raster)
    call ft_bitmap_done(bitmap)
    call ft_outline_done(outline)
    
  end subroutine test_monochrome_comparison

end program profile_antialiasing