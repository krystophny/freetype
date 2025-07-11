program performance_benchmark
  use ft_types
  use ft_bitmap_mod
  use ft_outline_mod
  use ft_raster
  use ft_raster_types
  use ft_geometry, only: FT_Vector
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real64
  use, intrinsic :: iso_c_binding
  implicit none
  
  integer, parameter :: NUM_ITERATIONS = 1000
  integer, parameter :: BITMAP_SIZE = 64
  
  ! Timing variables
  integer(int64) :: start_time, end_time, freq
  real(real64) :: elapsed_time
  
  ! Test data
  type(FT_Outline), target :: outline
  type(FT_Bitmap), target :: bitmap
  type(FT_Raster_State) :: raster
  type(FT_Raster_Params) :: params
  integer(FT_Error) :: error
  logical :: success
  integer :: i
  
  ! Results
  real(real64) :: glyphs_per_second, megapixels_per_second
  
  print '("FreeType Fortran Performance Benchmark")'
  print '("======================================")'
  print '()'
  
  ! Initialize timing
  call system_clock(count_rate=freq)
  
  ! Create test outline (simple rectangle)
  success = ft_outline_new(4, 1, outline, error)
  if (.not. success) then
    print '("ERROR: Could not create outline")'
    stop 1
  end if
  
  ! Rectangle outline (0,0) to (32,32) in 26.6 fixed point
  outline%points(1)%x = 8 * 64   ! 8 pixels
  outline%points(1)%y = 8 * 64
  outline%points(2)%x = 24 * 64  ! 24 pixels  
  outline%points(2)%y = 8 * 64
  outline%points(3)%x = 24 * 64
  outline%points(3)%y = 24 * 64
  outline%points(4)%x = 8 * 64
  outline%points(4)%y = 24 * 64
  
  outline%tags = FT_CURVE_TAG_ON
  outline%contours(1) = 3  ! End of contour (0-based)
  outline%n_points = 4
  outline%n_contours = 1
  
  ! Create bitmap
  success = ft_bitmap_new(BITMAP_SIZE, BITMAP_SIZE, FT_PIXEL_MODE_MONO, bitmap, error)
  if (.not. success) then
    print '("ERROR: Could not create bitmap")'
    call ft_outline_done(outline)
    stop 1
  end if
  
  ! Create rasterizer
  success = ft_raster_new(raster, error)
  if (.not. success) then
    print '("ERROR: Could not create rasterizer")'
    call ft_outline_done(outline)
    call ft_bitmap_done(bitmap)
    stop 1
  end if
  
  ! Set up rendering parameters
  raster%outline => outline
  params%target => bitmap
  params%flags = 0  ! Monochrome rendering
  
  print '("Test parameters:")'
  print '("  Iterations: ", I0)', NUM_ITERATIONS
  print '("  Bitmap size: ", I0, "x", I0)', BITMAP_SIZE, BITMAP_SIZE
  print '("  Total pixels per iteration: ", I0)', BITMAP_SIZE * BITMAP_SIZE
  print '()'
  
  ! Warm up
  do i = 1, 10
    success = ft_raster_render_outline(raster, params, error)
    if (.not. success) then
      print '("ERROR: Warmup render failed")'
      stop 1
    end if
  end do
  
  ! Benchmark monochrome rendering
  print '("Benchmarking monochrome rendering...")'
  call system_clock(start_time)
  
  do i = 1, NUM_ITERATIONS
    success = ft_raster_render_outline(raster, params, error)
    if (.not. success) then
      print '("ERROR: Render failed at iteration ", I0)', i
      stop 1
    end if
  end do
  
  call system_clock(end_time)
  elapsed_time = real(end_time - start_time, real64) / real(freq, real64)
  
  glyphs_per_second = real(NUM_ITERATIONS, real64) / elapsed_time
  megapixels_per_second = real(NUM_ITERATIONS * BITMAP_SIZE * BITMAP_SIZE, real64) / &
                          (elapsed_time * 1000000.0_real64)
  
  print '("Monochrome rendering results:")'
  print '("  Total time: ", F8.3, " seconds")', elapsed_time
  print '("  Glyphs per second: ", F10.1)', glyphs_per_second
  print '("  Megapixels per second: ", F8.3)', megapixels_per_second
  print '()'
  
  ! Test antialiased rendering if supported
  if (bitmap%pixel_mode == FT_PIXEL_MODE_MONO) then
    ! Switch to grayscale for antialiasing test
    call ft_bitmap_done(bitmap)
    success = ft_bitmap_new(BITMAP_SIZE, BITMAP_SIZE, FT_PIXEL_MODE_GRAY, bitmap, error)
    if (.not. success) then
      print '("ERROR: Could not create grayscale bitmap")'
      stop 1
    end if
    params%target => bitmap
  end if
  
  params%flags = FT_RASTER_FLAG_AA  ! Enable antialiasing
  
  ! Warm up antialiased
  do i = 1, 10
    success = ft_raster_render_outline(raster, params, error)
    if (.not. success) then
      print '("ERROR: AA warmup render failed")'
      stop 1
    end if
  end do
  
  print '("Benchmarking antialiased rendering...")'
  call system_clock(start_time)
  
  do i = 1, NUM_ITERATIONS
    success = ft_raster_render_outline(raster, params, error)
    if (.not. success) then
      print '("ERROR: AA render failed at iteration ", I0)', i
      stop 1
    end if
  end do
  
  call system_clock(end_time)
  elapsed_time = real(end_time - start_time, real64) / real(freq, real64)
  
  glyphs_per_second = real(NUM_ITERATIONS, real64) / elapsed_time
  megapixels_per_second = real(NUM_ITERATIONS * BITMAP_SIZE * BITMAP_SIZE, real64) / &
                          (elapsed_time * 1000000.0_real64)
  
  print '("Antialiased rendering results:")'
  print '("  Total time: ", F8.3, " seconds")', elapsed_time
  print '("  Glyphs per second: ", F10.1)', glyphs_per_second
  print '("  Megapixels per second: ", F8.3)', megapixels_per_second
  print '()'
  
  ! Memory usage benchmark
  print '("Memory usage test...")'
  call test_memory_usage()
  
  ! Cleanup
  call ft_raster_done(raster)
  call ft_bitmap_done(bitmap)
  call ft_outline_done(outline)
  
  print '("Benchmark completed successfully!")'

contains

  subroutine test_memory_usage()
    integer, parameter :: NUM_BITMAPS = 100
    type(FT_Bitmap), allocatable :: bitmaps(:)
    integer :: i
    logical :: success
    integer(FT_Error) :: error
    integer(int64) :: start_mem, end_mem
    
    print '("  Creating ", I0, " bitmaps...")', NUM_BITMAPS
    
    allocate(bitmaps(NUM_BITMAPS))
    
    ! Create multiple bitmaps to test memory allocation
    do i = 1, NUM_BITMAPS
      success = ft_bitmap_new(BITMAP_SIZE, BITMAP_SIZE, FT_PIXEL_MODE_GRAY, bitmaps(i), error)
      if (.not. success) then
        print '("ERROR: Could not create bitmap ", I0)', i
        return
      end if
    end do
    
    print '("  ", I0, " bitmaps created successfully")', NUM_BITMAPS
    print '("  Total allocated pixels: ", I0)', NUM_BITMAPS * BITMAP_SIZE * BITMAP_SIZE
    print '("  Estimated memory usage: ", F8.3, " MB")', &
          real(NUM_BITMAPS * BITMAP_SIZE * BITMAP_SIZE, real64) / (1024.0_real64 * 1024.0_real64)
    
    ! Clean up
    do i = 1, NUM_BITMAPS
      call ft_bitmap_done(bitmaps(i))
    end do
    
    deallocate(bitmaps)
    print '("  Memory cleanup completed")'
    
  end subroutine test_memory_usage

end program performance_benchmark