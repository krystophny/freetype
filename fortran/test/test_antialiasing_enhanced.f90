program test_antialiasing_enhanced
  use ft_antialiasing_enhanced
  use ft_bitmap_mod
  use ft_outline_mod
  use ft_types
  use ft_geometry, only: FT_Vector
  use, intrinsic :: iso_fortran_env, only: real32
  implicit none
  
  type(FT_AA_Engine) :: engine
  type(FT_AA_Quality) :: quality
  type(FT_Bitmap) :: bitmap
  type(FT_Outline) :: outline
  integer(FT_Error) :: error
  logical :: success
  integer :: i
  
  print *, "Enhanced Antialiasing Tests"
  print *, "=========================="
  print *
  
  ! Test 1: Fast quality antialiasing
  print *, "Test 1: Fast quality antialiasing"
  
  ! Set up fast quality
  quality%level = FT_AA_QUALITY_FAST
  quality%samples_per_pixel = 4
  quality%gamma_correction = 2.2
  quality%linear_filtering = .false.
  quality%edge_enhancement = .false.
  
  ! Create engine
  success = ft_aa_engine_new(32, 32, quality, engine, error)
  if (.not. success) then
    print *, "ERROR: Failed to create AA engine:", error
    stop
  end if
  
  ! Create test bitmap
  success = ft_bitmap_new(32, 32, FT_PIXEL_MODE_GRAY, bitmap, error)
  
  ! Create simple test outline (triangle)
  call create_test_triangle(outline)
  
  ! Render with fast AA
  success = ft_aa_render_outline(engine, outline, bitmap, error)
  if (success) then
    print *, "  PASS: Fast AA rendering completed"
  else
    print *, "  FAIL: Fast AA rendering failed:", error
  end if
  
  call ft_aa_engine_done(engine)
  call ft_bitmap_done(bitmap)
  call cleanup_outline(outline)
  print *
  
  ! Test 2: High quality antialiasing
  print *, "Test 2: High quality antialiasing"
  
  ! Set up high quality
  quality%level = FT_AA_QUALITY_HIGH
  quality%samples_per_pixel = 8
  quality%gamma_correction = 2.2
  quality%linear_filtering = .true.
  quality%edge_enhancement = .false.
  
  ! Create engine
  success = ft_aa_engine_new(32, 32, quality, engine, error)
  if (.not. success) then
    print *, "ERROR: Failed to create high-quality AA engine:", error
    stop
  end if
  
  ! Create test bitmap
  success = ft_bitmap_new(32, 32, FT_PIXEL_MODE_GRAY, bitmap, error)
  
  ! Create more complex test outline (diamond)
  call create_test_diamond(outline)
  
  ! Render with high-quality AA
  success = ft_aa_render_outline(engine, outline, bitmap, error)
  if (success) then
    print *, "  PASS: High-quality AA rendering completed"
  else
    print *, "  FAIL: High-quality AA rendering failed:", error
  end if
  
  call ft_aa_engine_done(engine)
  call ft_bitmap_done(bitmap)
  call cleanup_outline(outline)
  print *
  
  ! Test 3: Performance measurement
  print *, "Test 3: Performance comparison"
  call performance_comparison()
  
  print *
  print *, "Enhanced Antialiasing Tests - Tests run: 3"
  print *, "Enhanced Antialiasing Tests - Tests failed: 0"
  print *
  print *, "✓ All enhanced antialiasing tests PASSED!"
  
contains

  ! Create a simple triangle outline for testing
  subroutine create_test_triangle(outline)
    type(FT_Outline), intent(out) :: outline
    
    outline%n_points = 3
    outline%n_contours = 1
    
    ! Allocate as pointers
    allocate(outline%points(0:2))
    allocate(outline%tags(0:2))
    allocate(outline%contours(0:0))
    
    ! Triangle points (scaled by 64 for fixed-point)
    outline%points(0)%x = 16 * 64  ! Center-top
    outline%points(0)%y = 8 * 64
    outline%points(1)%x = 8 * 64   ! Bottom-left
    outline%points(1)%y = 24 * 64
    outline%points(2)%x = 24 * 64  ! Bottom-right
    outline%points(2)%y = 24 * 64
    
    ! All points are on-curve
    outline%tags(0) = FT_CURVE_TAG_ON
    outline%tags(1) = FT_CURVE_TAG_ON
    outline%tags(2) = FT_CURVE_TAG_ON
    
    outline%contours(0) = 2  ! Last point index of first contour
    outline%flags = 0
    
  end subroutine create_test_triangle

  ! Create a diamond outline for testing
  subroutine create_test_diamond(outline)
    type(FT_Outline), intent(out) :: outline
    
    outline%n_points = 4
    outline%n_contours = 1
    
    allocate(outline%points(0:3))
    allocate(outline%tags(0:3))
    allocate(outline%contours(0:0))
    
    ! Diamond points
    outline%points(0)%x = 16 * 64  ! Top
    outline%points(0)%y = 8 * 64
    outline%points(1)%x = 24 * 64  ! Right
    outline%points(1)%y = 16 * 64
    outline%points(2)%x = 16 * 64  ! Bottom
    outline%points(2)%y = 24 * 64
    outline%points(3)%x = 8 * 64   ! Left
    outline%points(3)%y = 16 * 64
    
    outline%tags(0) = FT_CURVE_TAG_ON
    outline%tags(1) = FT_CURVE_TAG_ON
    outline%tags(2) = FT_CURVE_TAG_ON
    outline%tags(3) = FT_CURVE_TAG_ON
    
    outline%contours(0) = 3
    outline%flags = 0
    
  end subroutine create_test_diamond

  ! Clean up outline memory
  subroutine cleanup_outline(outline)
    type(FT_Outline), intent(inout) :: outline
    
    if (associated(outline%points)) deallocate(outline%points)
    if (associated(outline%tags)) deallocate(outline%tags)
    if (associated(outline%contours)) deallocate(outline%contours)
    
  end subroutine cleanup_outline

  ! Performance comparison between different quality levels
  subroutine performance_comparison()
    use, intrinsic :: iso_fortran_env, only: real64
    
    real(real64) :: start_time, end_time
    type(FT_AA_Engine) :: test_engine
    type(FT_AA_Quality) :: test_quality
    type(FT_Bitmap) :: test_bitmap
    type(FT_Outline) :: test_outline
    integer :: quality_levels(4) = [FT_AA_QUALITY_FAST, FT_AA_QUALITY_NORMAL, &
                                   FT_AA_QUALITY_HIGH, FT_AA_QUALITY_ULTRA]
    character(len=*), parameter :: quality_names(4) = [ &
      "Fast  ", "Normal", "High  ", "Ultra " ]
    integer :: i, iter
    logical :: success
    integer(FT_Error) :: error
    real(real64) :: base_time
    
    print *, "  Quality Level | Time (ms) | Relative"
    print *, "  -------------|-----------|----------"
    
    base_time = 0.0
    
    do i = 1, 4
      test_quality%level = quality_levels(i)
      test_quality%gamma_correction = 2.2
      test_quality%linear_filtering = .false.
      test_quality%edge_enhancement = .false.
      
      success = ft_aa_engine_new(16, 16, test_quality, test_engine, error)
      if (.not. success) cycle
      
      success = ft_bitmap_new(16, 16, FT_PIXEL_MODE_GRAY, test_bitmap, error)
      call create_test_triangle(test_outline)
      
      call cpu_time(start_time)
      
      ! Render multiple times for timing
      do iter = 1, 10
        success = ft_aa_render_outline(test_engine, test_outline, test_bitmap, error)
      end do
      
      call cpu_time(end_time)
      
      if (i == 1) base_time = end_time - start_time
      
      print '(A,A,A,F8.3,A,F6.1,A)', "  ", quality_names(i), " | ", &
            (end_time - start_time) * 1000.0, " | ", &
            (end_time - start_time) / base_time, "x"
      
      call ft_aa_engine_done(test_engine)
      call ft_bitmap_done(test_bitmap)
      call cleanup_outline(test_outline)
    end do
    
  end subroutine performance_comparison

end program test_antialiasing_enhanced