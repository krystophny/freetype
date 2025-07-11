program test_performance
  use ft_types
  use ft_memory
  use ft_geometry
  use, intrinsic :: iso_fortran_env, only: int64, real64
  use, intrinsic :: iso_c_binding
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Run performance benchmarks
  call benchmark_fixed_point_math()
  call benchmark_memory_allocation()
  call benchmark_vector_operations()
  
  ! Print summary
  print '(/, "Performance benchmarks completed")'
  print '("Tests run: ", I0)', test_count
  print '("Tests failed: ", I0)', failed_count
  
  if (failed_count == 0) then
    stop 0
  else
    stop 1
  end if

contains

  subroutine benchmark_fixed_point_math()
    integer(FT_Fixed) :: a, b, c
    integer :: i, iterations
    real(real64) :: start_time, end_time, elapsed
    real :: rate
    
    print '(/, "Benchmarking fixed-point arithmetic...")'
    
    iterations = 1000000
    a = ft_fixed_from_float(3.14159)
    b = ft_fixed_from_float(2.71828)
    
    ! Benchmark multiplication
    test_count = test_count + 1
    call cpu_time(start_time)
    
    do i = 1, iterations
      c = ft_fixed_mul(a, b)
    end do
    
    call cpu_time(end_time)
    elapsed = end_time - start_time
    
    if (elapsed > 0.0_real64) then
      rate = real(iterations) / real(elapsed) / 1.0e6
      print '("  Fixed-point multiplications: ", F6.2, " million ops/sec")', rate
    else
      print '("  Fixed-point multiplications: too fast to measure")'
    end if
    
    ! Verify result is correct
    if (abs(ft_fixed_to_float(c) - 8.53973) > 0.001) then
      print '("  FAIL: Incorrect multiplication result")'
      failed_count = failed_count + 1
    end if
    
    ! Benchmark division
    test_count = test_count + 1
    call cpu_time(start_time)
    
    do i = 1, iterations
      c = ft_fixed_div(a, b)
    end do
    
    call cpu_time(end_time)
    elapsed = end_time - start_time
    
    if (elapsed > 0.0_real64) then
      rate = real(iterations) / real(elapsed) / 1.0e6
      print '("  Fixed-point divisions: ", F6.2, " million ops/sec")', rate
    else
      print '("  Fixed-point divisions: too fast to measure")'
    end if
    
  end subroutine benchmark_fixed_point_math
  
  subroutine benchmark_memory_allocation()
    type(c_ptr) :: ptrs(1000)
    integer(FT_Error) :: error
    integer(c_size_t) :: size
    integer :: i, j, iterations
    real(real64) :: start_time, end_time, elapsed
    real :: rate
    logical :: success
    
    print '(/, "Benchmarking memory allocation...")'
    
    iterations = 10000
    size = 1024  ! 1KB blocks
    
    test_count = test_count + 1
    call cpu_time(start_time)
    
    do i = 1, iterations
      ! Allocate and free 1000 blocks
      do j = 1, 1000
        success = ft_mem_alloc(size, ptrs(j), error)
      end do
      do j = 1, 1000
        call ft_mem_free(ptrs(j), size)
      end do
    end do
    
    call cpu_time(end_time)
    elapsed = end_time - start_time
    
    if (elapsed > 0.0_real64) then
      rate = real(iterations * 1000) / real(elapsed) / 1.0e6
      print '("  Memory allocations: ", F6.2, " million alloc/free pairs/sec")', rate
      print '("  Total data throughput: ", F6.2, " GB/sec")', &
        real(iterations * 1000 * size) / real(elapsed) / 1.0e9
    else
      print '("  Memory allocations: too fast to measure")'
    end if
    
  end subroutine benchmark_memory_allocation
  
  subroutine benchmark_vector_operations()
    type(FT_Vector) :: v1, v2, result
    type(FT_Matrix) :: matrix
    integer :: i, iterations
    real(real64) :: start_time, end_time, elapsed
    real :: rate
    
    print '(/, "Benchmarking vector operations...")'
    
    iterations = 1000000
    
    ! Initialize test data
    v1%x = ft_fixed_from_int(100)
    v1%y = ft_fixed_from_int(200)
    v2%x = ft_fixed_from_int(50)
    v2%y = ft_fixed_from_int(75)
    
    ! Identity matrix
    matrix%xx = FT_FIXED_ONE
    matrix%xy = 0
    matrix%yx = 0
    matrix%yy = FT_FIXED_ONE
    
    ! Benchmark vector addition
    test_count = test_count + 1
    call cpu_time(start_time)
    
    do i = 1, iterations
      result = ft_vector_add(v1, v2)
    end do
    
    call cpu_time(end_time)
    elapsed = end_time - start_time
    
    if (elapsed > 0.0_real64) then
      rate = real(iterations) / real(elapsed) / 1.0e6
      print '("  Vector additions: ", F6.2, " million ops/sec")', rate
    else
      print '("  Vector additions: too fast to measure")'
    end if
    
    ! Benchmark vector transformation
    test_count = test_count + 1
    call cpu_time(start_time)
    
    do i = 1, iterations
      result = ft_vector_transform(v1, matrix)
    end do
    
    call cpu_time(end_time)
    elapsed = end_time - start_time
    
    if (elapsed > 0.0_real64) then
      rate = real(iterations) / real(elapsed) / 1.0e6
      print '("  Vector transformations: ", F6.2, " million ops/sec")', rate
    else
      print '("  Vector transformations: too fast to measure")'
    end if
    
  end subroutine benchmark_vector_operations

end program test_performance