program test_memory
  use ft_types
  use ft_memory
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int64
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Initialize memory tracking
  call ft_mem_init(enable_tracking=.true.)
  
  ! Test memory functionality
  call test_basic_alloc_free()
  call test_memory_operations()
  call test_memory_tracking()
  
  ! Print test summary
  print '(/, "Tests run: ", I0)', test_count
  print '("Tests failed: ", I0)', failed_count
  
  ! Clean up memory system
  call ft_mem_done()
  
  if (failed_count == 0) then
    print '(/, "All tests passed!")'
    stop 0
  else
    print '(/, "Some tests failed!")'
    stop 1
  end if

contains

  subroutine test_basic_alloc_free()
    type(c_ptr) :: ptr
    integer(FT_Error) :: error
    logical :: success
    integer(c_size_t) :: size
    
    print '(/, "Testing basic allocation and deallocation...")'
    
    ! Test allocation
    test_count = test_count + 1
    size = 1024
    success = ft_mem_alloc(size, ptr, error)
    
    if (.not. success .or. error /= FT_Err_Ok .or. .not. c_associated(ptr)) then
      print '("FAIL: Memory allocation")'
      failed_count = failed_count + 1
    else
      print '("PASS: Memory allocation")'
    end if
    
    ! Test deallocation
    test_count = test_count + 1
    call ft_mem_free(ptr, size)
    
    if (c_associated(ptr)) then
      print '("FAIL: Memory deallocation - pointer not nullified")'
      failed_count = failed_count + 1
    else
      print '("PASS: Memory deallocation")'
    end if
    
    ! Test zero-size allocation
    test_count = test_count + 1
    size = 0
    success = ft_mem_alloc(size, ptr, error)
    
    if (success .or. c_associated(ptr)) then
      print '("FAIL: Zero-size allocation should fail")'
      failed_count = failed_count + 1
    else
      print '("PASS: Zero-size allocation")'
    end if
    
  end subroutine test_basic_alloc_free
  
  subroutine test_memory_operations()
    type(c_ptr) :: ptr1, ptr2
    integer(FT_Error) :: error
    integer(c_size_t) :: size
    integer(c_int8_t), pointer :: data1(:), data2(:)
    integer :: i
    logical :: match
    
    print '(/, "Testing memory operations...")'
    
    size = 100
    
    ! Allocate two blocks
    if (.not. ft_mem_alloc(size, ptr1, error)) return
    if (.not. ft_mem_alloc(size, ptr2, error)) then
      call ft_mem_free(ptr1, size)
      return
    end if
    
    ! Test mem_set
    test_count = test_count + 1
    call ft_mem_set(ptr1, int(42, c_int8_t), size)
    
    call c_f_pointer(ptr1, data1, [size])
    match = .true.
    do i = 1, int(size)
      if (data1(i) /= 42) then
        match = .false.
        exit
      end if
    end do
    
    if (.not. match) then
      print '("FAIL: Memory set")'
      failed_count = failed_count + 1
    else
      print '("PASS: Memory set")'
    end if
    
    ! Test mem_copy
    test_count = test_count + 1
    call ft_mem_copy(ptr2, ptr1, size)
    
    call c_f_pointer(ptr2, data2, [size])
    match = .true.
    do i = 1, int(size)
      if (data2(i) /= data1(i)) then
        match = .false.
        exit
      end if
    end do
    
    if (.not. match) then
      print '("FAIL: Memory copy")'
      failed_count = failed_count + 1
    else
      print '("PASS: Memory copy")'
    end if
    
    ! Clean up
    call ft_mem_free(ptr1, size)
    call ft_mem_free(ptr2, size)
    
  end subroutine test_memory_operations
  
  subroutine test_memory_tracking()
    type(FT_Memory_Stats) :: stats
    type(c_ptr) :: ptr1, ptr2
    integer(FT_Error) :: error
    integer(c_size_t) :: size1, size2
    
    print '(/, "Testing memory tracking...")'
    
    ! Get initial stats
    stats = ft_mem_get_stats()
    
    ! Allocate some memory
    size1 = 1000
    size2 = 2000
    
    test_count = test_count + 1
    if (ft_mem_alloc(size1, ptr1, error)) then
      stats = ft_mem_get_stats()
      
      if (stats%current_bytes /= int(size1, int64)) then
        print '("FAIL: Memory tracking - current bytes")'
        failed_count = failed_count + 1
      else
        print '("PASS: Memory tracking - allocation")'
      end if
      
      ! Allocate more
      if (ft_mem_alloc(size2, ptr2, error)) then
        stats = ft_mem_get_stats()
        
        test_count = test_count + 1
        if (stats%current_bytes /= int(size1 + size2, int64) .or. &
            stats%allocation_count /= 2) then
          print '("FAIL: Memory tracking - multiple allocations")'
          failed_count = failed_count + 1
        else
          print '("PASS: Memory tracking - multiple allocations")'
        end if
        
        ! Free one
        call ft_mem_free(ptr2, size2)
        stats = ft_mem_get_stats()
        
        test_count = test_count + 1
        if (stats%current_bytes /= int(size1, int64) .or. &
            stats%allocation_count /= 1) then
          print '("FAIL: Memory tracking - after free")'
          failed_count = failed_count + 1
        else
          print '("PASS: Memory tracking - deallocation")'
        end if
      end if
      
      call ft_mem_free(ptr1, size1)
    end if
    
  end subroutine test_memory_tracking

end program test_memory