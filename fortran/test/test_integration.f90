program test_integration
  use ft_types
  use ft_memory
  use ft_geometry
  use ft_object
  use ft_glyph_types
  use, intrinsic :: iso_c_binding
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Initialize memory tracking
  call ft_mem_init(enable_tracking=.true.)
  
  ! Run integration tests
  call test_object_with_vectors()
  call test_list_of_objects()
  call test_memory_intensive_operations()
  
  ! Print test summary
  print '(/, "Integration Tests - Tests run: ", I0)', test_count
  print '("Integration Tests - Tests failed: ", I0)', failed_count
  
  ! Check for memory leaks
  call ft_mem_done()
  
  if (failed_count == 0) then
    print '(/, "All integration tests passed!")'
    stop 0
  else
    print '(/, "Some integration tests failed!")'
    stop 1
  end if

contains

  subroutine test_object_with_vectors()
    type(FT_Object_Type) :: obj
    type(FT_Object_Class) :: vector_container_class
    type(FT_List) :: vector_list
    type(FT_Vector), target :: vectors(3)
    type(FT_ListNode), pointer :: node
    integer(FT_Error) :: error
    integer :: i
    
    print '(/, "Testing objects containing lists of vectors...")'
    
    ! Initialize vectors
    do i = 1, 3
      vectors(i)%x = ft_fixed_from_int(i * 10)
      vectors(i)%y = ft_fixed_from_int(i * 20)
    end do
    
    ! Create a custom object class
    vector_container_class%class_id = FT_CLASS_GLYPH
    vector_container_class%object_size = 256  ! Arbitrary size for container
    
    ! Initialize object
    test_count = test_count + 1
    call ft_object_init(obj, vector_container_class)
    call ft_list_init(vector_list)
    
    ! Add vectors to list
    do i = 1, 3
      node => ft_list_add(vector_list, c_loc(vectors(i)), error)
    end do
    
    if (ft_list_count(vector_list) /= 3) then
      print '("FAIL: Vector list creation")'
      failed_count = failed_count + 1
    else
      print '("PASS: Object with vector list")'
    end if
    
    ! Clean up
    call ft_list_done(vector_list)
    call ft_object_done(obj)
    
  end subroutine test_object_with_vectors
  
  subroutine test_list_of_objects()
    type(FT_List) :: object_list
    type(FT_Object_Type), target :: objects(5)
    type(FT_Object_Class) :: test_class
    type(FT_ListNode), pointer :: node
    integer(FT_Error) :: error
    integer :: i
    logical :: all_found
    
    print '(/, "Testing list of objects with reference counting...")'
    
    ! Initialize test class
    test_class%class_id = FT_CLASS_SIZE
    test_class%object_size = 64
    
    call ft_list_init(object_list)
    
    ! Create and add objects
    test_count = test_count + 1
    do i = 1, 5
      call ft_object_init(objects(i), test_class)
      node => ft_list_add(object_list, c_loc(objects(i)), error)
      ! Increment reference count since it's in the list
      call ft_object_ref(objects(i))
    end do
    
    ! Verify all objects are in list
    all_found = .true.
    do i = 1, 5
      node => ft_list_find(object_list, c_loc(objects(i)))
      if (.not. associated(node)) all_found = .false.
    end do
    
    if (.not. all_found .or. ft_list_count(object_list) /= 5) then
      print '("FAIL: List of objects")'
      failed_count = failed_count + 1
    else
      print '("PASS: List of objects with references")'
    end if
    
    ! Clean up - remove from list and unref
    do i = 1, 5
      node => ft_list_find(object_list, c_loc(objects(i)))
      if (associated(node)) call ft_list_remove(object_list, node)
      ! Unref twice - once for list, once for original
      if (ft_object_unref(objects(i))) continue
      if (ft_object_unref(objects(i))) continue
    end do
    
    call ft_list_done(object_list)
    
  end subroutine test_list_of_objects
  
  subroutine test_memory_intensive_operations()
    type(FT_Memory_Stats) :: stats_before, stats_after
    type(FT_Glyph_Metrics), pointer :: metrics_array(:)
    type(c_ptr) :: metrics_ptr
    integer(FT_Error) :: error
    integer(c_size_t) :: array_size
    integer :: i
    logical :: success
    
    print '(/, "Testing memory-intensive operations...")'
    
    ! Get initial memory stats
    stats_before = ft_mem_get_stats()
    
    ! Allocate large array of glyph metrics
    test_count = test_count + 1
    array_size = 1000 * 64  ! Approximate size of FT_Glyph_Metrics
    success = ft_mem_alloc(array_size, metrics_ptr, error)
    
    if (.not. success) then
      print '("FAIL: Large allocation")'
      failed_count = failed_count + 1
      return
    end if
    
    ! Map to Fortran pointer
    call c_f_pointer(metrics_ptr, metrics_array, [1000])
    
    ! Initialize all metrics
    do i = 1, 1000
      call ft_glyph_metrics_init(metrics_array(i))
      metrics_array(i)%width = ft_f26dot6_from_pixels(i)
      metrics_array(i)%height = ft_f26dot6_from_pixels(i * 2)
    end do
    
    ! Scale all metrics
    do i = 1, 1000
      call ft_glyph_metrics_scale(metrics_array(i), &
                                  ft_fixed_from_float(1.5), &
                                  ft_fixed_from_float(1.5))
    end do
    
    ! Verify a sample
    if (ft_f26dot6_to_int(metrics_array(100)%width) /= 150) then
      print '("FAIL: Scaled metrics incorrect")'
      failed_count = failed_count + 1
    else
      print '("PASS: Memory-intensive glyph operations")'
    end if
    
    ! Free memory
    call ft_mem_free(metrics_ptr, array_size)
    
    ! Verify memory was freed
    test_count = test_count + 1
    stats_after = ft_mem_get_stats()
    
    if (stats_after%current_bytes /= stats_before%current_bytes) then
      print '("FAIL: Memory leak detected")'
      failed_count = failed_count + 1
    else
      print '("PASS: Memory properly freed")'
    end if
    
  end subroutine test_memory_intensive_operations

end program test_integration