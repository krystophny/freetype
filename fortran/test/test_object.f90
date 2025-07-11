program test_object
  use ft_types
  use ft_object
  use ft_memory
  use, intrinsic :: iso_c_binding
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Initialize memory tracking
  call ft_mem_init(enable_tracking=.true.)
  
  ! Test object functionality
  call test_basic_object()
  call test_reference_counting()
  call test_object_class()
  
  ! Print test summary
  print '(/, "Tests run: ", I0)', test_count
  print '("Tests failed: ", I0)', failed_count
  
  ! Clean up
  call ft_mem_done()
  
  if (failed_count == 0) then
    print '(/, "All tests passed!")'
    stop 0
  else
    print '(/, "Some tests failed!")'
    stop 1
  end if

contains

  subroutine test_basic_object()
    type(FT_Object_Type) :: obj
    type(FT_Object_Class) :: basic_class
    integer :: class_id
    
    print '(/, "Testing basic object operations...")'
    
    ! Initialize a basic class
    basic_class%class_id = FT_CLASS_OBJECT
    basic_class%object_size = 0
    
    ! Test object initialization
    test_count = test_count + 1
    call ft_object_init(obj, basic_class)
    
    if (.not. associated(obj%clazz) .or. obj%ref_count /= 1) then
      print '("FAIL: Object initialization")'
      failed_count = failed_count + 1
    else
      print '("PASS: Object initialization")'
    end if
    
    ! Test class ID retrieval
    test_count = test_count + 1
    class_id = ft_object_get_class_id(obj)
    
    if (class_id /= FT_CLASS_OBJECT) then
      print '("FAIL: Class ID retrieval")'
      failed_count = failed_count + 1
    else
      print '("PASS: Class ID retrieval")'
    end if
    
    ! Clean up
    call ft_object_done(obj)
  end subroutine test_basic_object
  
  subroutine test_reference_counting()
    type(FT_Object_Type) :: obj
    type(FT_Object_Class) :: basic_class
    logical :: destroyed
    
    print '(/, "Testing reference counting...")'
    
    ! Initialize a basic class
    basic_class%class_id = FT_CLASS_OBJECT
    basic_class%object_size = 0
    
    call ft_object_init(obj, basic_class)
    
    ! Test reference increment
    test_count = test_count + 1
    call ft_object_ref(obj)
    
    if (obj%ref_count /= 2) then
      print '("FAIL: Reference increment - expected 2, got ", I0)', obj%ref_count
      failed_count = failed_count + 1
    else
      print '("PASS: Reference increment")'
    end if
    
    ! Test reference decrement (not destroyed)
    test_count = test_count + 1
    destroyed = ft_object_unref(obj)
    
    if (destroyed .or. obj%ref_count /= 1) then
      print '("FAIL: Reference decrement without destruction")'
      failed_count = failed_count + 1
    else
      print '("PASS: Reference decrement")'
    end if
    
    ! Test final unref (should destroy)
    test_count = test_count + 1
    destroyed = ft_object_unref(obj)
    
    if (.not. destroyed .or. associated(obj%clazz)) then
      print '("FAIL: Object destruction on final unref")'
      failed_count = failed_count + 1
    else
      print '("PASS: Object destruction")'
    end if
  end subroutine test_reference_counting
  
  subroutine test_object_class()
    type(FT_Object_Type) :: obj
    type(FT_Object_Class) :: custom_class
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing object with internal data...")'
    
    ! Create a class with internal data
    custom_class%class_id = FT_CLASS_FACE  ! Use FACE as example
    custom_class%object_size = 256  ! Some internal data
    custom_class%init => null()
    custom_class%done => null()
    
    ! Test object creation with allocation
    test_count = test_count + 1
    success = ft_object_create(custom_class, obj, error)
    
    if (.not. success .or. error /= FT_Err_Ok .or. &
        .not. c_associated(obj%internal)) then
      print '("FAIL: Object creation with internal data")'
      failed_count = failed_count + 1
    else
      print '("PASS: Object creation with internal data")'
    end if
    
    ! Clean up
    call ft_object_done(obj)
    
    ! Verify cleanup
    test_count = test_count + 1
    if (c_associated(obj%internal)) then
      print '("FAIL: Internal data not freed")'
      failed_count = failed_count + 1
    else
      print '("PASS: Internal data cleanup")'
    end if
  end subroutine test_object_class

end program test_object