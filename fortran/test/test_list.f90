program test_list
  use ft_types
  use ft_object
  use, intrinsic :: iso_c_binding
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Iterator state type
  type :: iterator_state
    integer :: count = 0
    integer :: sum = 0
    integer :: max_count = 0
  end type iterator_state
  
  ! Test list functionality
  call test_list_operations()
  call test_list_iteration()
  
  ! Print test summary
  print '(/, "Tests run: ", I0)', test_count
  print '("Tests failed: ", I0)', failed_count
  
  if (failed_count == 0) then
    print '(/, "All tests passed!")'
    stop 0
  else
    print '(/, "Some tests failed!")'
    stop 1
  end if

contains

  subroutine test_list_operations()
    type(FT_List) :: list
    type(FT_ListNode), pointer :: node1, node2, found
    integer(FT_Error) :: error
    integer, target :: data1, data2, data3
    integer :: count
    
    print '(/, "Testing list operations...")'
    
    data1 = 10
    data2 = 20
    data3 = 30
    
    ! Initialize list
    test_count = test_count + 1
    call ft_list_init(list)
    
    if (ft_list_count(list) /= 0) then
      print '("FAIL: List initialization")'
      failed_count = failed_count + 1
    else
      print '("PASS: List initialization")'
    end if
    
    ! Add elements
    test_count = test_count + 1
    node1 => ft_list_add(list, c_loc(data1), error)
    node2 => ft_list_add(list, c_loc(data2), error)
    
    if (ft_list_count(list) /= 2 .or. error /= FT_Err_Ok) then
      print '("FAIL: List add - expected count 2, got ", I0)', ft_list_count(list)
      failed_count = failed_count + 1
    else
      print '("PASS: List add")'
    end if
    
    ! Test find
    test_count = test_count + 1
    found => ft_list_find(list, c_loc(data2))
    
    if (.not. associated(found) .or. .not. c_associated(found%data, c_loc(data2))) then
      print '("FAIL: List find")'
      failed_count = failed_count + 1
    else
      print '("PASS: List find")'
    end if
    
    ! Test remove
    test_count = test_count + 1
    call ft_list_remove(list, node1)
    count = ft_list_count(list)
    
    if (count /= 1) then
      print '("FAIL: List remove - expected count 1, got ", I0)', count
      failed_count = failed_count + 1
    else
      print '("PASS: List remove")'
    end if
    
    ! Test find after remove
    test_count = test_count + 1
    found => ft_list_find(list, c_loc(data1))
    
    if (associated(found)) then
      print '("FAIL: Found removed element")'
      failed_count = failed_count + 1
    else
      print '("PASS: Element not found after removal")'
    end if
    
    ! Clean up
    call ft_list_done(list)
    
    ! Verify cleanup
    test_count = test_count + 1
    if (ft_list_count(list) /= 0) then
      print '("FAIL: List cleanup")'
      failed_count = failed_count + 1
    else
      print '("PASS: List cleanup")'
    end if
    
  end subroutine test_list_operations
  
  subroutine test_list_iteration()
    type(FT_List) :: list
    type(FT_ListNode), pointer :: node
    integer(FT_Error) :: error
    integer, target :: data(5)
    integer :: i
    type(iterator_state), target :: state
    
    print '(/, "Testing list iteration...")'
    
    ! Initialize data
    do i = 1, 5
      data(i) = i * 10
    end do
    
    call ft_list_init(list)
    
    ! Add elements
    do i = 1, 5
      node => ft_list_add(list, c_loc(data(i)), error)
    end do
    
    ! Test iteration
    test_count = test_count + 1
    state%count = 0
    state%sum = 0
    
    call ft_list_iterate(list, count_iterator, c_loc(state))
    
    if (state%count /= 5 .or. state%sum /= 150) then
      print '("FAIL: List iteration - count=", I0, ", sum=", I0)', &
        state%count, state%sum
      failed_count = failed_count + 1
    else
      print '("PASS: List iteration")'
    end if
    
    ! Test early termination
    test_count = test_count + 1
    state%count = 0
    state%max_count = 3
    
    call ft_list_iterate(list, limited_iterator, c_loc(state))
    
    if (state%count /= 3) then
      print '("FAIL: Early termination - count=", I0)', state%count
      failed_count = failed_count + 1
    else
      print '("PASS: List iteration with early termination")'
    end if
    
    call ft_list_done(list)
    
  end subroutine test_list_iteration
  
  ! Iterator callback that counts and sums
  function count_iterator(node, user_data) result(continue)
    type(FT_ListNode), intent(in) :: node
    type(c_ptr), intent(in) :: user_data
    logical :: continue
    type(iterator_state), pointer :: state
    integer, pointer :: value
    
    call c_f_pointer(user_data, state)
    call c_f_pointer(node%data, value)
    
    state%count = state%count + 1
    state%sum = state%sum + value
    
    continue = .true.
  end function count_iterator
  
  ! Iterator with early termination
  function limited_iterator(node, user_data) result(continue)
    type(FT_ListNode), intent(in) :: node
    type(c_ptr), intent(in) :: user_data
    logical :: continue
    type(iterator_state), pointer :: state
    
    call c_f_pointer(user_data, state)
    
    state%count = state%count + 1
    continue = state%count < state%max_count
  end function limited_iterator

end program test_list