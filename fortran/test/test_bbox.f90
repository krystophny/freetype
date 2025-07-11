program test_bbox
  use ft_types
  use ft_geometry
  use, intrinsic :: iso_c_binding
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Test bounding box functionality
  call test_bbox_operations()
  
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

  subroutine test_bbox_operations()
    type(FT_BBox) :: a, b, c
    
    print '(/, "Testing bounding box operations...")'
    
    ! Initialize test bounding boxes
    ! Box A: (10,20) to (30,40)
    a%xMin = ft_fixed_from_int(10)
    a%yMin = ft_fixed_from_int(20)
    a%xMax = ft_fixed_from_int(30)
    a%yMax = ft_fixed_from_int(40)
    
    ! Box B: (20,25) to (40,35)
    b%xMin = ft_fixed_from_int(20)
    b%yMin = ft_fixed_from_int(25)
    b%xMax = ft_fixed_from_int(40)
    b%yMax = ft_fixed_from_int(35)
    
    ! Test union
    test_count = test_count + 1
    c = ft_bbox_union(a, b)
    
    if (ft_fixed_to_int(c%xMin) /= 10 .or. &
        ft_fixed_to_int(c%yMin) /= 20 .or. &
        ft_fixed_to_int(c%xMax) /= 40 .or. &
        ft_fixed_to_int(c%yMax) /= 40) then
      print '("FAIL: BBox union")'
      failed_count = failed_count + 1
    else
      print '("PASS: BBox union")'
    end if
    
    ! Test intersection
    test_count = test_count + 1
    c = ft_bbox_intersect(a, b)
    
    if (ft_fixed_to_int(c%xMin) /= 20 .or. &
        ft_fixed_to_int(c%yMin) /= 25 .or. &
        ft_fixed_to_int(c%xMax) /= 30 .or. &
        ft_fixed_to_int(c%yMax) /= 35) then
      print '("FAIL: BBox intersection")'
      failed_count = failed_count + 1
    else
      print '("PASS: BBox intersection")'
    end if
    
    ! Test no intersection case
    test_count = test_count + 1
    
    ! Box B: far away, no overlap
    b%xMin = ft_fixed_from_int(50)
    b%yMin = ft_fixed_from_int(50)
    b%xMax = ft_fixed_from_int(60)
    b%yMax = ft_fixed_from_int(60)
    
    c = ft_bbox_intersect(a, b)
    
    if (c%xMin /= 0 .or. c%yMin /= 0 .or. c%xMax /= 0 .or. c%yMax /= 0) then
      print '("FAIL: BBox no intersection")'
      failed_count = failed_count + 1
    else
      print '("PASS: BBox no intersection")'
    end if
    
  end subroutine test_bbox_operations

end program test_bbox