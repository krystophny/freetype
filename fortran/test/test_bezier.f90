program test_bezier
  use ft_types
  use ft_geometry
  use ft_bezier
  use ft_outline_mod
  use ft_outline_flatten
  use ft_bitmap_mod
  use ft_scanline_simple
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Run all tests
  call test_conic_splitting()
  call test_cubic_splitting()
  call test_conic_flattening()
  call test_curve_rendering()
  
  ! Print test summary
  print '(/, "FT_Bezier Tests - Tests run: ", I0)', test_count
  print '("FT_Bezier Tests - Tests failed: ", I0)', failed_count
  
  if (failed_count == 0) then
    print '(/, "All FT_Bezier tests passed!")'
    stop 0
  else
    print '(/, "Some FT_Bezier tests failed!")'
    stop 1
  end if

contains

  subroutine test_conic_splitting()
    type(FT_Vector) :: p0, p1, p2
    type(FT_Vector) :: q0, q1, r
    
    print '(/, "Testing conic Bezier splitting...")'
    
    ! Set up a simple conic curve
    p0 = FT_Vector(0, 0)
    p1 = FT_Vector(512, 512)  ! Control point
    p2 = FT_Vector(1024, 0)
    
    ! Split at midpoint
    call ft_bezier_split_conic(p0, p1, p2, q0, q1, r)
    
    test_count = test_count + 1
    if (r%x == 512 .and. r%y == 256) then
      print '("PASS: Conic split at correct midpoint")'
    else
      print '("FAIL: Incorrect conic midpoint: (", I0, ",", I0, ")")', r%x, r%y
      failed_count = failed_count + 1
    end if
    
  end subroutine test_conic_splitting
  
  subroutine test_cubic_splitting()
    type(FT_Vector) :: p0, p1, p2, p3
    type(FT_Vector) :: q0, q1, q2, r0, r1, s
    
    print '(/, "Testing cubic Bezier splitting...")'
    
    ! Set up a simple cubic curve
    p0 = FT_Vector(0, 0)
    p1 = FT_Vector(256, 512)
    p2 = FT_Vector(768, 512)
    p3 = FT_Vector(1024, 0)
    
    ! Split at midpoint
    call ft_bezier_split_cubic(p0, p1, p2, p3, q0, q1, q2, r0, r1, s)
    
    test_count = test_count + 1
    if (s%x == 512 .and. s%y > 200 .and. s%y < 400) then
      print '("PASS: Cubic split at reasonable midpoint")'
    else
      print '("FAIL: Unexpected cubic midpoint: (", I0, ",", I0, ")")', s%x, s%y
      failed_count = failed_count + 1
    end if
    
  end subroutine test_cubic_splitting
  
  subroutine test_conic_flattening()
    type(FT_Vector) :: p0, p1, p2
    integer, target :: line_count
    
    print '(/, "Testing conic Bezier flattening...")'
    
    ! Set up a conic curve
    p0 = FT_Vector(0, 0)
    p1 = FT_Vector(512, 1024)  ! High control point for more curvature
    p2 = FT_Vector(1024, 0)
    
    line_count = 0
    call ft_bezier_flatten_conic(p0, p1, p2, count_lines, c_loc(line_count))
    
    test_count = test_count + 1
    if (line_count > 1) then
      print '("PASS: Conic flattened to ", I0, " line segments")', line_count
    else
      print '("FAIL: Conic should produce multiple line segments")'
      failed_count = failed_count + 1
    end if
    
  end subroutine test_conic_flattening
  
  ! Callback to count line segments
  subroutine count_lines(from, to, user_data)
    type(FT_Vector), intent(in) :: from, to
    type(c_ptr), intent(in) :: user_data
    
    integer, pointer :: count
    
    call c_f_pointer(user_data, count)
    count = count + 1
    
  end subroutine count_lines
  
  subroutine test_curve_rendering()
    type(FT_Outline), target :: outline
    type(FT_Bitmap), target :: bitmap
    integer(FT_Error) :: error
    logical :: success
    integer :: x, y
    character(len=20) :: row_str
    
    print '(/, "Testing curved outline rendering...")'
    
    ! Create outline with conic curve (letter 'O' shape)
    success = ft_outline_new(8, 2, outline, error)
    if (.not. success) return
    
    ! Outer contour (clockwise)
    outline%points(1) = FT_Vector(512, 256)    ! Top
    outline%points(2) = FT_Vector(768, 512)    ! Top-right control
    outline%points(3) = FT_Vector(768, 768)    ! Right
    outline%points(4) = FT_Vector(512, 1024)   ! Bottom
    
    ! Inner contour (counter-clockwise for hole)
    outline%points(5) = FT_Vector(512, 512)    ! Top
    outline%points(6) = FT_Vector(640, 640)    ! Control
    outline%points(7) = FT_Vector(512, 768)    ! Bottom
    outline%points(8) = FT_Vector(384, 640)    ! Control
    
    ! Set tags
    outline%tags(1) = FT_CURVE_TAG_ON
    outline%tags(2) = FT_CURVE_TAG_CONIC
    outline%tags(3) = FT_CURVE_TAG_ON
    outline%tags(4) = FT_CURVE_TAG_ON
    outline%tags(5) = FT_CURVE_TAG_ON
    outline%tags(6) = FT_CURVE_TAG_CONIC
    outline%tags(7) = FT_CURVE_TAG_ON
    outline%tags(8) = FT_CURVE_TAG_CONIC
    
    outline%contours(1) = 3  ! First contour ends at point 3
    outline%contours(2) = 7  ! Second contour ends at point 7
    
    ! Create bitmap
    success = ft_bitmap_new(6, 6, FT_PIXEL_MODE_MONO, bitmap, error)
    if (.not. success) then
      call ft_outline_done(outline)
      return
    end if
    
    ! Render with curve flattening
    success = render_outline_with_curves(outline, bitmap, error)
    
    test_count = test_count + 1
    if (success) then
      print '("PASS: Curved outline rendered")'
      print '("Bitmap output (6x6):")'
      
      ! Display bitmap
      do y = 0, bitmap%rows - 1
        row_str = ""
        do x = 0, bitmap%width - 1
          if (ft_bitmap_get_pixel(bitmap, x, y)) then
            row_str(x+1:x+1) = "*"
          else
            row_str(x+1:x+1) = "."
          end if
        end do
        print '(A)', trim(row_str)
      end do
    else
      print '("FAIL: Could not render curved outline")'
      failed_count = failed_count + 1
    end if
    
    ! Cleanup
    call ft_outline_done(outline)
    call ft_bitmap_done(bitmap)
    
  end subroutine test_curve_rendering
  
  ! Render outline with curve flattening
  function render_outline_with_curves(outline, bitmap, error) result(success)
    type(FT_Outline), intent(in) :: outline
    type(FT_Bitmap), intent(inout) :: bitmap
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    type(FT_Outline), target :: flat_outline
    integer :: max_points
    
    ! Estimate max points after flattening (rough estimate)
    max_points = outline%n_points * 10
    
    ! Create flattened outline
    success = ft_outline_new(max_points, int(outline%n_contours), flat_outline, error)
    if (.not. success) return
    
    ! Flatten curves to lines
    flat_outline%n_points = 0
    success = ft_outline_flatten_to_lines(outline, add_line_to_outline, &
                                         c_loc(flat_outline), error)
    
    if (success) then
      ! Copy contour endpoints
      flat_outline%contours = outline%contours
      
      ! Render flattened outline
      success = ft_render_outline_filled(flat_outline, bitmap, error)
    end if
    
    ! Cleanup
    call ft_outline_done(flat_outline)
    
  end function render_outline_with_curves
  
  ! Callback to add lines to outline
  subroutine add_line_to_outline(from, to, user_data)
    type(FT_Vector), intent(in) :: from, to
    type(c_ptr), intent(in) :: user_data
    
    type(FT_Outline), pointer :: outline
    integer :: n
    
    call c_f_pointer(user_data, outline)
    
    ! Add points to outline (simplified - doesn't handle contours properly)
    n = outline%n_points + 1
    if (n <= size(outline%points)) then
      outline%points(n) = from
      outline%tags(n) = FT_CURVE_TAG_ON
      outline%n_points = int(n, kind=kind(outline%n_points))
    end if
    
  end subroutine add_line_to_outline

end program test_bezier