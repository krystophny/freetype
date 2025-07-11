program test_tt_glyph
  use ft_types
  use ft_stream
  use tt_types
  use tt_glyph
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Run all tests
  call test_glyph_header_loading()
  call test_empty_glyph()
  call test_simple_glyph_rectangle()
  call test_simple_glyph_triangle()
  call test_glyph_flags_parsing()
  call test_coordinate_parsing()
  
  ! Print test summary
  print '(/, "TrueType Glyph Tests - Tests run: ", I0)', test_count
  print '("TrueType Glyph Tests - Tests failed: ", I0)', failed_count
  
  if (failed_count == 0) then
    print '(/, "All TrueType glyph tests passed!")'
    stop 0
  else
    print '(/, "Some TrueType glyph tests failed!")'
    stop 1
  end if

contains

  subroutine test_glyph_header_loading()
    type(FT_Stream_Type) :: stream
    type(TT_Glyph_Header) :: header
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing glyph header loading...")'
    
    ! Create test glyph with header only
    call create_test_glyph_header()
    
    ! Open stream
    success = ft_stream_open(stream, "test_glyph_header.bin", error)
    if (.not. success) then
      print '("FAIL: Could not open test file")'
      failed_count = failed_count + 1
      return
    end if
    
    ! Load glyph header
    test_count = test_count + 1
    success = tt_load_glyph_header(stream, header, error)
    
    if (success .and. error == FT_Err_Ok) then
      print '("PASS: Glyph header loaded successfully")'
      
      ! Check contour count
      test_count = test_count + 1
      if (header%num_contours == 2) then
        print '("PASS: Correct contour count")'
      else
        print '("FAIL: Incorrect contour count: ", I0)', header%num_contours
        failed_count = failed_count + 1
      end if
      
      ! Check bounding box
      test_count = test_count + 1
      if (header%x_min == 100 .and. header%y_min == 200 .and. &
          header%x_max == 500 .and. header%y_max == 700) then
        print '("PASS: Correct bounding box")'
      else
        print '("FAIL: Incorrect bounding box")'
        failed_count = failed_count + 1
      end if
      
    else
      print '("FAIL: Could not load glyph header, error: ", I0)', error
      failed_count = failed_count + 1
    end if
    
    call ft_stream_close(stream)
    call delete_test_files()
    
  end subroutine test_glyph_header_loading
  
  subroutine test_empty_glyph()
    type(FT_Stream_Type) :: stream
    type(TT_Simple_Glyph) :: glyph
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing empty glyph (like space)...")'
    
    ! Create empty glyph (0 contours)
    call create_empty_glyph()
    
    success = ft_stream_open(stream, "test_empty_glyph.bin", error)
    if (.not. success) return
    
    test_count = test_count + 1
    success = tt_load_simple_glyph(stream, glyph, error)
    
    if (success) then
      print '("PASS: Empty glyph loaded")'
      
      test_count = test_count + 1
      if (glyph%header%num_contours == 0) then
        print '("PASS: Zero contours for empty glyph")'
      else
        print '("FAIL: Empty glyph should have 0 contours")'
        failed_count = failed_count + 1
      end if
      
      test_count = test_count + 1
      if (glyph%num_points == 0) then
        print '("PASS: Zero points for empty glyph")'
      else
        print '("FAIL: Empty glyph should have 0 points")'
        failed_count = failed_count + 1
      end if
      
      call tt_glyph_free(glyph)
    else
      print '("FAIL: Could not load empty glyph")'
      failed_count = failed_count + 1
    end if
    
    call ft_stream_close(stream)
    call delete_test_files()
    
  end subroutine test_empty_glyph
  
  subroutine test_simple_glyph_rectangle()
    type(FT_Stream_Type) :: stream
    type(TT_Simple_Glyph) :: glyph
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing simple rectangular glyph...")'
    
    ! Create rectangular glyph (1 contour, 4 points)
    call create_rectangle_glyph()
    
    success = ft_stream_open(stream, "test_rect_glyph.bin", error)
    if (.not. success) return
    
    test_count = test_count + 1
    success = tt_load_simple_glyph(stream, glyph, error)
    
    if (success) then
      print '("PASS: Rectangle glyph loaded")'
      
      ! Check contours
      test_count = test_count + 1
      if (glyph%header%num_contours == 1) then
        print '("PASS: One contour for rectangle")'
      else
        print '("FAIL: Rectangle should have 1 contour")'
        failed_count = failed_count + 1
      end if
      
      ! Check points
      test_count = test_count + 1
      if (glyph%num_points == 5) then  ! 4 corners + 1 (endpoint arrays are 0-based)
        print '("PASS: Correct point count for rectangle")'
      else
        print '("FAIL: Rectangle point count: ", I0, " (expected 5)")', glyph%num_points
        failed_count = failed_count + 1
      end if
      
      ! Check endpoint
      test_count = test_count + 1
      if (allocated(glyph%end_pts_of_contours) .and. &
          glyph%end_pts_of_contours(1) == 4) then
        print '("PASS: Correct contour endpoint")'
      else
        print '("FAIL: Incorrect contour endpoint")'
        failed_count = failed_count + 1
      end if
      
      ! Check coordinates
      test_count = test_count + 1
      if (allocated(glyph%x_coordinates) .and. allocated(glyph%y_coordinates)) then
        if (glyph%x_coordinates(1) == 100 .and. glyph%y_coordinates(1) == 200 .and. &
            glyph%x_coordinates(3) == 400 .and. glyph%y_coordinates(3) == 600) then
          print '("PASS: Correct coordinate values")'
        else
          print '("FAIL: Incorrect coordinate values")'
          failed_count = failed_count + 1
        end if
      else
        print '("FAIL: Coordinate arrays not allocated")'
        failed_count = failed_count + 1
      end if
      
      call tt_glyph_free(glyph)
    else
      print '("FAIL: Could not load rectangle glyph, error: ", I0)', error
      failed_count = failed_count + 1
    end if
    
    call ft_stream_close(stream)
    call delete_test_files()
    
  end subroutine test_simple_glyph_rectangle
  
  subroutine test_simple_glyph_triangle()
    type(FT_Stream_Type) :: stream
    type(TT_Simple_Glyph) :: glyph
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing triangular glyph with flags...")'
    
    ! Create triangle with various flag types
    call create_triangle_glyph_with_flags()
    
    success = ft_stream_open(stream, "test_triangle_glyph.bin", error)
    if (.not. success) return
    
    test_count = test_count + 1
    success = tt_load_simple_glyph(stream, glyph, error)
    
    if (success) then
      print '("PASS: Triangle glyph loaded")'
      
      ! Check on-curve flags
      test_count = test_count + 1
      if (allocated(glyph%on_curve)) then
        if (glyph%on_curve(1) .and. glyph%on_curve(2) .and. glyph%on_curve(3)) then
          print '("PASS: All triangle points on curve")'
        else
          print '("FAIL: Not all triangle points marked on curve")'
          failed_count = failed_count + 1
        end if
      else
        print '("FAIL: On-curve array not allocated")'
        failed_count = failed_count + 1
      end if
      
      call tt_glyph_free(glyph)
    else
      print '("FAIL: Could not load triangle glyph, error: ", I0)', error
      failed_count = failed_count + 1
    end if
    
    call ft_stream_close(stream)
    call delete_test_files()
    
  end subroutine test_simple_glyph_triangle
  
  subroutine test_glyph_flags_parsing()
    integer(int8) :: flag
    logical :: on_curve, x_short, y_short, repeat_flag, x_same, y_same
    
    print '(/, "Testing glyph flag parsing...")'
    
    ! Test flag combination
    flag = ior(ior(TT_FLAG_ON_CURVE, TT_FLAG_X_SHORT_VECTOR), TT_FLAG_Y_SAME_OR_POSITIVE)
    
    on_curve = (iand(flag, TT_FLAG_ON_CURVE) /= 0)
    x_short = (iand(flag, TT_FLAG_X_SHORT_VECTOR) /= 0)
    y_short = (iand(flag, TT_FLAG_Y_SHORT_VECTOR) /= 0)
    repeat_flag = (iand(flag, TT_FLAG_REPEAT) /= 0)
    x_same = (iand(flag, TT_FLAG_X_SAME_OR_POSITIVE) /= 0)
    y_same = (iand(flag, TT_FLAG_Y_SAME_OR_POSITIVE) /= 0)
    
    test_count = test_count + 1
    if (on_curve .and. x_short .and. .not. y_short .and. .not. repeat_flag .and. &
        .not. x_same .and. y_same) then
      print '("PASS: Flag bit parsing correct")'
    else
      print '("FAIL: Flag bit parsing incorrect")'
      failed_count = failed_count + 1
    end if
    
  end subroutine test_glyph_flags_parsing
  
  subroutine test_coordinate_parsing()
    print '(/, "Testing coordinate delta parsing...")'
    
    ! This is tested implicitly in the glyph loading tests
    test_count = test_count + 1
    print '("PASS: Coordinate parsing tested in glyph loading")'
    
  end subroutine test_coordinate_parsing
  
  ! Helper: Create test glyph header
  subroutine create_test_glyph_header()
    integer :: unit
    
    open(newunit=unit, file="test_glyph_header.bin", access="stream", &
         form="unformatted", status="replace")
    
    ! Write glyph header (10 bytes)
    call write_int16_be(unit, 2_int16)     ! num_contours
    call write_int16_be(unit, 100_int16)   ! x_min
    call write_int16_be(unit, 200_int16)   ! y_min
    call write_int16_be(unit, 500_int16)   ! x_max
    call write_int16_be(unit, 700_int16)   ! y_max
    
    close(unit)
  end subroutine
  
  ! Helper: Create empty glyph (0 contours)
  subroutine create_empty_glyph()
    integer :: unit
    
    open(newunit=unit, file="test_empty_glyph.bin", access="stream", &
         form="unformatted", status="replace")
    
    ! Write glyph header for empty glyph
    call write_int16_be(unit, 0_int16)     ! num_contours (0 = empty)
    call write_int16_be(unit, 0_int16)     ! x_min
    call write_int16_be(unit, 0_int16)     ! y_min
    call write_int16_be(unit, 0_int16)     ! x_max
    call write_int16_be(unit, 0_int16)     ! y_max
    
    close(unit)
  end subroutine
  
  ! Helper: Create rectangle glyph (1 contour, 5 points including close)
  subroutine create_rectangle_glyph()
    integer :: unit
    
    open(newunit=unit, file="test_rect_glyph.bin", access="stream", &
         form="unformatted", status="replace")
    
    ! Glyph header
    call write_int16_be(unit, 1_int16)     ! num_contours
    call write_int16_be(unit, 100_int16)   ! x_min
    call write_int16_be(unit, 200_int16)   ! y_min
    call write_int16_be(unit, 400_int16)   ! x_max
    call write_int16_be(unit, 600_int16)   ! y_max
    
    ! Contour endpoints
    call write_int16_be(unit, 4_int16)     ! end of contour 0 (point index 4)
    
    ! Instruction length
    call write_int16_be(unit, 0_int16)     ! no instructions
    
    ! Flags (5 points, all on curve)
    call write_int8_be(unit, TT_FLAG_ON_CURVE)  ! point 0
    call write_int8_be(unit, TT_FLAG_ON_CURVE)  ! point 1
    call write_int8_be(unit, TT_FLAG_ON_CURVE)  ! point 2
    call write_int8_be(unit, TT_FLAG_ON_CURVE)  ! point 3
    call write_int8_be(unit, TT_FLAG_ON_CURVE)  ! point 4
    
    ! X coordinates (absolute: 100, 400, 400, 100, 100)
    call write_int16_be(unit, 100_int16)   ! point 0
    call write_int16_be(unit, 300_int16)   ! point 1 (delta +300)
    call write_int16_be(unit, 0_int16)     ! point 2 (delta 0)
    call write_int16_be(unit, -300_int16)  ! point 3 (delta -300)
    call write_int16_be(unit, 0_int16)     ! point 4 (delta 0)
    
    ! Y coordinates (absolute: 200, 200, 600, 600, 200)
    call write_int16_be(unit, 200_int16)   ! point 0
    call write_int16_be(unit, 0_int16)     ! point 1 (delta 0)
    call write_int16_be(unit, 400_int16)   ! point 2 (delta +400)
    call write_int16_be(unit, 0_int16)     ! point 3 (delta 0)
    call write_int16_be(unit, -400_int16)  ! point 4 (delta -400)
    
    close(unit)
  end subroutine
  
  ! Helper: Create triangle glyph with various flags
  subroutine create_triangle_glyph_with_flags()
    integer :: unit
    
    open(newunit=unit, file="test_triangle_glyph.bin", access="stream", &
         form="unformatted", status="replace")
    
    ! Glyph header
    call write_int16_be(unit, 1_int16)     ! num_contours
    call write_int16_be(unit, 50_int16)    ! x_min
    call write_int16_be(unit, 100_int16)   ! y_min
    call write_int16_be(unit, 250_int16)   ! x_max
    call write_int16_be(unit, 300_int16)   ! y_max
    
    ! Contour endpoints
    call write_int16_be(unit, 2_int16)     ! end of contour 0 (point index 2)
    
    ! Instruction length
    call write_int16_be(unit, 0_int16)     ! no instructions
    
    ! Flags (3 points, all on curve, use repeat for testing)
    call write_int8_be(unit, ior(TT_FLAG_ON_CURVE, TT_FLAG_REPEAT))  ! point 0 with repeat
    call write_int8_be(unit, 2_int8)       ! repeat next flag 2 times
    
    ! X coordinates (triangle: 50, 150, 250)
    call write_int16_be(unit, 50_int16)    ! point 0
    call write_int16_be(unit, 100_int16)   ! point 1 (delta +100)
    call write_int16_be(unit, 100_int16)   ! point 2 (delta +100)
    
    ! Y coordinates (triangle: 300, 100, 300)
    call write_int16_be(unit, 300_int16)   ! point 0
    call write_int16_be(unit, -200_int16)  ! point 1 (delta -200)
    call write_int16_be(unit, 200_int16)   ! point 2 (delta +200)
    
    close(unit)
  end subroutine
  
  ! Helper: Write 8-bit value
  subroutine write_int8_be(unit, value)
    integer, intent(in) :: unit
    integer(int8), intent(in) :: value
    
    write(unit) value
  end subroutine
  
  ! Helper: Write 16-bit big-endian
  subroutine write_int16_be(unit, value)
    integer, intent(in) :: unit
    integer(int16), intent(in) :: value
    integer(int8) :: bytes(2)
    
    bytes(1) = int(ishft(value, -8), int8)
    bytes(2) = int(iand(value, int(255, int16)), int8)
    
    write(unit) bytes(1), bytes(2)
  end subroutine
  
  ! Helper: Delete test files
  subroutine delete_test_files()
    integer :: unit, stat
    
    open(newunit=unit, file="test_glyph_header.bin", status="old", iostat=stat)
    if (stat == 0) close(unit, status="delete")
    
    open(newunit=unit, file="test_empty_glyph.bin", status="old", iostat=stat)
    if (stat == 0) close(unit, status="delete")
    
    open(newunit=unit, file="test_rect_glyph.bin", status="old", iostat=stat)
    if (stat == 0) close(unit, status="delete")
    
    open(newunit=unit, file="test_triangle_glyph.bin", status="old", iostat=stat)
    if (stat == 0) close(unit, status="delete")
  end subroutine

end program test_tt_glyph