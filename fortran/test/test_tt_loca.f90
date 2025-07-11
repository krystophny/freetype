program test_tt_loca
  use ft_types
  use ft_stream
  use tt_types
  use tt_loca
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Run all tests
  call test_loca_short_format()
  call test_loca_long_format()
  call test_glyph_offset_access()
  call test_glyph_size_calculation()
  
  ! Print test summary
  print '(/, "Loca Table Tests - Tests run: ", I0)', test_count
  print '("Loca Table Tests - Tests failed: ", I0)', failed_count
  
  if (failed_count == 0) then
    print '(/, "All loca table tests passed!")'
    stop 0
  else
    print '(/, "Some loca table tests failed!")'
    stop 1
  end if

contains

  subroutine test_loca_short_format()
    type(FT_Stream_Type) :: stream
    type(TT_Loca_Table) :: loca
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing loca short format loading...")'
    
    ! Create test font with short format loca
    call create_test_font_with_loca_short()
    
    ! Open stream
    success = ft_stream_open(stream, "test_loca_short.ttf", error)
    if (.not. success) then
      print '("FAIL: Could not open test file")'
      failed_count = failed_count + 1
      return
    end if
    
    ! Load loca table (4 glyphs, short format)
    test_count = test_count + 1
    success = tt_load_loca_table(stream, 28_c_size_t, 10_c_size_t, 4, .false., loca, error)
    
    if (success .and. error == FT_Err_Ok) then
      print '("PASS: Load short format loca")'
      
      ! Check format
      test_count = test_count + 1
      if (.not. loca%is_long_format) then
        print '("PASS: Format correctly identified as short")'
      else
        print '("FAIL: Format should be short")'
        failed_count = failed_count + 1
      end if
      
      ! Check number of offsets
      test_count = test_count + 1
      if (allocated(loca%offsets) .and. size(loca%offsets) == 5) then
        print '("PASS: Correct number of offsets (glyphs+1)")'
      else
        print '("FAIL: Incorrect offset array size")'
        failed_count = failed_count + 1
      end if
      
      ! Check offset values (remember short offsets are doubled)
      test_count = test_count + 1
      if (loca%offsets(0) == 0 .and. &
          loca%offsets(1) == 20 .and. &
          loca%offsets(2) == 40 .and. &
          loca%offsets(3) == 60 .and. &
          loca%offsets(4) == 100) then
        print '("PASS: Offset values correct")'
      else
        print '("FAIL: Incorrect offset values")'
        failed_count = failed_count + 1
      end if
      
      call tt_loca_free(loca)
    else
      print '("FAIL: Could not load loca table, error: ", I0)', error
      failed_count = failed_count + 1
    end if
    
    call ft_stream_close(stream)
    call delete_test_files()
    
  end subroutine test_loca_short_format
  
  subroutine test_loca_long_format()
    type(FT_Stream_Type) :: stream
    type(TT_Loca_Table) :: loca
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing loca long format loading...")'
    
    ! Create test font with long format loca
    call create_test_font_with_loca_long()
    
    ! Open stream
    success = ft_stream_open(stream, "test_loca_long.ttf", error)
    if (.not. success) then
      print '("FAIL: Could not open test file")'
      failed_count = failed_count + 1
      return
    end if
    
    ! Load loca table (3 glyphs, long format)
    test_count = test_count + 1
    success = tt_load_loca_table(stream, 28_c_size_t, 16_c_size_t, 3, .true., loca, error)
    
    if (success .and. error == FT_Err_Ok) then
      print '("PASS: Load long format loca")'
      
      ! Check format
      test_count = test_count + 1
      if (loca%is_long_format) then
        print '("PASS: Format correctly identified as long")'
      else
        print '("FAIL: Format should be long")'
        failed_count = failed_count + 1
      end if
      
      ! Check offset values
      test_count = test_count + 1
      if (loca%offsets(0) == 0 .and. &
          loca%offsets(1) == 100 .and. &
          loca%offsets(2) == 200 .and. &
          loca%offsets(3) == 300) then
        print '("PASS: Long offset values correct")'
      else
        print '("FAIL: Incorrect long offset values")'
        failed_count = failed_count + 1
      end if
      
      call tt_loca_free(loca)
    else
      print '("FAIL: Could not load long format loca")'
      failed_count = failed_count + 1
    end if
    
    call ft_stream_close(stream)
    call delete_test_files()
    
  end subroutine test_loca_long_format
  
  subroutine test_glyph_offset_access()
    type(TT_Loca_Table) :: loca
    integer(FT_ULong) :: offset
    
    print '(/, "Testing glyph offset access...")'
    
    ! Set up test data
    loca%is_long_format = .true.
    loca%num_glyphs = 3
    allocate(loca%offsets(0:3))
    loca%offsets = [0, 100, 200, 300]
    
    ! Test valid glyph index
    test_count = test_count + 1
    offset = tt_loca_get_offset(loca, 1)
    
    if (offset == 100) then
      print '("PASS: Get offset for glyph 1")'
    else
      print '("FAIL: Incorrect offset for glyph 1: ", I0)', offset
      failed_count = failed_count + 1
    end if
    
    ! Test boundary glyph
    test_count = test_count + 1
    offset = tt_loca_get_offset(loca, 2)
    
    if (offset == 200) then
      print '("PASS: Get offset for last glyph")'
    else
      print '("FAIL: Incorrect offset for last glyph")'
      failed_count = failed_count + 1
    end if
    
    ! Test invalid glyph index
    test_count = test_count + 1
    offset = tt_loca_get_offset(loca, 5)
    
    if (offset == 0) then
      print '("PASS: Invalid glyph returns 0")'
    else
      print '("FAIL: Invalid glyph should return 0")'
      failed_count = failed_count + 1
    end if
    
    call tt_loca_free(loca)
    
  end subroutine test_glyph_offset_access
  
  subroutine test_glyph_size_calculation()
    type(TT_Loca_Table) :: loca
    integer(FT_ULong) :: size
    
    print '(/, "Testing glyph size calculation...")'
    
    ! Set up test data
    loca%is_long_format = .false.
    loca%num_glyphs = 4
    allocate(loca%offsets(0:4))
    loca%offsets = [0, 20, 40, 60, 100]
    
    ! Test glyph size
    test_count = test_count + 1
    size = tt_loca_get_size(loca, 0)
    
    if (size == 20) then
      print '("PASS: Size of glyph 0")'
    else
      print '("FAIL: Incorrect size for glyph 0: ", I0)', size
      failed_count = failed_count + 1
    end if
    
    ! Test larger glyph
    test_count = test_count + 1
    size = tt_loca_get_size(loca, 3)
    
    if (size == 40) then
      print '("PASS: Size of glyph 3")'
    else
      print '("FAIL: Incorrect size for glyph 3: ", I0)', size
      failed_count = failed_count + 1
    end if
    
    ! Test empty glyph (same offsets)
    loca%offsets(2) = loca%offsets(1)  ! Make glyph 1 empty
    
    test_count = test_count + 1
    size = tt_loca_get_size(loca, 1)
    
    if (size == 0) then
      print '("PASS: Empty glyph has size 0")'
    else
      print '("FAIL: Empty glyph should have size 0")'
      failed_count = failed_count + 1
    end if
    
    call tt_loca_free(loca)
    
  end subroutine test_glyph_size_calculation
  
  ! Helper: Create test font with short format loca
  subroutine create_test_font_with_loca_short()
    integer :: unit
    
    open(newunit=unit, file="test_loca_short.ttf", access="stream", &
         form="unformatted", status="replace")
    
    ! Write header
    call write_int32_be(unit, int(z'00010000', int32))
    call write_int16_be(unit, 1_int16)
    call write_int16_be(unit, 16_int16)
    call write_int16_be(unit, 0_int16)
    call write_int16_be(unit, 0_int16)
    
    ! Write table directory entry
    call write_int32_be(unit, TTAG_loca)
    call write_int32_be(unit, int(z'12345678', int32))
    call write_int32_be(unit, 28_int32)  ! offset
    call write_int32_be(unit, 10_int32)  ! length (5 * 2 bytes)
    
    ! Write loca table (short format - 5 offsets for 4 glyphs)
    ! Values are word offsets, not byte offsets
    call write_int16_be(unit, 0_int16)   ! glyph 0 at byte 0
    call write_int16_be(unit, 10_int16)  ! glyph 1 at byte 20
    call write_int16_be(unit, 20_int16)  ! glyph 2 at byte 40
    call write_int16_be(unit, 30_int16)  ! glyph 3 at byte 60
    call write_int16_be(unit, 50_int16)  ! end at byte 100
    
    close(unit)
  end subroutine
  
  ! Helper: Create test font with long format loca
  subroutine create_test_font_with_loca_long()
    integer :: unit
    
    open(newunit=unit, file="test_loca_long.ttf", access="stream", &
         form="unformatted", status="replace")
    
    ! Write header
    call write_int32_be(unit, int(z'00010000', int32))
    call write_int16_be(unit, 1_int16)
    call write_int16_be(unit, 16_int16)
    call write_int16_be(unit, 0_int16)
    call write_int16_be(unit, 0_int16)
    
    ! Write table directory entry
    call write_int32_be(unit, TTAG_loca)
    call write_int32_be(unit, int(z'12345678', int32))
    call write_int32_be(unit, 28_int32)
    call write_int32_be(unit, 16_int32)  ! length (4 * 4 bytes)
    
    ! Write loca table (long format - 4 offsets for 3 glyphs)
    call write_int32_be(unit, 0_int32)
    call write_int32_be(unit, 100_int32)
    call write_int32_be(unit, 200_int32)
    call write_int32_be(unit, 300_int32)
    
    close(unit)
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
  
  ! Helper: Write 32-bit big-endian
  subroutine write_int32_be(unit, value)
    integer, intent(in) :: unit
    integer(int32), intent(in) :: value
    integer(int8) :: bytes(4)
    
    bytes(1) = int(ishft(value, -24), int8)
    bytes(2) = int(iand(ishft(value, -16), int(255, int32)), int8)
    bytes(3) = int(iand(ishft(value, -8), int(255, int32)), int8)
    bytes(4) = int(iand(value, int(255, int32)), int8)
    
    write(unit) bytes(1), bytes(2), bytes(3), bytes(4)
  end subroutine
  
  ! Helper: Delete test files
  subroutine delete_test_files()
    integer :: unit, stat
    
    open(newunit=unit, file="test_loca_short.ttf", status="old", iostat=stat)
    if (stat == 0) close(unit, status="delete")
    
    open(newunit=unit, file="test_loca_long.ttf", status="old", iostat=stat)
    if (stat == 0) close(unit, status="delete")
  end subroutine

end program test_tt_loca