program test_tt_hmtx
  use ft_types
  use ft_stream
  use tt_types
  use tt_hmtx
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Run all tests
  call test_hmtx_loading()
  call test_advance_width_access()
  call test_lsb_access()
  call test_monospaced_font()
  
  ! Print test summary
  print '(/, "HMtx Table Tests - Tests run: ", I0)', test_count
  print '("HMtx Table Tests - Tests failed: ", I0)', failed_count
  
  if (failed_count == 0) then
    print '(/, "All hmtx table tests passed!")'
    stop 0
  else
    print '(/, "Some hmtx table tests failed!")'
    stop 1
  end if

contains

  subroutine test_hmtx_loading()
    type(FT_Stream_Type) :: stream
    type(TT_HMtx_Table) :: hmtx
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing hmtx table loading...")'
    
    ! Create test font with hmtx table
    call create_test_font_with_hmtx()
    
    ! Open stream
    success = ft_stream_open(stream, "test_hmtx.ttf", error)
    if (.not. success) then
      print '("FAIL: Could not open test file")'
      failed_count = failed_count + 1
      return
    end if
    
    ! Load hmtx table (3 metrics for 5 glyphs)
    test_count = test_count + 1
    success = tt_load_hmtx_table(stream, 28_c_size_t, 3, 5, hmtx, error)
    
    if (success .and. error == FT_Err_Ok) then
      print '("PASS: Load hmtx table")'
      
      ! Check metrics array size
      test_count = test_count + 1
      if (allocated(hmtx%metrics) .and. size(hmtx%metrics) == 3) then
        print '("PASS: Correct number of metrics")'
      else
        print '("FAIL: Incorrect metrics array size")'
        failed_count = failed_count + 1
      end if
      
      ! Check additional LSBs
      test_count = test_count + 1
      if (allocated(hmtx%left_side_bearings) .and. size(hmtx%left_side_bearings) == 2) then
        print '("PASS: Correct number of additional LSBs")'
      else
        print '("FAIL: Incorrect LSB array size")'
        failed_count = failed_count + 1
      end if
      
      ! Check first metric values
      test_count = test_count + 1
      if (hmtx%metrics(1)%advance_width == 1000 .and. hmtx%metrics(1)%lsb == 100) then
        print '("PASS: First metric values correct")'
      else
        print '("FAIL: First metric values incorrect")'
        failed_count = failed_count + 1
      end if
      
      call tt_hmtx_free(hmtx)
    else
      print '("FAIL: Could not load hmtx table, error: ", I0)', error
      failed_count = failed_count + 1
    end if
    
    call ft_stream_close(stream)
    call delete_test_files()
    
  end subroutine test_hmtx_loading
  
  subroutine test_advance_width_access()
    type(TT_HMtx_Table) :: hmtx
    integer(FT_UShort) :: advance
    
    print '(/, "Testing advance width access...")'
    
    ! Set up test data
    hmtx%num_metrics = 3
    hmtx%num_glyphs = 5
    allocate(hmtx%metrics(3))
    hmtx%metrics(1) = TT_HMetric(1000_FT_UShort, 100_FT_Short)
    hmtx%metrics(2) = TT_HMetric(1200_FT_UShort, 150_FT_Short)
    hmtx%metrics(3) = TT_HMetric(800_FT_UShort, 80_FT_Short)
    
    ! Test glyph within metrics range
    test_count = test_count + 1
    advance = tt_hmtx_get_advance(hmtx, 1)
    
    if (advance == 1200) then
      print '("PASS: Get advance for glyph 1")'
    else
      print '("FAIL: Incorrect advance for glyph 1: ", I0)', advance
      failed_count = failed_count + 1
    end if
    
    ! Test glyph beyond metrics range (should use last advance)
    test_count = test_count + 1
    advance = tt_hmtx_get_advance(hmtx, 4)
    
    if (advance == 800) then
      print '("PASS: Get advance for glyph 4 (uses last)")'
    else
      print '("FAIL: Incorrect advance for glyph 4: ", I0)', advance
      failed_count = failed_count + 1
    end if
    
    ! Test invalid glyph
    test_count = test_count + 1
    advance = tt_hmtx_get_advance(hmtx, 10)
    
    if (advance == 0) then
      print '("PASS: Invalid glyph returns 0")'
    else
      print '("FAIL: Invalid glyph should return 0")'
      failed_count = failed_count + 1
    end if
    
    call tt_hmtx_free(hmtx)
    
  end subroutine test_advance_width_access
  
  subroutine test_lsb_access()
    type(TT_HMtx_Table) :: hmtx
    integer(FT_Short) :: lsb
    
    print '(/, "Testing left side bearing access...")'
    
    ! Set up test data
    hmtx%num_metrics = 2
    hmtx%num_glyphs = 4
    allocate(hmtx%metrics(2))
    allocate(hmtx%left_side_bearings(2))
    
    hmtx%metrics(1) = TT_HMetric(1000_FT_UShort, 100_FT_Short)
    hmtx%metrics(2) = TT_HMetric(1200_FT_UShort, -50_FT_Short)
    hmtx%left_side_bearings(1) = 75_FT_Short
    hmtx%left_side_bearings(2) = -25_FT_Short
    
    ! Test glyph with metric
    test_count = test_count + 1
    lsb = tt_hmtx_get_lsb(hmtx, 0)
    
    if (lsb == 100) then
      print '("PASS: Get LSB for glyph 0")'
    else
      print '("FAIL: Incorrect LSB for glyph 0: ", I0)', lsb
      failed_count = failed_count + 1
    end if
    
    ! Test negative LSB
    test_count = test_count + 1
    lsb = tt_hmtx_get_lsb(hmtx, 1)
    
    if (lsb == -50) then
      print '("PASS: Get negative LSB for glyph 1")'
    else
      print '("FAIL: Incorrect LSB for glyph 1: ", I0)', lsb
      failed_count = failed_count + 1
    end if
    
    ! Test glyph using additional LSB array
    test_count = test_count + 1
    lsb = tt_hmtx_get_lsb(hmtx, 2)
    
    if (lsb == 75) then
      print '("PASS: Get LSB from additional array")'
    else
      print '("FAIL: Incorrect LSB for glyph 2: ", I0)', lsb
      failed_count = failed_count + 1
    end if
    
    call tt_hmtx_free(hmtx)
    
  end subroutine test_lsb_access
  
  subroutine test_monospaced_font()
    type(FT_Stream_Type) :: stream
    type(TT_HMtx_Table) :: hmtx
    integer(FT_Error) :: error
    logical :: success
    integer :: i
    
    print '(/, "Testing monospaced font metrics...")'
    
    ! Create test monospaced font
    call create_test_monospaced_font()
    
    ! Open and load
    success = ft_stream_open(stream, "test_mono.ttf", error)
    if (.not. success) return
    
    ! Load hmtx (1 metric for 10 glyphs - typical for monospaced)
    test_count = test_count + 1
    success = tt_load_hmtx_table(stream, 28_c_size_t, 1, 10, hmtx, error)
    
    if (success) then
      print '("PASS: Load monospaced hmtx")'
      
      ! All glyphs should have same advance
      test_count = test_count + 1
      success = .true.
      do i = 0, 9
        if (tt_hmtx_get_advance(hmtx, i) /= 600) then
          success = .false.
          exit
        end if
      end do
      
      if (success) then
        print '("PASS: All glyphs have same advance (600)")'
      else
        print '("FAIL: Monospaced advances inconsistent")'
        failed_count = failed_count + 1
      end if
      
      call tt_hmtx_free(hmtx)
    else
      print '("FAIL: Could not load monospaced hmtx")'
      failed_count = failed_count + 1
    end if
    
    call ft_stream_close(stream)
    call delete_test_files()
    
  end subroutine test_monospaced_font
  
  ! Helper: Create test font with hmtx
  subroutine create_test_font_with_hmtx()
    integer :: unit
    
    open(newunit=unit, file="test_hmtx.ttf", access="stream", &
         form="unformatted", status="replace")
    
    ! Write header
    call write_int32_be(unit, int(z'00010000', int32))
    call write_int16_be(unit, 1_int16)
    call write_int16_be(unit, 16_int16)
    call write_int16_be(unit, 0_int16)
    call write_int16_be(unit, 0_int16)
    
    ! Write table directory entry
    call write_int32_be(unit, TTAG_hmtx)
    call write_int32_be(unit, int(z'12345678', int32))
    call write_int32_be(unit, 28_int32)  ! offset
    call write_int32_be(unit, 16_int32)  ! length (3*4 + 2*2)
    
    ! Write hmtx table
    ! 3 hMetrics records
    call write_int16_be(unit, 1000_int16)  ! advance width
    call write_int16_be(unit, 100_int16)   ! lsb
    
    call write_int16_be(unit, 1200_int16)  
    call write_int16_be(unit, 150_int16)
    
    call write_int16_be(unit, 800_int16)
    call write_int16_be(unit, 80_int16)
    
    ! 2 additional LSBs for glyphs 3 and 4
    call write_int16_be(unit, 90_int16)
    call write_int16_be(unit, 70_int16)
    
    close(unit)
  end subroutine
  
  ! Helper: Create monospaced font
  subroutine create_test_monospaced_font()
    integer :: unit
    
    open(newunit=unit, file="test_mono.ttf", access="stream", &
         form="unformatted", status="replace")
    
    ! Write header
    call write_int32_be(unit, int(z'00010000', int32))
    call write_int16_be(unit, 1_int16)
    call write_int16_be(unit, 16_int16)
    call write_int16_be(unit, 0_int16)
    call write_int16_be(unit, 0_int16)
    
    ! Write table directory entry
    call write_int32_be(unit, TTAG_hmtx)
    call write_int32_be(unit, int(z'12345678', int32))
    call write_int32_be(unit, 28_int32)
    call write_int32_be(unit, 22_int32)  ! length (1*4 + 9*2)
    
    ! Write hmtx table for monospaced font
    ! Only 1 hMetric record
    call write_int16_be(unit, 600_int16)   ! advance width (all glyphs)
    call write_int16_be(unit, 50_int16)    ! lsb
    
    ! 9 additional LSBs
    call write_int16_be(unit, 50_int16)
    call write_int16_be(unit, 60_int16)
    call write_int16_be(unit, 40_int16)
    call write_int16_be(unit, 55_int16)
    call write_int16_be(unit, 45_int16)
    call write_int16_be(unit, 50_int16)
    call write_int16_be(unit, 48_int16)
    call write_int16_be(unit, 52_int16)
    call write_int16_be(unit, 50_int16)
    
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
    
    open(newunit=unit, file="test_hmtx.ttf", status="old", iostat=stat)
    if (stat == 0) close(unit, status="delete")
    
    open(newunit=unit, file="test_mono.ttf", status="old", iostat=stat)
    if (stat == 0) close(unit, status="delete")
  end subroutine

end program test_tt_hmtx