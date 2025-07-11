program test_tt_maxp
  use ft_types
  use ft_stream
  use tt_types
  use tt_maxp
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Run all tests
  call test_maxp_v10_loading()
  call test_maxp_v05_loading()
  call test_maxp_validation()
  
  ! Print test summary
  print '(/, "Maxp Table Tests - Tests run: ", I0)', test_count
  print '("Maxp Table Tests - Tests failed: ", I0)', failed_count
  
  if (failed_count == 0) then
    print '(/, "All maxp table tests passed!")'
    stop 0
  else
    print '(/, "Some maxp table tests failed!")'
    stop 1
  end if

contains

  subroutine test_maxp_v10_loading()
    type(FT_Stream_Type) :: stream
    type(TT_MaxProfile) :: maxp
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing maxp v1.0 table loading...")'
    
    ! Create test font with maxp v1.0 table
    call create_test_font_with_maxp_v10()
    
    ! Open stream
    success = ft_stream_open(stream, "test_maxp_v10.ttf", error)
    if (.not. success) then
      print '("FAIL: Could not open test file")'
      failed_count = failed_count + 1
      return
    end if
    
    ! Test loading maxp table
    test_count = test_count + 1
    success = tt_load_maxp_table(stream, 28_c_size_t, maxp, error)
    
    if (success .and. error == FT_Err_Ok) then
      print '("PASS: Load maxp v1.0 table")'
      
      ! Check version
      test_count = test_count + 1
      if (maxp%version == int(z'00010000', int32)) then
        print '("PASS: Maxp version correct")'
      else
        print '("FAIL: Maxp version incorrect: ", Z8.8)', maxp%version
        failed_count = failed_count + 1
      end if
      
      ! Check number of glyphs
      test_count = test_count + 1
      if (maxp%num_glyphs == 256) then
        print '("PASS: Number of glyphs correct")'
      else
        print '("FAIL: Number of glyphs incorrect: ", I0)', maxp%num_glyphs
        failed_count = failed_count + 1
      end if
      
      ! Check some v1.0 fields
      test_count = test_count + 1
      if (maxp%max_points == 100 .and. maxp%max_contours == 10) then
        print '("PASS: Max points/contours correct")'
      else
        print '("FAIL: Max points/contours incorrect")'
        failed_count = failed_count + 1
      end if
    else
      print '("FAIL: Could not load maxp table, error: ", I0)', error
      failed_count = failed_count + 1
    end if
    
    call ft_stream_close(stream)
    call delete_test_files()
    
  end subroutine test_maxp_v10_loading
  
  subroutine test_maxp_v05_loading()
    type(FT_Stream_Type) :: stream
    type(TT_MaxProfile) :: maxp
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing maxp v0.5 table loading...")'
    
    ! Create test font with maxp v0.5 table
    call create_test_font_with_maxp_v05()
    
    ! Open stream
    success = ft_stream_open(stream, "test_maxp_v05.ttf", error)
    if (.not. success) then
      print '("FAIL: Could not open test file")'
      failed_count = failed_count + 1
      return
    end if
    
    ! Test loading maxp table
    test_count = test_count + 1
    success = tt_load_maxp_table(stream, 28_c_size_t, maxp, error)
    
    if (success .and. error == FT_Err_Ok) then
      print '("PASS: Load maxp v0.5 table")'
      
      ! Check version
      test_count = test_count + 1
      if (maxp%version == int(z'00005000', int32)) then
        print '("PASS: Maxp v0.5 version correct")'
      else
        print '("FAIL: Maxp version incorrect: ", Z8.8)', maxp%version
        failed_count = failed_count + 1
      end if
      
      ! Check number of glyphs
      test_count = test_count + 1
      if (maxp%num_glyphs == 128) then
        print '("PASS: Number of glyphs correct for v0.5")'
      else
        print '("FAIL: Number of glyphs incorrect: ", I0)', maxp%num_glyphs
        failed_count = failed_count + 1
      end if
    else
      print '("FAIL: Could not load maxp v0.5 table, error: ", I0)', error
      failed_count = failed_count + 1
    end if
    
    call ft_stream_close(stream)
    call delete_test_files()
    
  end subroutine test_maxp_v05_loading
  
  subroutine test_maxp_validation()
    type(FT_Stream_Type) :: stream
    type(TT_MaxProfile) :: maxp
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing maxp table validation...")'
    
    ! Test zero glyphs
    call create_test_font_with_invalid_maxp(1)  ! Zero glyphs
    
    success = ft_stream_open(stream, "test_invalid_maxp.ttf", error)
    if (success) then
      test_count = test_count + 1
      success = tt_load_maxp_table(stream, 28_c_size_t, maxp, error)
      
      if (.not. success .and. error == FT_Err_Invalid_Table) then
        print '("PASS: Zero glyphs detection")'
      else
        print '("FAIL: Should reject zero glyphs")'
        failed_count = failed_count + 1
      end if
      
      call ft_stream_close(stream)
    end if
    
    ! Test invalid version
    call create_test_font_with_invalid_maxp(2)  ! Bad version
    
    success = ft_stream_open(stream, "test_invalid_maxp.ttf", error)
    if (success) then
      test_count = test_count + 1
      success = tt_load_maxp_table(stream, 28_c_size_t, maxp, error)
      
      if (.not. success .and. error == FT_Err_Invalid_Table_Format) then
        print '("PASS: Invalid version detection")'
      else
        print '("FAIL: Should reject invalid version")'
        failed_count = failed_count + 1
      end if
      
      call ft_stream_close(stream)
    end if
    
    call delete_test_files()
    
  end subroutine test_maxp_validation
  
  ! Helper: Create test font with maxp v1.0 table
  subroutine create_test_font_with_maxp_v10()
    integer :: unit, i
    
    open(newunit=unit, file="test_maxp_v10.ttf", access="stream", &
         form="unformatted", status="replace")
    
    ! Write header
    call write_int32_be(unit, int(z'00010000', int32))  ! version
    call write_int16_be(unit, 1_int16)  ! num_tables
    call write_int16_be(unit, 16_int16)  ! search_range
    call write_int16_be(unit, 0_int16)  ! entry_selector
    call write_int16_be(unit, 0_int16)  ! range_shift
    
    ! Write table directory entry for maxp
    call write_int32_be(unit, TTAG_maxp)
    call write_int32_be(unit, int(z'12345678', int32))  ! checksum
    call write_int32_be(unit, 28_int32)  ! offset
    call write_int32_be(unit, 32_int32)  ! length (6 + 26 bytes for v1.0)
    
    ! Write maxp v1.0 table
    call write_int32_be(unit, int(z'00010000', int32))  ! version 1.0
    call write_int16_be(unit, 256_int16)  ! numGlyphs
    call write_int16_be(unit, 100_int16)  ! maxPoints
    call write_int16_be(unit, 10_int16)   ! maxContours
    call write_int16_be(unit, 200_int16)  ! maxCompositePoints
    call write_int16_be(unit, 20_int16)   ! maxCompositeContours
    call write_int16_be(unit, 2_int16)    ! maxZones
    call write_int16_be(unit, 0_int16)    ! maxTwilightPoints
    call write_int16_be(unit, 32_int16)   ! maxStorage
    call write_int16_be(unit, 10_int16)   ! maxFunctionDefs
    call write_int16_be(unit, 0_int16)    ! maxInstructionDefs
    call write_int16_be(unit, 64_int16)   ! maxStackElements
    call write_int16_be(unit, 0_int16)    ! maxSizeOfInstructions
    call write_int16_be(unit, 4_int16)    ! maxComponentElements
    call write_int16_be(unit, 1_int16)    ! maxComponentDepth
    
    close(unit)
  end subroutine
  
  ! Helper: Create test font with maxp v0.5 table
  subroutine create_test_font_with_maxp_v05()
    integer :: unit
    
    open(newunit=unit, file="test_maxp_v05.ttf", access="stream", &
         form="unformatted", status="replace")
    
    ! Write header
    call write_int32_be(unit, int(z'00010000', int32))
    call write_int16_be(unit, 1_int16)
    call write_int16_be(unit, 16_int16)
    call write_int16_be(unit, 0_int16)
    call write_int16_be(unit, 0_int16)
    
    ! Write table directory entry
    call write_int32_be(unit, TTAG_maxp)
    call write_int32_be(unit, int(z'12345678', int32))
    call write_int32_be(unit, 28_int32)
    call write_int32_be(unit, 6_int32)  ! length (only 6 bytes for v0.5)
    
    ! Write maxp v0.5 table
    call write_int32_be(unit, int(z'00005000', int32))  ! version 0.5
    call write_int16_be(unit, 128_int16)  ! numGlyphs
    
    close(unit)
  end subroutine
  
  ! Helper: Create test font with invalid maxp table
  subroutine create_test_font_with_invalid_maxp(error_type)
    integer, intent(in) :: error_type
    integer :: unit
    
    open(newunit=unit, file="test_invalid_maxp.ttf", access="stream", &
         form="unformatted", status="replace")
    
    ! Write header
    call write_int32_be(unit, int(z'00010000', int32))
    call write_int16_be(unit, 1_int16)
    call write_int16_be(unit, 16_int16)
    call write_int16_be(unit, 0_int16)
    call write_int16_be(unit, 0_int16)
    
    ! Write table directory entry
    call write_int32_be(unit, TTAG_maxp)
    call write_int32_be(unit, int(z'12345678', int32))
    call write_int32_be(unit, 28_int32)
    call write_int32_be(unit, 6_int32)
    
    ! Write invalid maxp table
    select case (error_type)
    case (1)  ! Zero glyphs
      call write_int32_be(unit, int(z'00010000', int32))  ! version 1.0
      call write_int16_be(unit, 0_int16)  ! numGlyphs = 0
    case (2)  ! Invalid version
      call write_int32_be(unit, int(z'DEADBEEF', int32))  ! bad version
      call write_int16_be(unit, 100_int16)
    end select
    
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
    
    open(newunit=unit, file="test_maxp_v10.ttf", status="old", iostat=stat)
    if (stat == 0) close(unit, status="delete")
    
    open(newunit=unit, file="test_maxp_v05.ttf", status="old", iostat=stat)
    if (stat == 0) close(unit, status="delete")
    
    open(newunit=unit, file="test_invalid_maxp.ttf", status="old", iostat=stat)
    if (stat == 0) close(unit, status="delete")
  end subroutine

end program test_tt_maxp