program test_tt_head
  use ft_types
  use ft_stream
  use tt_types
  use tt_load
  use tt_head
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Run all tests
  call test_head_table_loading()
  call test_head_table_validation()
  call test_head_table_values()
  
  ! Print test summary
  print '(/, "Head Table Tests - Tests run: ", I0)', test_count
  print '("Head Table Tests - Tests failed: ", I0)', failed_count
  
  if (failed_count == 0) then
    print '(/, "All head table tests passed!")'
    stop 0
  else
    print '(/, "Some head table tests failed!")'
    stop 1
  end if

contains

  subroutine test_head_table_loading()
    type(FT_Stream_Type) :: stream
    type(TT_Header_Table) :: head
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing head table loading...")'
    
    ! Create a test font with head table
    call create_test_font_with_head()
    
    ! Open stream
    success = ft_stream_open(stream, "test_head.ttf", error)
    if (.not. success) then
      print '("FAIL: Could not open test file")'
      failed_count = failed_count + 1
      return
    end if
    
    ! Test loading head table (offset after header and table directory)
    test_count = test_count + 1
    success = tt_load_head_table(stream, 28_c_size_t, head, error)
    
    if (success .and. error == FT_Err_Ok) then
      print '("PASS: Load head table")'
    else
      print '("FAIL: Could not load head table, error: ", I0)', error
      failed_count = failed_count + 1
    end if
    
    ! Test invalid offset
    test_count = test_count + 1
    success = tt_load_head_table(stream, 1000_c_size_t, head, error)
    
    if (.not. success) then
      print '("PASS: Invalid offset handling")'
    else
      print '("FAIL: Should fail with invalid offset")'
      failed_count = failed_count + 1
    end if
    
    call ft_stream_close(stream)
    call delete_test_files()
    
  end subroutine test_head_table_loading
  
  subroutine test_head_table_validation()
    type(FT_Stream_Type) :: stream
    type(TT_Header_Table) :: head
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing head table validation...")'
    
    ! Test invalid version
    call create_test_font_with_invalid_head(1)  ! Invalid version
    
    success = ft_stream_open(stream, "test_invalid_head.ttf", error)
    if (success) then
      test_count = test_count + 1
      success = tt_load_head_table(stream, 28_c_size_t, head, error)
      
      if (.not. success .and. error == FT_Err_Invalid_Table_Format) then
        print '("PASS: Invalid version detection")'
      else
        print '("FAIL: Should reject invalid version")'
        failed_count = failed_count + 1
      end if
      
      call ft_stream_close(stream)
    end if
    
    ! Test invalid magic number
    call create_test_font_with_invalid_head(2)  ! Invalid magic
    
    success = ft_stream_open(stream, "test_invalid_head.ttf", error)
    if (success) then
      test_count = test_count + 1
      success = tt_load_head_table(stream, 28_c_size_t, head, error)
      
      if (.not. success .and. error == FT_Err_Invalid_Table_Format) then
        print '("PASS: Invalid magic number detection")'
      else
        print '("FAIL: Should reject invalid magic")'
        failed_count = failed_count + 1
      end if
      
      call ft_stream_close(stream)
    end if
    
    ! Test invalid units per EM
    call create_test_font_with_invalid_head(3)  ! Invalid units
    
    success = ft_stream_open(stream, "test_invalid_head.ttf", error)
    if (success) then
      test_count = test_count + 1
      success = tt_load_head_table(stream, 28_c_size_t, head, error)
      
      if (.not. success .and. error == FT_Err_Invalid_Table) then
        print '("PASS: Invalid units per EM detection")'
      else
        print '("FAIL: Should reject invalid units per EM")'
        failed_count = failed_count + 1
      end if
      
      call ft_stream_close(stream)
    end if
    
    call delete_test_files()
    
  end subroutine test_head_table_validation
  
  subroutine test_head_table_values()
    type(FT_Stream_Type) :: stream
    type(TT_Header_Table) :: head
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing head table values...")'
    
    ! Create test font
    call create_test_font_with_head()
    
    success = ft_stream_open(stream, "test_head.ttf", error)
    if (.not. success) return
    
    success = tt_load_head_table(stream, 28_c_size_t, head, error)
    if (.not. success) then
      call ft_stream_close(stream)
      return
    end if
    
    ! Check units per EM
    test_count = test_count + 1
    if (head%units_per_em == 2048) then
      print '("PASS: Units per EM correct")'
    else
      print '("FAIL: Units per EM incorrect: ", I0)', head%units_per_em
      failed_count = failed_count + 1
    end if
    
    ! Check bounding box
    test_count = test_count + 1
    if (head%x_min == -1000 .and. head%y_min == -500 .and. &
        head%x_max == 2000 .and. head%y_max == 1500) then
      print '("PASS: Bounding box correct")'
    else
      print '("FAIL: Bounding box incorrect")'
      failed_count = failed_count + 1
    end if
    
    ! Check index format
    test_count = test_count + 1
    if (head%index_to_loc_format == 0) then
      print '("PASS: Index to loc format correct")'
    else
      print '("FAIL: Index to loc format incorrect: ", I0)', head%index_to_loc_format
      failed_count = failed_count + 1
    end if
    
    call ft_stream_close(stream)
    call delete_test_files()
    
  end subroutine test_head_table_values
  
  ! Helper: Create test font with valid head table
  subroutine create_test_font_with_head()
    integer :: unit
    integer(int32) :: val32
    integer(int16) :: val16
    integer(int64) :: val64
    
    open(newunit=unit, file="test_head.ttf", access="stream", &
         form="unformatted", status="replace")
    
    ! Write header
    call write_int32_be(unit, int(z'00010000', int32))  ! version
    call write_int16_be(unit, 1_int16)  ! num_tables
    call write_int16_be(unit, 16_int16)  ! search_range
    call write_int16_be(unit, 0_int16)  ! entry_selector
    call write_int16_be(unit, 0_int16)  ! range_shift
    
    ! Write table directory entry for head
    call write_int32_be(unit, TTAG_head)
    call write_int32_be(unit, int(z'12345678', int32))  ! checksum
    call write_int32_be(unit, 28_int32)  ! offset (after header + 1 table entry)
    call write_int32_be(unit, 54_int32)  ! length
    
    ! Write head table
    call write_int32_be(unit, int(z'00010000', int32))  ! version
    call write_int32_be(unit, int(z'00000000', int32))  ! font revision
    call write_int32_be(unit, int(z'00000000', int32))  ! checksum adjust
    call write_int32_be(unit, int(z'5F0F3CF5', int32))  ! magic number
    call write_int16_be(unit, 0_int16)   ! flags
    call write_int16_be(unit, 2048_int16)  ! units per EM
    
    ! Created/modified dates (8 bytes each)
    val64 = 0
    call write_int64_be(unit, val64)  ! created
    call write_int64_be(unit, val64)  ! modified
    
    ! Bounding box
    call write_int16_be(unit, -1000_int16)  ! x_min
    call write_int16_be(unit, -500_int16)   ! y_min
    call write_int16_be(unit, 2000_int16)   ! x_max
    call write_int16_be(unit, 1500_int16)   ! y_max
    
    ! Style and rendering info
    call write_int16_be(unit, 0_int16)   ! mac_style
    call write_int16_be(unit, 8_int16)   ! lowest_rec_ppem
    call write_int16_be(unit, 2_int16)   ! font_direction
    call write_int16_be(unit, 0_int16)   ! index_to_loc_format (short)
    call write_int16_be(unit, 0_int16)   ! glyph_data_format
    
    close(unit)
  end subroutine create_test_font_with_head
  
  ! Helper: Create test font with invalid head table
  subroutine create_test_font_with_invalid_head(error_type)
    integer, intent(in) :: error_type
    integer :: unit
    integer(int32) :: val32
    integer(int16) :: val16
    integer(int64) :: val64
    
    open(newunit=unit, file="test_invalid_head.ttf", access="stream", &
         form="unformatted", status="replace")
    
    ! Write header
    call write_int32_be(unit, int(z'00010000', int32))
    call write_int16_be(unit, 1_int16)
    call write_int16_be(unit, 16_int16)
    call write_int16_be(unit, 0_int16)
    call write_int16_be(unit, 0_int16)
    
    ! Write table directory entry
    call write_int32_be(unit, TTAG_head)
    call write_int32_be(unit, int(z'12345678', int32))
    call write_int32_be(unit, 28_int32)
    call write_int32_be(unit, 54_int32)
    
    ! Write head table with specific error
    select case (error_type)
    case (1)  ! Invalid version
      call write_int32_be(unit, int(z'DEADBEEF', int32))
    case (2)  ! Invalid magic after valid version
      call write_int32_be(unit, int(z'00010000', int32))  ! version OK
      call write_int32_be(unit, int(z'00000000', int32))  ! font revision
      call write_int32_be(unit, int(z'00000000', int32))  ! checksum adjust
      call write_int32_be(unit, int(z'DEADBEEF', int32))  ! bad magic
    case (3)  ! Invalid units per EM
      call write_int32_be(unit, int(z'00010000', int32))  ! version OK
      call write_int32_be(unit, int(z'00000000', int32))  ! font revision
      call write_int32_be(unit, int(z'00000000', int32))  ! checksum adjust
      call write_int32_be(unit, int(z'5F0F3CF5', int32))  ! magic OK
      call write_int16_be(unit, 0_int16)   ! flags
      call write_int16_be(unit, 10_int16)  ! units per EM too small
    end select
    
    close(unit)
  end subroutine create_test_font_with_invalid_head
  
  ! Helper: Write 16-bit big-endian
  subroutine write_int16_be(unit, value)
    integer, intent(in) :: unit
    integer(int16), intent(in) :: value
    integer(int8) :: bytes(2)
    
    bytes(1) = int(ishft(value, -8), int8)
    bytes(2) = int(iand(value, int(255, int16)), int8)
    
    write(unit) bytes(1), bytes(2)
  end subroutine write_int16_be
  
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
  end subroutine write_int32_be
  
  ! Helper: Write 64-bit big-endian
  subroutine write_int64_be(unit, value)
    integer, intent(in) :: unit
    integer(int64), intent(in) :: value
    integer(int8) :: bytes(8)
    integer :: i
    
    do i = 1, 8
      bytes(i) = int(iand(ishft(value, -8*(8-i)), int(255, int64)), int8)
    end do
    
    write(unit) bytes
  end subroutine write_int64_be
  
  ! Helper: Delete test files
  subroutine delete_test_files()
    integer :: unit, stat
    
    open(newunit=unit, file="test_head.ttf", status="old", iostat=stat)
    if (stat == 0) close(unit, status="delete")
    
    open(newunit=unit, file="test_invalid_head.ttf", status="old", iostat=stat)
    if (stat == 0) close(unit, status="delete")
  end subroutine delete_test_files

end program test_tt_head