program test_tt_load
  use ft_types
  use ft_stream
  use tt_types
  use tt_load
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Run all tests
  call test_header_loading()
  call test_table_directory()
  call test_find_table()
  
  ! Print test summary
  print '(/, "TrueType Load Tests - Tests run: ", I0)', test_count
  print '("TrueType Load Tests - Tests failed: ", I0)', failed_count
  
  if (failed_count == 0) then
    print '(/, "All TrueType load tests passed!")'
    stop 0
  else
    print '(/, "Some TrueType load tests failed!")'
    stop 1
  end if

contains

  subroutine test_header_loading()
    type(FT_Stream_Type) :: stream
    type(TT_Header) :: header
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing TrueType header loading...")'
    
    ! Create a minimal valid TrueType file
    call create_minimal_ttf()
    
    ! Open stream
    success = ft_stream_open(stream, "test_minimal.ttf", error)
    if (.not. success) then
      print '("FAIL: Could not open test file")'
      failed_count = failed_count + 1
      return
    end if
    
    ! Test loading header
    test_count = test_count + 1
    success = tt_load_font_header(stream, header, error)
    
    if (success .and. error == FT_Err_Ok) then
      print '("PASS: Load TrueType header")'
      
      ! Check header values
      test_count = test_count + 1
      if (header%table_version == int(z'00010000', int32) .and. &
          header%num_tables == 3) then
        print '("PASS: Header values correct")'
      else
        print '("FAIL: Header values incorrect")'
        failed_count = failed_count + 1
      end if
    else
      print '("FAIL: Could not load header, error: ", I0)', error
      failed_count = failed_count + 1
    end if
    
    ! Test invalid file
    call ft_stream_close(stream)
    call create_invalid_file()
    
    success = ft_stream_open(stream, "test_invalid.ttf", error)
    if (success) then
      test_count = test_count + 1
      success = tt_load_font_header(stream, header, error)
      
      if (.not. success .and. error == FT_Err_Unknown_File_Format) then
        print '("PASS: Invalid file detection")'
      else
        print '("FAIL: Should reject invalid file")'
        failed_count = failed_count + 1
      end if
      
      call ft_stream_close(stream)
    end if
    
    ! Clean up
    call delete_test_files()
    
  end subroutine test_header_loading
  
  subroutine test_table_directory()
    type(FT_Stream_Type) :: stream
    type(TT_Table_Directory) :: directory
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing table directory loading...")'
    
    ! Create test file
    call create_minimal_ttf()
    
    ! Open stream
    success = ft_stream_open(stream, "test_minimal.ttf", error)
    if (.not. success) return
    
    ! Load table directory
    test_count = test_count + 1
    success = tt_load_table_directory(stream, directory, error)
    
    if (success .and. error == FT_Err_Ok) then
      print '("PASS: Load table directory")'
      
      ! Check tables
      test_count = test_count + 1
      if (allocated(directory%tables) .and. &
          size(directory%tables) == 3) then
        print '("PASS: Table count correct")'
        
        ! Check first table tag
        test_count = test_count + 1
        if (directory%tables(1)%tag == TTAG_head) then
          print '("PASS: First table is head table")'
        else
          print '("FAIL: First table tag incorrect: ", Z8.8)', directory%tables(1)%tag
          failed_count = failed_count + 1
        end if
      else
        print '("FAIL: Table allocation failed")'
        failed_count = failed_count + 1
      end if
      
      ! Clean up
      if (allocated(directory%tables)) deallocate(directory%tables)
    else
      print '("FAIL: Could not load table directory")'
      failed_count = failed_count + 1
    end if
    
    call ft_stream_close(stream)
    call delete_test_files()
    
  end subroutine test_table_directory
  
  subroutine test_find_table()
    type(TT_Table_Directory) :: directory
    integer :: index
    
    print '(/, "Testing table finding...")'
    
    ! Create a mock directory
    directory%header%num_tables = 3
    allocate(directory%tables(3))
    
    directory%tables(1)%tag = TTAG_head
    directory%tables(2)%tag = TTAG_cmap
    directory%tables(3)%tag = TTAG_glyf
    
    ! Test finding existing table
    test_count = test_count + 1
    index = tt_find_table(directory, TTAG_cmap)
    
    if (index == 2) then
      print '("PASS: Find existing table")'
    else
      print '("FAIL: Table index incorrect: ", I0)', index
      failed_count = failed_count + 1
    end if
    
    ! Test finding non-existent table
    test_count = test_count + 1
    index = tt_find_table(directory, TTAG_loca)
    
    if (index == 0) then
      print '("PASS: Non-existent table returns 0")'
    else
      print '("FAIL: Should return 0 for missing table")'
      failed_count = failed_count + 1
    end if
    
    deallocate(directory%tables)
    
  end subroutine test_find_table
  
  ! Helper: Create minimal valid TrueType file
  subroutine create_minimal_ttf()
    integer :: unit
    integer(int32) :: val32
    integer(int16) :: val16
    
    open(newunit=unit, file="test_minimal.ttf", access="stream", &
         form="unformatted", status="replace")
    
    ! Write header
    val32 = int(z'00010000', int32)  ! version 1.0
    call write_int32_be(unit, val32)
    
    val16 = 3_int16  ! num_tables
    call write_int16_be(unit, val16)
    
    val16 = 48_int16  ! search_range (3 tables -> 2^1 * 16 = 32, but using 48)
    call write_int16_be(unit, val16)
    
    val16 = 1_int16  ! entry_selector
    call write_int16_be(unit, val16)
    
    val16 = 0_int16  ! range_shift
    call write_int16_be(unit, val16)
    
    ! Write table directory entries
    ! Table 1: head
    call write_int32_be(unit, TTAG_head)
    call write_int32_be(unit, int(z'12345678', int32))  ! checksum
    call write_int32_be(unit, 100_int32)  ! offset
    call write_int32_be(unit, 54_int32)   ! length
    
    ! Table 2: cmap
    call write_int32_be(unit, TTAG_cmap)
    call write_int32_be(unit, int(z'87654321', int32))  ! checksum
    call write_int32_be(unit, 200_int32)  ! offset
    call write_int32_be(unit, 100_int32)  ! length
    
    ! Table 3: glyf
    call write_int32_be(unit, TTAG_glyf)
    call write_int32_be(unit, int(z'ABCDEF01', int32))  ! checksum
    call write_int32_be(unit, 300_int32)  ! offset
    call write_int32_be(unit, 1000_int32) ! length
    
    close(unit)
  end subroutine create_minimal_ttf
  
  ! Helper: Create invalid file
  subroutine create_invalid_file()
    integer :: unit
    integer(int32) :: val32
    
    open(newunit=unit, file="test_invalid.ttf", access="stream", &
         form="unformatted", status="replace")
    
    ! Write invalid magic number
    val32 = int(z'DEADBEEF', int32)
    call write_int32_be(unit, val32)
    
    close(unit)
  end subroutine create_invalid_file
  
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
  
  ! Helper: Delete test files
  subroutine delete_test_files()
    integer :: unit, stat
    
    open(newunit=unit, file="test_minimal.ttf", status="old", iostat=stat)
    if (stat == 0) close(unit, status="delete")
    
    open(newunit=unit, file="test_invalid.ttf", status="old", iostat=stat)
    if (stat == 0) close(unit, status="delete")
  end subroutine delete_test_files

end program test_tt_load