program test_tt_integration
  use ft_types
  use ft_stream
  use tt_types
  use tt_load
  use tt_head
  use tt_maxp
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Run integration tests
  call test_multi_table_loading()
  call test_table_lookup_and_load()
  
  ! Print test summary
  print '(/, "TrueType Integration Tests - Tests run: ", I0)', test_count
  print '("TrueType Integration Tests - Tests failed: ", I0)', failed_count
  
  if (failed_count == 0) then
    print '(/, "All TrueType integration tests passed!")'
    stop 0
  else
    print '(/, "Some TrueType integration tests failed!")'
    stop 1
  end if

contains

  subroutine test_multi_table_loading()
    type(FT_Stream_Type) :: stream
    type(TT_Table_Directory) :: directory
    type(TT_Header_Table) :: head
    type(TT_MaxProfile) :: maxp
    integer(FT_Error) :: error
    logical :: success
    integer :: head_idx, maxp_idx
    
    print '(/, "Testing multi-table loading...")'
    
    ! Create test font with both tables
    call create_test_font_with_tables()
    
    ! Open stream
    success = ft_stream_open(stream, "test_multi.ttf", error)
    if (.not. success) then
      print '("FAIL: Could not open test file")'
      failed_count = failed_count + 1
      return
    end if
    
    ! Load table directory
    test_count = test_count + 1
    success = tt_load_table_directory(stream, directory, error)
    
    if (.not. success) then
      print '("FAIL: Could not load table directory")'
      failed_count = failed_count + 1
      call ft_stream_close(stream)
      return
    end if
    
    print '("PASS: Load table directory")'
    
    ! Find head table
    test_count = test_count + 1
    head_idx = tt_find_table(directory, TTAG_head)
    
    if (head_idx > 0) then
      print '("PASS: Found head table at index ", I0)', head_idx
    else
      print '("FAIL: Could not find head table")'
      failed_count = failed_count + 1
    end if
    
    ! Find maxp table
    test_count = test_count + 1
    maxp_idx = tt_find_table(directory, TTAG_maxp)
    
    if (maxp_idx > 0) then
      print '("PASS: Found maxp table at index ", I0)', maxp_idx
    else
      print '("FAIL: Could not find maxp table")'
      failed_count = failed_count + 1
    end if
    
    ! Load head table
    if (head_idx > 0) then
      test_count = test_count + 1
      success = tt_load_head_table(stream, &
                                  int(directory%tables(head_idx)%offset, c_size_t), &
                                  head, error)
      
      if (success) then
        print '("PASS: Load head table from directory")'
        print '("  Units/EM: ", I0)', head%units_per_em
        print '("  BBox: [", I0, ",", I0, ",", I0, ",", I0, "]")', &
              head%x_min, head%y_min, head%x_max, head%y_max
      else
        print '("FAIL: Could not load head table")'
        failed_count = failed_count + 1
      end if
    end if
    
    ! Load maxp table
    if (maxp_idx > 0) then
      test_count = test_count + 1
      success = tt_load_maxp_table(stream, &
                                  int(directory%tables(maxp_idx)%offset, c_size_t), &
                                  maxp, error)
      
      if (success) then
        print '("PASS: Load maxp table from directory")'
        print '("  Num glyphs: ", I0)', maxp%num_glyphs
      else
        print '("FAIL: Could not load maxp table")'
        failed_count = failed_count + 1
      end if
    end if
    
    ! Clean up
    if (allocated(directory%tables)) deallocate(directory%tables)
    call ft_stream_close(stream)
    call delete_test_files()
    
  end subroutine test_multi_table_loading
  
  subroutine test_table_lookup_and_load()
    type(FT_Stream_Type) :: stream
    type(TT_Table_Directory) :: directory
    type(TT_Header_Table) :: head
    type(TT_MaxProfile) :: maxp
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing table lookup and consistency...")'
    
    ! Create test font
    call create_test_font_with_tables()
    
    ! Open and load directory
    success = ft_stream_open(stream, "test_multi.ttf", error)
    if (.not. success) return
    
    success = tt_load_table_directory(stream, directory, error)
    if (.not. success) then
      call ft_stream_close(stream)
      return
    end if
    
    ! Load both tables
    if (tt_find_table(directory, TTAG_head) > 0 .and. &
        tt_find_table(directory, TTAG_maxp) > 0) then
      
      success = tt_load_head_table(stream, &
          int(directory%tables(tt_find_table(directory, TTAG_head))%offset, c_size_t), &
          head, error)
      
      success = success .and. tt_load_maxp_table(stream, &
          int(directory%tables(tt_find_table(directory, TTAG_maxp))%offset, c_size_t), &
          maxp, error)
      
      if (success) then
        test_count = test_count + 1
        
        ! Cross-validate: index format determines loca table entry size
        ! If we have glyphs, we should have valid index format
        if (maxp%num_glyphs > 0 .and. &
            (head%index_to_loc_format == 0 .or. head%index_to_loc_format == 1)) then
          print '("PASS: Head and maxp tables consistent")'
        else
          print '("FAIL: Tables inconsistent")'
          failed_count = failed_count + 1
        end if
      end if
    end if
    
    ! Test non-existent table
    test_count = test_count + 1
    if (tt_find_table(directory, TTAG_glyf) == 0) then
      print '("PASS: Non-existent table returns 0")'
    else
      print '("FAIL: Should return 0 for missing table")'
      failed_count = failed_count + 1
    end if
    
    ! Clean up
    if (allocated(directory%tables)) deallocate(directory%tables)
    call ft_stream_close(stream)
    call delete_test_files()
    
  end subroutine test_table_lookup_and_load
  
  ! Helper: Create test font with multiple tables
  subroutine create_test_font_with_tables()
    integer :: unit
    integer(int32) :: val32
    integer(int16) :: val16
    integer(int64) :: val64
    
    open(newunit=unit, file="test_multi.ttf", access="stream", &
         form="unformatted", status="replace")
    
    ! Write header
    call write_int32_be(unit, int(z'00010000', int32))  ! version
    call write_int16_be(unit, 2_int16)  ! num_tables
    call write_int16_be(unit, 32_int16)  ! search_range
    call write_int16_be(unit, 1_int16)  ! entry_selector
    call write_int16_be(unit, 0_int16)  ! range_shift
    
    ! Write table directory entries
    ! Entry 1: head table
    call write_int32_be(unit, TTAG_head)
    call write_int32_be(unit, int(z'12345678', int32))  ! checksum
    call write_int32_be(unit, 44_int32)  ! offset (after header + 2 entries)
    call write_int32_be(unit, 54_int32)  ! length
    
    ! Entry 2: maxp table  
    call write_int32_be(unit, TTAG_maxp)
    call write_int32_be(unit, int(z'87654321', int32))  ! checksum
    call write_int32_be(unit, 98_int32)  ! offset (44 + 54)
    call write_int32_be(unit, 32_int32)  ! length
    
    ! Write head table at offset 44
    call write_int32_be(unit, int(z'00010000', int32))  ! version
    call write_int32_be(unit, int(z'00000000', int32))  ! font revision
    call write_int32_be(unit, int(z'00000000', int32))  ! checksum adjust
    call write_int32_be(unit, int(z'5F0F3CF5', int32))  ! magic number
    call write_int16_be(unit, 0_int16)   ! flags
    call write_int16_be(unit, 1024_int16)  ! units per EM
    
    ! Dates
    val64 = 0
    call write_int64_be(unit, val64)  ! created
    call write_int64_be(unit, val64)  ! modified
    
    ! Bounding box
    call write_int16_be(unit, -100_int16)  ! x_min
    call write_int16_be(unit, -200_int16)  ! y_min
    call write_int16_be(unit, 1000_int16)  ! x_max
    call write_int16_be(unit, 800_int16)   ! y_max
    
    ! Style info
    call write_int16_be(unit, 0_int16)   ! mac_style
    call write_int16_be(unit, 9_int16)   ! lowest_rec_ppem
    call write_int16_be(unit, 2_int16)   ! font_direction
    call write_int16_be(unit, 1_int16)   ! index_to_loc_format (long)
    call write_int16_be(unit, 0_int16)   ! glyph_data_format
    
    ! Write maxp table at offset 98
    call write_int32_be(unit, int(z'00010000', int32))  ! version 1.0
    call write_int16_be(unit, 100_int16)  ! numGlyphs
    ! Write remaining v1.0 fields
    call write_int16_be(unit, 50_int16)   ! maxPoints
    call write_int16_be(unit, 5_int16)    ! maxContours
    call write_int16_be(unit, 100_int16)  ! maxCompositePoints
    call write_int16_be(unit, 10_int16)   ! maxCompositeContours
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
  end subroutine
  
  ! Helper: Delete test files
  subroutine delete_test_files()
    integer :: unit, stat
    
    open(newunit=unit, file="test_multi.ttf", status="old", iostat=stat)
    if (stat == 0) close(unit, status="delete")
  end subroutine

end program test_tt_integration