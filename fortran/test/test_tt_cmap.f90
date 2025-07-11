program test_tt_cmap
  use ft_types
  use ft_stream
  use tt_types
  use tt_cmap
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Run all tests
  call test_cmap_header_loading()
  call test_cmap_format4_loading()
  call test_char_to_glyph_mapping()
  
  ! Print test summary
  print '(/, "CMap Table Tests - Tests run: ", I0)', test_count
  print '("CMap Table Tests - Tests failed: ", I0)', failed_count
  
  if (failed_count == 0) then
    print '(/, "All cmap table tests passed!")'
    stop 0
  else
    print '(/, "Some cmap table tests failed!")'
    stop 1
  end if

contains

  subroutine test_cmap_header_loading()
    type(FT_Stream_Type) :: stream
    type(TT_CMap_Table) :: cmap
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing cmap header loading...")'
    
    ! Create test font with cmap table
    call create_test_font_with_cmap()
    
    ! Open stream
    success = ft_stream_open(stream, "test_cmap.ttf", error)
    if (.not. success) then
      print '("FAIL: Could not open test file")'
      failed_count = failed_count + 1
      return
    end if
    
    ! Test loading cmap table
    test_count = test_count + 1
    success = tt_load_cmap_table(stream, 28_c_size_t, cmap, error)
    
    if (success .and. error == FT_Err_Ok) then
      print '("PASS: Load cmap table")'
      
      ! Check version
      test_count = test_count + 1
      if (cmap%header%version == 0) then
        print '("PASS: CMap version correct")'
      else
        print '("FAIL: CMap version incorrect: ", I0)', cmap%header%version
        failed_count = failed_count + 1
      end if
      
      ! Check number of tables
      test_count = test_count + 1
      if (cmap%header%num_tables == 1) then
        print '("PASS: Number of encoding tables correct")'
      else
        print '("FAIL: Number of tables incorrect: ", I0)', cmap%header%num_tables
        failed_count = failed_count + 1
      end if
      
      ! Clean up
      call tt_cmap_free(cmap)
    else
      print '("FAIL: Could not load cmap table, error: ", I0)', error
      failed_count = failed_count + 1
    end if
    
    call ft_stream_close(stream)
    call delete_test_files()
    
  end subroutine test_cmap_header_loading
  
  subroutine test_cmap_format4_loading()
    type(FT_Stream_Type) :: stream
    type(TT_CMap_Table) :: cmap
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing cmap format 4 loading...")'
    
    ! Create test font with format 4 cmap
    call create_test_font_with_cmap_format4()
    
    ! Open stream
    success = ft_stream_open(stream, "test_cmap_fmt4.ttf", error)
    if (.not. success) then
      print '("FAIL: Could not open test file")'
      failed_count = failed_count + 1
      return
    end if
    
    ! Load cmap table
    test_count = test_count + 1
    success = tt_load_cmap_table(stream, 28_c_size_t, cmap, error)
    
    if (success .and. cmap%has_format4) then
      print '("PASS: Load format 4 cmap")'
      
      ! Check format
      test_count = test_count + 1
      if (cmap%format4%format == 4) then
        print '("PASS: Format 4 identified")'
      else
        print '("FAIL: Format incorrect: ", I0)', cmap%format4%format
        failed_count = failed_count + 1
      end if
      
      ! Check segment count
      test_count = test_count + 1
      if (size(cmap%format4%end_code) == 3) then  ! We create 3 segments in test
        print '("PASS: Segment count correct")'
      else
        print '("FAIL: Segment count incorrect: ", I0)', size(cmap%format4%end_code)
        failed_count = failed_count + 1
      end if
      
      call tt_cmap_free(cmap)
    else
      print '("FAIL: Could not load format 4 cmap")'
      failed_count = failed_count + 1
    end if
    
    call ft_stream_close(stream)
    call delete_test_files()
    
  end subroutine test_cmap_format4_loading
  
  subroutine test_char_to_glyph_mapping()
    type(FT_Stream_Type) :: stream
    type(TT_CMap_Table) :: cmap
    integer(FT_Error) :: error
    logical :: success
    integer(FT_UShort) :: glyph_index
    
    print '(/, "Testing character to glyph mapping...")'
    
    ! Create test font
    call create_test_font_with_cmap_format4()
    
    ! Open and load
    success = ft_stream_open(stream, "test_cmap_fmt4.ttf", error)
    if (.not. success) return
    
    success = tt_load_cmap_table(stream, 28_c_size_t, cmap, error)
    if (.not. success) then
      call ft_stream_close(stream)
      return
    end if
    
    ! Test ASCII 'A' (65) -> glyph 36
    test_count = test_count + 1
    glyph_index = tt_cmap_char_to_glyph(cmap, 65_FT_ULong)
    
    if (glyph_index == 36) then
      print '("PASS: Map ''A'' to glyph 36")'
    else
      print '("FAIL: ''A'' mapped to glyph ", I0, " (expected 36)")', glyph_index
      failed_count = failed_count + 1
    end if
    
    ! Test ASCII space (32) -> glyph 3
    test_count = test_count + 1
    glyph_index = tt_cmap_char_to_glyph(cmap, 32_FT_ULong)
    
    if (glyph_index == 3) then
      print '("PASS: Map space to glyph 3")'
    else
      print '("FAIL: Space mapped to glyph ", I0, " (expected 3)")', glyph_index
      failed_count = failed_count + 1
    end if
    
    ! Test unmapped character
    test_count = test_count + 1
    glyph_index = tt_cmap_char_to_glyph(cmap, 1000_FT_ULong)
    
    if (glyph_index == 0) then
      print '("PASS: Unmapped character returns 0")'
    else
      print '("FAIL: Unmapped character returned ", I0)', glyph_index
      failed_count = failed_count + 1
    end if
    
    ! Clean up
    call tt_cmap_free(cmap)
    call ft_stream_close(stream)
    call delete_test_files()
    
  end subroutine test_char_to_glyph_mapping
  
  ! Helper: Create simple test font with cmap
  subroutine create_test_font_with_cmap()
    integer :: unit
    
    open(newunit=unit, file="test_cmap.ttf", access="stream", &
         form="unformatted", status="replace")
    
    ! Write header
    call write_int32_be(unit, int(z'00010000', int32))
    call write_int16_be(unit, 1_int16)  ! num_tables
    call write_int16_be(unit, 16_int16)
    call write_int16_be(unit, 0_int16)
    call write_int16_be(unit, 0_int16)
    
    ! Write table directory entry
    call write_int32_be(unit, TTAG_cmap)
    call write_int32_be(unit, int(z'12345678', int32))
    call write_int32_be(unit, 28_int32)  ! offset
    call write_int32_be(unit, 44_int32)  ! length (updated for full data)
    
    ! Write cmap header
    call write_int16_be(unit, 0_int16)   ! version
    call write_int16_be(unit, 1_int16)   ! numTables
    
    ! Write encoding record (Microsoft Unicode)
    call write_int16_be(unit, 3_int16)   ! platformID
    call write_int16_be(unit, 1_int16)   ! encodingID
    call write_int32_be(unit, 12_int32)  ! offset to subtable
    
    ! Write format 4 subtable header (minimal)
    call write_int16_be(unit, 4_int16)   ! format
    call write_int16_be(unit, 24_int16)  ! length (no glyphIdArray)
    call write_int16_be(unit, 0_int16)   ! language
    call write_int16_be(unit, 2_int16)   ! segCountX2 (1 segment)
    call write_int16_be(unit, 2_int16)   ! searchRange
    call write_int16_be(unit, 0_int16)   ! entrySelector
    call write_int16_be(unit, 0_int16)   ! rangeShift
    
    ! Minimal segment data
    call write_int16_be(unit, -1_int16)  ! endCode (0xFFFF)
    call write_int16_be(unit, 0_int16)   ! reserved
    call write_int16_be(unit, -1_int16)  ! startCode
    call write_int16_be(unit, 1_int16)   ! idDelta
    call write_int16_be(unit, 0_int16)   ! idRangeOffset
    
    close(unit)
  end subroutine
  
  ! Helper: Create test font with format 4 cmap
  subroutine create_test_font_with_cmap_format4()
    integer :: unit
    
    open(newunit=unit, file="test_cmap_fmt4.ttf", access="stream", &
         form="unformatted", status="replace")
    
    ! Write header
    call write_int32_be(unit, int(z'00010000', int32))
    call write_int16_be(unit, 1_int16)
    call write_int16_be(unit, 16_int16)
    call write_int16_be(unit, 0_int16)
    call write_int16_be(unit, 0_int16)
    
    ! Write table directory entry
    call write_int32_be(unit, TTAG_cmap)
    call write_int32_be(unit, int(z'12345678', int32))
    call write_int32_be(unit, 28_int32)
    call write_int32_be(unit, 100_int32)  ! length
    
    ! Write cmap header
    call write_int16_be(unit, 0_int16)   ! version
    call write_int16_be(unit, 1_int16)   ! numTables
    
    ! Write encoding record
    call write_int16_be(unit, 3_int16)   ! platformID (Microsoft)
    call write_int16_be(unit, 1_int16)   ! encodingID (Unicode BMP)
    call write_int32_be(unit, 12_int32)  ! offset
    
    ! Write format 4 subtable
    call write_int16_be(unit, 4_int16)   ! format
    call write_int16_be(unit, 40_int16)  ! length (no glyphIdArray)
    call write_int16_be(unit, 0_int16)   ! language
    call write_int16_be(unit, 6_int16)   ! segCountX2 (3 segments)
    call write_int16_be(unit, 4_int16)   ! searchRange
    call write_int16_be(unit, 1_int16)   ! entrySelector
    call write_int16_be(unit, 2_int16)   ! rangeShift
    
    ! End codes for 3 segments: space(32), A-Z(65-90), terminator(0xFFFF)
    call write_int16_be(unit, 32_int16)    ! segment 1: space
    call write_int16_be(unit, 90_int16)    ! segment 2: A-Z
    call write_int16_be(unit, -1_int16)    ! segment 3: 0xFFFF
    
    call write_int16_be(unit, 0_int16)     ! reserved pad
    
    ! Start codes
    call write_int16_be(unit, 32_int16)    ! segment 1: space
    call write_int16_be(unit, 65_int16)    ! segment 2: A-Z  
    call write_int16_be(unit, -1_int16)    ! segment 3: 0xFFFF
    
    ! ID deltas
    call write_int16_be(unit, -29_int16)   ! segment 1: 32 - 29 = 3
    call write_int16_be(unit, -29_int16)   ! segment 2: 65 - 29 = 36
    call write_int16_be(unit, 1_int16)     ! segment 3: required to be 1
    
    ! ID range offsets (all 0 - use delta)
    call write_int16_be(unit, 0_int16)
    call write_int16_be(unit, 0_int16)
    call write_int16_be(unit, 0_int16)
    
    ! No glyphIdArray needed when all offsets are 0
    
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
    
    open(newunit=unit, file="test_cmap.ttf", status="old", iostat=stat)
    if (stat == 0) close(unit, status="delete")
    
    open(newunit=unit, file="test_cmap_fmt4.ttf", status="old", iostat=stat)
    if (stat == 0) close(unit, status="delete")
  end subroutine

end program test_tt_cmap