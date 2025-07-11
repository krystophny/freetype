program test_ft_face
  use ft_types
  use ft_face, only: FT_Face_Type, ft_new_face, ft_done_face, ft_get_char_index, ft_get_advance, &
                    FT_FACE_FLAG_SCALABLE, FT_FACE_FLAG_SFNT
  use tt_types
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Run all tests
  call test_face_creation()
  call test_face_properties()
  call test_char_to_glyph_mapping()
  call test_advance_width_access()
  call test_face_cleanup()
  
  ! Print test summary
  print '(/, "FT_Face Tests - Tests run: ", I0)', test_count
  print '("FT_Face Tests - Tests failed: ", I0)', failed_count
  
  if (failed_count == 0) then
    print '(/, "All FT_Face tests passed!")'
    stop 0
  else
    print '(/, "Some FT_Face tests failed!")'
    stop 1
  end if

contains

  subroutine test_face_creation()
    type(FT_Face_Type) :: face
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing face creation and loading...")'
    
    ! Create comprehensive test font
    call create_comprehensive_test_font()
    
    ! Test face creation
    test_count = test_count + 1
    success = ft_new_face("test_complete.ttf", 0_FT_Long, face, error)
    
    if (success .and. error == FT_Err_Ok) then
      print '("PASS: Face creation successful")'
      
      ! Test face is open
      test_count = test_count + 1
      if (face%is_open) then
        print '("PASS: Face is marked as open")'
      else
        print '("FAIL: Face should be marked as open")'
        failed_count = failed_count + 1
      end if
      
      ! Test basic properties exist
      test_count = test_count + 1
      if (len_trim(face%font_format) > 0) then
        print '("PASS: Font format is set")'
      else
        print '("FAIL: Font format not set")'
        failed_count = failed_count + 1
      end if
      
      ! Clean up
      call ft_done_face(face)
    else
      print '("FAIL: Face creation failed, error: ", I0)', error
      failed_count = failed_count + 1
    end if
    
    call delete_test_files()
    
  end subroutine test_face_creation
  
  subroutine test_face_properties()
    type(FT_Face_Type) :: face
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing face properties...")'
    
    call create_comprehensive_test_font()
    success = ft_new_face("test_complete.ttf", 0_FT_Long, face, error)
    
    if (.not. success) then
      call delete_test_files()
      return
    end if
    
    ! Test glyph count
    test_count = test_count + 1
    if (face%num_glyphs == 5) then
      print '("PASS: Correct number of glyphs")'
    else
      print '("FAIL: Incorrect glyph count: ", I0, " (expected 5)")', face%num_glyphs
      failed_count = failed_count + 1
    end if
    
    ! Test units per EM
    test_count = test_count + 1
    if (face%units_per_em == 2048) then
      print '("PASS: Correct units per EM")'
    else
      print '("FAIL: Incorrect units per EM: ", I0)', face%units_per_em
      failed_count = failed_count + 1
    end if
    
    ! Test face flags
    test_count = test_count + 1
    if (iand(face%face_flags, FT_FACE_FLAG_SCALABLE) /= 0) then
      print '("PASS: Scalable flag set")'
    else
      print '("FAIL: Scalable flag not set")'
      failed_count = failed_count + 1
    end if
    
    test_count = test_count + 1
    if (iand(face%face_flags, FT_FACE_FLAG_SFNT) /= 0) then
      print '("PASS: SFNT flag set")'
    else
      print '("FAIL: SFNT flag not set")'
      failed_count = failed_count + 1
    end if
    
    ! Test character maps
    test_count = test_count + 1
    if (face%num_charmaps >= 1) then
      print '("PASS: Has character maps")'
    else
      print '("FAIL: No character maps found")'
      failed_count = failed_count + 1
    end if
    
    call ft_done_face(face)
    call delete_test_files()
    
  end subroutine test_face_properties
  
  subroutine test_char_to_glyph_mapping()
    type(FT_Face_Type) :: face
    integer(FT_Error) :: error
    logical :: success
    integer(FT_UShort) :: glyph_index
    
    print '(/, "Testing character to glyph mapping...")'
    
    call create_comprehensive_test_font()
    success = ft_new_face("test_complete.ttf", 0_FT_Long, face, error)
    
    if (.not. success) then
      call delete_test_files()
      return
    end if
    
    ! Test ASCII 'A' mapping (should map to glyph 36 based on our cmap test data)
    test_count = test_count + 1
    glyph_index = ft_get_char_index(face, 65_FT_ULong)  ! 'A'
    
    if (glyph_index == 36) then
      print '("PASS: Character ''A'' maps to glyph 36")'
    else
      print '("FAIL: Character ''A'' maps to glyph ", I0, " (expected 36)")', glyph_index
      failed_count = failed_count + 1
    end if
    
    ! Test space character
    test_count = test_count + 1
    glyph_index = ft_get_char_index(face, 32_FT_ULong)  ! space
    
    if (glyph_index == 3) then
      print '("PASS: Space character maps to glyph 3")'
    else
      print '("FAIL: Space character maps to glyph ", I0, " (expected 3)")', glyph_index
      failed_count = failed_count + 1
    end if
    
    ! Test unmapped character
    test_count = test_count + 1
    glyph_index = ft_get_char_index(face, 1000_FT_ULong)
    
    if (glyph_index == 0) then
      print '("PASS: Unmapped character returns glyph 0")'
    else
      print '("FAIL: Unmapped character should return 0")'
      failed_count = failed_count + 1
    end if
    
    call ft_done_face(face)
    call delete_test_files()
    
  end subroutine test_char_to_glyph_mapping
  
  subroutine test_advance_width_access()
    type(FT_Face_Type) :: face
    integer(FT_Error) :: error
    logical :: success
    integer(FT_Fixed) :: advance
    
    print '(/, "Testing advance width access...")'
    
    call create_comprehensive_test_font()
    success = ft_new_face("test_complete.ttf", 0_FT_Long, face, error)
    
    if (.not. success) then
      call delete_test_files()
      return
    end if
    
    ! Test advance for glyph 0 (should be 1000 * 65536 in 16.16 format)
    test_count = test_count + 1
    advance = ft_get_advance(face, 0_FT_UShort)
    
    if (advance == 1000 * 65536) then
      print '("PASS: Glyph 0 advance width correct")'
    else
      print '("FAIL: Glyph 0 advance incorrect: ", I0)', advance
      failed_count = failed_count + 1
    end if
    
    ! Test advance for glyph 1
    test_count = test_count + 1
    advance = ft_get_advance(face, 1_FT_UShort)
    
    if (advance == 1200 * 65536) then
      print '("PASS: Glyph 1 advance width correct")'
    else
      print '("FAIL: Glyph 1 advance incorrect: ", I0)', advance
      failed_count = failed_count + 1
    end if
    
    call ft_done_face(face)
    call delete_test_files()
    
  end subroutine test_advance_width_access
  
  subroutine test_face_cleanup()
    type(FT_Face_Type) :: face
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing face cleanup...")'
    
    call create_comprehensive_test_font()
    success = ft_new_face("test_complete.ttf", 0_FT_Long, face, error)
    
    if (.not. success) then
      call delete_test_files()
      return
    end if
    
    ! Test cleanup
    call ft_done_face(face)
    
    test_count = test_count + 1
    if (.not. face%is_open) then
      print '("PASS: Face marked as closed after cleanup")'
    else
      print '("FAIL: Face should be marked as closed")'
      failed_count = failed_count + 1
    end if
    
    test_count = test_count + 1
    if (face%num_glyphs == 0) then
      print '("PASS: Glyph count reset")'
    else
      print '("FAIL: Glyph count not reset")'
      failed_count = failed_count + 1
    end if
    
    call delete_test_files()
    
  end subroutine test_face_cleanup
  
  ! Create a comprehensive test font with all required tables
  subroutine create_comprehensive_test_font()
    integer :: unit
    integer(c_size_t) :: offset
    
    open(newunit=unit, file="test_complete.ttf", access="stream", &
         form="unformatted", status="replace")
    
    ! Write header (5 tables: head, maxp, cmap, loca, hmtx)
    call write_int32_be(unit, int(z'00010000', int32))  ! version
    call write_int16_be(unit, 5_int16)                  ! num_tables
    call write_int16_be(unit, 64_int16)                 ! search_range
    call write_int16_be(unit, 2_int16)                  ! entry_selector
    call write_int16_be(unit, 16_int16)                 ! range_shift
    
    ! Calculate table offsets
    offset = 12 + 5 * 16  ! header + 5 table entries = 92
    
    ! Write table directory
    ! head table
    call write_int32_be(unit, TTAG_head)
    call write_int32_be(unit, int(z'12345678', int32))
    call write_int32_be(unit, int(offset, int32))
    call write_int32_be(unit, 54_int32)
    offset = offset + 54
    
    ! maxp table  
    call write_int32_be(unit, TTAG_maxp)
    call write_int32_be(unit, int(z'23456789', int32))
    call write_int32_be(unit, int(offset, int32))
    call write_int32_be(unit, 32_int32)
    offset = offset + 32
    
    ! cmap table
    call write_int32_be(unit, TTAG_cmap)
    call write_int32_be(unit, int(z'34567890', int32))
    call write_int32_be(unit, int(offset, int32))
    call write_int32_be(unit, 100_int32)
    offset = offset + 100
    
    ! loca table
    call write_int32_be(unit, TTAG_loca)
    call write_int32_be(unit, int(z'45678901', int32))
    call write_int32_be(unit, int(offset, int32))
    call write_int32_be(unit, 12_int32)  ! 6 offsets * 2 bytes (short format)
    offset = offset + 12
    
    ! hmtx table
    call write_int32_be(unit, TTAG_hmtx)
    call write_int32_be(unit, int(z'56789012', int32))
    call write_int32_be(unit, int(offset, int32))
    call write_int32_be(unit, 16_int32)  ! 3 metrics * 4 bytes + 2 LSBs * 2 bytes
    
    ! Write head table
    call write_int32_be(unit, int(z'00010000', int32))  ! version
    call write_int32_be(unit, int(z'00000001', int32))  ! revision
    call write_int32_be(unit, int(z'00000000', int32))  ! checksum_adjust
    call write_int32_be(unit, int(z'5F0F3CF5', int32))  ! magic
    call write_int16_be(unit, 0_int16)                  ! flags
    call write_int16_be(unit, 2048_int16)               ! units_per_em
    call write_int64_be(unit, int(z'0000000000000000', int64))  ! created
    call write_int64_be(unit, int(z'0000000000000000', int64))  ! modified
    call write_int16_be(unit, 0_int16)                  ! xMin
    call write_int16_be(unit, 0_int16)                  ! yMin
    call write_int16_be(unit, 1000_int16)               ! xMax
    call write_int16_be(unit, 1000_int16)               ! yMax
    call write_int16_be(unit, 0_int16)                  ! macStyle
    call write_int16_be(unit, 8_int16)                  ! lowestRecPPEM
    call write_int16_be(unit, 2_int16)                  ! fontDirectionHint
    call write_int16_be(unit, 0_int16)                  ! indexToLocFormat (short)
    call write_int16_be(unit, 0_int16)                  ! glyphDataFormat
    
    ! Write maxp table (version 1.0)
    call write_int32_be(unit, int(z'00010000', int32))  ! version
    call write_int16_be(unit, 5_int16)                  ! numGlyphs
    call write_int16_be(unit, 100_int16)                ! maxPoints
    call write_int16_be(unit, 10_int16)                 ! maxContours
    call write_int16_be(unit, 200_int16)                ! maxCompositePoints
    call write_int16_be(unit, 20_int16)                 ! maxCompositeContours
    call write_int16_be(unit, 2_int16)                  ! maxZones
    call write_int16_be(unit, 50_int16)                 ! maxTwilightPoints
    call write_int16_be(unit, 10_int16)                 ! maxStorage
    call write_int16_be(unit, 20_int16)                 ! maxFunctionDefs
    call write_int16_be(unit, 30_int16)                 ! maxInstructionDefs
    call write_int16_be(unit, 40_int16)                 ! maxStackElements
    call write_int16_be(unit, 500_int16)                ! maxSizeOfInstructions
    call write_int16_be(unit, 5_int16)                  ! maxComponentElements
    call write_int16_be(unit, 2_int16)                  ! maxComponentDepth
    
    ! Write cmap table (format 4)
    call write_int16_be(unit, 0_int16)                  ! version
    call write_int16_be(unit, 1_int16)                  ! numTables
    
    ! Encoding record
    call write_int16_be(unit, 3_int16)                  ! platformID (Microsoft)
    call write_int16_be(unit, 1_int16)                  ! encodingID (Unicode BMP)
    call write_int32_be(unit, 12_int32)                 ! offset
    
    ! Format 4 subtable
    call write_int16_be(unit, 4_int16)                  ! format
    call write_int16_be(unit, 40_int16)                 ! length
    call write_int16_be(unit, 0_int16)                  ! language
    call write_int16_be(unit, 6_int16)                  ! segCountX2 (3 segments)
    call write_int16_be(unit, 4_int16)                  ! searchRange
    call write_int16_be(unit, 1_int16)                  ! entrySelector
    call write_int16_be(unit, 2_int16)                  ! rangeShift
    
    ! End codes: space(32), A-Z(90), terminator(0xFFFF)
    call write_int16_be(unit, 32_int16)
    call write_int16_be(unit, 90_int16)
    call write_int16_be(unit, -1_int16)
    call write_int16_be(unit, 0_int16)                  ! reserved pad
    
    ! Start codes
    call write_int16_be(unit, 32_int16)
    call write_int16_be(unit, 65_int16)
    call write_int16_be(unit, -1_int16)
    
    ! ID deltas
    call write_int16_be(unit, -29_int16)                ! 32 - 29 = 3
    call write_int16_be(unit, -29_int16)                ! 65 - 29 = 36
    call write_int16_be(unit, 1_int16)
    
    ! ID range offsets
    call write_int16_be(unit, 0_int16)
    call write_int16_be(unit, 0_int16)
    call write_int16_be(unit, 0_int16)
    
    ! Write loca table (short format, 5 glyphs + 1)
    call write_int16_be(unit, 0_int16)     ! glyph 0 at offset 0
    call write_int16_be(unit, 10_int16)    ! glyph 1 at offset 20
    call write_int16_be(unit, 20_int16)    ! glyph 2 at offset 40
    call write_int16_be(unit, 30_int16)    ! glyph 3 at offset 60
    call write_int16_be(unit, 40_int16)    ! glyph 4 at offset 80
    call write_int16_be(unit, 50_int16)    ! end at offset 100
    
    ! Write hmtx table (3 metrics + 2 additional LSBs)
    call write_int16_be(unit, 1000_int16)  ! advance
    call write_int16_be(unit, 100_int16)   ! lsb
    call write_int16_be(unit, 1200_int16)
    call write_int16_be(unit, 150_int16)
    call write_int16_be(unit, 800_int16)
    call write_int16_be(unit, 80_int16)
    call write_int16_be(unit, 90_int16)    ! additional LSB
    call write_int16_be(unit, 70_int16)    ! additional LSB
    
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
      bytes(i) = int(iand(ishft(value, -(8-i)*8), int(255, int64)), int8)
    end do
    
    write(unit) bytes
  end subroutine
  
  ! Helper: Delete test files
  subroutine delete_test_files()
    integer :: unit, stat
    
    open(newunit=unit, file="test_complete.ttf", status="old", iostat=stat)
    if (stat == 0) close(unit, status="delete")
  end subroutine

end program test_ft_face