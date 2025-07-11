program create_test_font
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64
  implicit none
  
  integer :: unit
  integer :: i
  character(len=256) :: filename
  
  ! Create a minimal but valid TrueType font
  filename = "test_font.ttf"
  
  open(newunit=unit, file=filename, access='stream', form='unformatted', &
       status='replace', action='write')
  
  ! Write TrueType header
  call write_uint32(unit, int(z'00010000', int32))  ! sfnt version
  call write_uint16(unit, 4_int16)                  ! numTables
  call write_uint16(unit, 64_int16)                 ! searchRange
  call write_uint16(unit, 2_int16)                  ! entrySelector  
  call write_uint16(unit, 0_int16)                  ! rangeShift
  
  ! Table directory entries (cmap, head, maxp, hmtx)
  ! cmap table
  call write_tag(unit, 'cmap')
  call write_uint32(unit, 0_int32)                  ! checkSum
  call write_uint32(unit, 96_int32)                 ! offset
  call write_uint32(unit, 28_int32)                 ! length
  
  ! head table
  call write_tag(unit, 'head')
  call write_uint32(unit, 0_int32)                  ! checkSum
  call write_uint32(unit, 124_int32)                ! offset
  call write_uint32(unit, 54_int32)                 ! length
  
  ! hmtx table
  call write_tag(unit, 'hmtx')
  call write_uint32(unit, 0_int32)                  ! checkSum
  call write_uint32(unit, 180_int32)                ! offset  
  call write_uint32(unit, 8_int32)                  ! length
  
  ! maxp table
  call write_tag(unit, 'maxp')
  call write_uint32(unit, 0_int32)                  ! checkSum
  call write_uint32(unit, 188_int32)                ! offset
  call write_uint32(unit, 6_int32)                  ! length
  
  ! Pad to offset 96 (cmap table)
  do i = 80, 95
    write(unit) int(0, int8)
  end do
  
  ! Write cmap table (minimal format 4)
  call write_uint16(unit, 0_int16)                  ! version
  call write_uint16(unit, 1_int16)                  ! numTables
  
  ! cmap subtable record
  call write_uint16(unit, 0_int16)                  ! platformID (Unicode)
  call write_uint16(unit, 3_int16)                  ! encodingID (Unicode 2.0)
  call write_uint32(unit, 12_int32)                 ! offset from cmap start
  
  ! Format 4 subtable (minimal)
  call write_uint16(unit, 4_int16)                  ! format
  call write_uint16(unit, 16_int16)                 ! length
  call write_uint16(unit, 0_int16)                  ! language
  call write_uint16(unit, 2_int16)                  ! segCountX2
  call write_uint16(unit, 2_int16)                  ! searchRange
  call write_uint16(unit, 0_int16)                  ! entrySelector
  call write_uint16(unit, 0_int16)                  ! rangeShift
  
  ! Write head table at offset 124
  call write_uint32(unit, int(z'00010000', int32))  ! version
  call write_uint32(unit, int(z'00010000', int32))  ! fontRevision
  call write_uint32(unit, 0_int32)                  ! checkSumAdjustment
  call write_uint32(unit, int(z'5F0F3CF5', int32))  ! magicNumber
  call write_uint16(unit, 0_int16)                  ! flags
  call write_uint16(unit, 2048_int16)               ! unitsPerEm
  call write_int64(unit, 0_int64)                   ! created
  call write_int64(unit, 0_int64)                   ! modified
  call write_int16(unit, 0_int16)                   ! xMin
  call write_int16(unit, 0_int16)                   ! yMin
  call write_int16(unit, 100_int16)                 ! xMax
  call write_int16(unit, 100_int16)                 ! yMax
  call write_uint16(unit, 0_int16)                  ! macStyle
  call write_uint16(unit, 7_int16)                  ! lowestRecPPEM
  call write_int16(unit, 2_int16)                   ! fontDirectionHint
  call write_int16(unit, 0_int16)                   ! indexToLocFormat
  call write_int16(unit, 0_int16)                   ! glyphDataFormat
  
  ! Write hmtx table at offset 180
  call write_uint16(unit, 100_int16)                ! advanceWidth
  call write_int16(unit, 0_int16)                   ! lsb
  call write_uint16(unit, 100_int16)                ! advanceWidth
  call write_int16(unit, 0_int16)                   ! lsb
  
  ! Write maxp table at offset 188
  call write_uint32(unit, int(z'00005000', int32))  ! version (0.5)
  call write_uint16(unit, 2_int16)                  ! numGlyphs
  
  close(unit)
  
  print '("Created test font: ", A)', trim(filename)
  print '("File size: 194 bytes")'
  
contains
  
  subroutine write_tag(unit, tag)
    integer, intent(in) :: unit
    character(len=4), intent(in) :: tag
    write(unit) tag
  end subroutine write_tag
  
  subroutine write_uint16(unit, value)
    integer, intent(in) :: unit
    integer(int16), intent(in) :: value
    write(unit) ishft(iand(value, int(z'FF00', int16)), -8)
    write(unit) iand(value, int(z'00FF', int16))
  end subroutine write_uint16
  
  subroutine write_int16(unit, value)
    integer, intent(in) :: unit
    integer(int16), intent(in) :: value
    call write_uint16(unit, value)
  end subroutine write_int16
  
  subroutine write_uint32(unit, value)
    integer, intent(in) :: unit
    integer(int32), intent(in) :: value
    write(unit) int(ishft(iand(value, int(z'FF000000', int32)), -24), int8)
    write(unit) int(ishft(iand(value, int(z'00FF0000', int32)), -16), int8)
    write(unit) int(ishft(iand(value, int(z'0000FF00', int32)), -8), int8)
    write(unit) int(iand(value, int(z'000000FF', int32)), int8)
  end subroutine write_uint32
  
  subroutine write_int64(unit, value)
    integer, intent(in) :: unit
    integer(int64), intent(in) :: value
    integer :: i
    do i = 56, 0, -8
      write(unit) int(ishft(value, -i), int8)
    end do
  end subroutine write_int64
  
end program create_test_font