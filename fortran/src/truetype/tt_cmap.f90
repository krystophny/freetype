module tt_cmap
  use ft_types
  use ft_stream
  use tt_types, only: FT_UShort, FT_Short, FT_ULong
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int16, int32
  implicit none
  private
  
  ! Public types
  public :: TT_CMap_Header
  public :: TT_CMap_Encoding
  public :: TT_CMap_Format4
  public :: TT_CMap_Table
  
  ! Public functions
  public :: tt_load_cmap_table
  public :: tt_cmap_char_to_glyph
  public :: tt_cmap_free
  
  ! CMap table header
  type :: TT_CMap_Header
    integer(FT_UShort) :: version        ! Table version (should be 0)
    integer(FT_UShort) :: num_tables     ! Number of encoding tables
  end type TT_CMap_Header
  
  ! CMap encoding record
  type :: TT_CMap_Encoding
    integer(FT_UShort) :: platform_id    ! Platform ID
    integer(FT_UShort) :: encoding_id    ! Platform-specific encoding ID
    integer(FT_ULong) :: offset          ! Offset to subtable
  end type TT_CMap_Encoding
  
  ! Format 4: Segment mapping to delta values (most common for Unicode BMP)
  type :: TT_CMap_Format4
    integer(FT_UShort) :: format         ! Format number (4)
    integer(FT_UShort) :: length         ! Length of subtable
    integer(FT_UShort) :: language       ! Language code
    integer(FT_UShort) :: seg_count_x2   ! 2 * segCount
    integer(FT_UShort) :: search_range   ! 2 * (2^floor(log2(segCount)))
    integer(FT_UShort) :: entry_selector ! log2(searchRange/2)
    integer(FT_UShort) :: range_shift    ! 2 * segCount - searchRange
    
    ! Arrays (allocated based on segCount)
    integer(FT_UShort), allocatable :: end_code(:)      ! End character codes
    integer(FT_UShort), allocatable :: start_code(:)    ! Start character codes
    integer(FT_Short), allocatable :: id_delta(:)       ! Delta for all character codes
    integer(FT_UShort), allocatable :: id_range_offset(:) ! Offsets into glyphIdArray
    integer(FT_UShort), allocatable :: glyph_id_array(:)  ! Glyph index array
  end type TT_CMap_Format4
  
  ! Main CMap table structure
  type :: TT_CMap_Table
    type(TT_CMap_Header) :: header
    type(TT_CMap_Encoding), allocatable :: encodings(:)
    type(TT_CMap_Format4) :: format4     ! We'll support format 4 for now
    logical :: has_format4 = .false.
  end type TT_CMap_Table
  
  ! Platform IDs
  integer(FT_UShort), parameter :: TT_PLATFORM_APPLE_UNICODE = 0
  integer(FT_UShort), parameter :: TT_PLATFORM_MACINTOSH = 1
  integer(FT_UShort), parameter :: TT_PLATFORM_ISO = 2
  integer(FT_UShort), parameter :: TT_PLATFORM_MICROSOFT = 3

contains

  ! Load cmap table
  function tt_load_cmap_table(stream, offset, cmap, error) result(success)
    type(FT_Stream_Type), intent(inout) :: stream
    integer(c_size_t), intent(in) :: offset
    type(TT_CMap_Table), intent(out) :: cmap
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer(int16) :: temp16
    integer(int32) :: temp32
    integer :: i
    integer(c_size_t) :: subtable_offset
    logical :: found_format4
    
    success = .false.
    error = FT_Err_Ok
    cmap%has_format4 = .false.
    
    ! Seek to cmap table
    if (.not. ft_stream_seek(stream, offset, error)) then
      error = FT_Err_Invalid_Stream_Seek
      return
    end if
    
    ! Read header
    if (.not. ft_stream_read_short(stream, temp16, error)) return
    cmap%header%version = temp16
    
    if (.not. ft_stream_read_short(stream, temp16, error)) return
    cmap%header%num_tables = temp16
    
    if (cmap%header%num_tables == 0) then
      error = FT_Err_Invalid_Table
      return
    end if
    
    ! Allocate encoding records
    allocate(cmap%encodings(cmap%header%num_tables))
    
    ! Read encoding records
    found_format4 = .false.
    do i = 1, cmap%header%num_tables
      if (.not. ft_stream_read_short(stream, temp16, error)) then
        deallocate(cmap%encodings)
        return
      end if
      cmap%encodings(i)%platform_id = temp16
      
      if (.not. ft_stream_read_short(stream, temp16, error)) then
        deallocate(cmap%encodings)
        return
      end if
      cmap%encodings(i)%encoding_id = temp16
      
      if (.not. ft_stream_read_long(stream, temp32, error)) then
        deallocate(cmap%encodings)
        return
      end if
      cmap%encodings(i)%offset = temp32
      
      ! Look for Unicode BMP encoding (platform 3, encoding 1 or platform 0)
      if ((cmap%encodings(i)%platform_id == TT_PLATFORM_MICROSOFT .and. &
           cmap%encodings(i)%encoding_id == 1) .or. &
          cmap%encodings(i)%platform_id == TT_PLATFORM_APPLE_UNICODE) then
        subtable_offset = offset + cmap%encodings(i)%offset
        found_format4 = .true.
      end if
    end do
    
    ! Try to load format 4 subtable if found
    if (found_format4) then
      success = load_format4_subtable(stream, subtable_offset, cmap%format4, error)
      if (success) then
        cmap%has_format4 = .true.
      end if
    else
      ! No supported format found
      error = FT_Err_Unimplemented_Feature
    end if
    
  end function tt_load_cmap_table
  
  ! Load format 4 subtable
  function load_format4_subtable(stream, offset, fmt4, error) result(success)
    type(FT_Stream_Type), intent(inout) :: stream
    integer(c_size_t), intent(in) :: offset
    type(TT_CMap_Format4), intent(out) :: fmt4
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer(int16) :: temp16
    integer :: seg_count, i
    integer(c_size_t) :: glyph_array_size
    
    success = .false.
    error = FT_Err_Ok
    
    ! Seek to subtable
    if (.not. ft_stream_seek(stream, offset, error)) then
      error = FT_Err_Invalid_Stream_Seek
      return
    end if
    
    ! Read format
    if (.not. ft_stream_read_short(stream, temp16, error)) return
    fmt4%format = temp16
    
    if (fmt4%format /= 4) then
      error = FT_Err_Invalid_Table_Format
      return
    end if
    
    ! Read subtable header
    if (.not. ft_stream_read_short(stream, temp16, error)) return
    fmt4%length = temp16
    
    if (.not. ft_stream_read_short(stream, temp16, error)) return
    fmt4%language = temp16
    
    if (.not. ft_stream_read_short(stream, temp16, error)) return
    fmt4%seg_count_x2 = temp16
    seg_count = fmt4%seg_count_x2 / 2
    
    if (seg_count == 0) then
      error = FT_Err_Invalid_Table
      return
    end if
    
    if (.not. ft_stream_read_short(stream, temp16, error)) return
    fmt4%search_range = temp16
    
    if (.not. ft_stream_read_short(stream, temp16, error)) return
    fmt4%entry_selector = temp16
    
    if (.not. ft_stream_read_short(stream, temp16, error)) return
    fmt4%range_shift = temp16
    
    ! Allocate arrays
    allocate(fmt4%end_code(seg_count))
    allocate(fmt4%start_code(seg_count))
    allocate(fmt4%id_delta(seg_count))
    allocate(fmt4%id_range_offset(seg_count))
    
    ! Read endCode array
    do i = 1, seg_count
      if (.not. ft_stream_read_short(stream, temp16, error)) then
        call free_format4_arrays(fmt4)
        return
      end if
      fmt4%end_code(i) = temp16
    end do
    
    ! Skip reserved pad
    if (.not. ft_stream_read_short(stream, temp16, error)) then
      call free_format4_arrays(fmt4)
      return
    end if
    
    ! Read startCode array
    do i = 1, seg_count
      if (.not. ft_stream_read_short(stream, temp16, error)) then
        call free_format4_arrays(fmt4)
        return
      end if
      fmt4%start_code(i) = temp16
    end do
    
    ! Read idDelta array
    do i = 1, seg_count
      if (.not. ft_stream_read_short(stream, temp16, error)) then
        call free_format4_arrays(fmt4)
        return
      end if
      fmt4%id_delta(i) = temp16
    end do
    
    ! Read idRangeOffset array
    do i = 1, seg_count
      if (.not. ft_stream_read_short(stream, temp16, error)) then
        call free_format4_arrays(fmt4)
        return
      end if
      fmt4%id_range_offset(i) = temp16
    end do
    
    ! Calculate size of glyphIdArray
    ! It's whatever remains in the subtable
    ! Header is 14 bytes, then 2*(seg_count+1) for endCode + reserved,
    ! then 2*seg_count each for startCode, idDelta, idRangeOffset
    glyph_array_size = fmt4%length - 14 - 2 * (seg_count + 1) - 6 * seg_count
    if (glyph_array_size > 0 .and. glyph_array_size < fmt4%length) then
      allocate(fmt4%glyph_id_array(glyph_array_size / 2))
      
      do i = 1, size(fmt4%glyph_id_array)
        if (.not. ft_stream_read_short(stream, temp16, error)) then
          deallocate(fmt4%glyph_id_array)
          call free_format4_arrays(fmt4)
          return
        end if
        fmt4%glyph_id_array(i) = temp16
      end do
    end if
    
    success = .true.
    
  end function load_format4_subtable
  
  ! Map character code to glyph index using cmap
  function tt_cmap_char_to_glyph(cmap, char_code) result(glyph_index)
    type(TT_CMap_Table), intent(in) :: cmap
    integer(FT_ULong), intent(in) :: char_code
    integer(FT_UShort) :: glyph_index
    
    glyph_index = 0
    
    ! Only support format 4 for now
    if (cmap%has_format4) then
      glyph_index = char_to_glyph_format4(cmap%format4, char_code)
    end if
    
  end function tt_cmap_char_to_glyph
  
  ! Map character to glyph using format 4
  function char_to_glyph_format4(fmt4, char_code) result(glyph_index)
    type(TT_CMap_Format4), intent(in) :: fmt4
    integer(FT_ULong), intent(in) :: char_code
    integer(FT_UShort) :: glyph_index
    
    integer :: i, seg_count
    integer(FT_UShort) :: code
    integer :: offset_idx
    
    glyph_index = 0
    
    ! Format 4 only handles BMP (0-65535)
    if (char_code > 65535) return
    
    code = int(char_code, FT_UShort)
    seg_count = size(fmt4%end_code)
    
    ! Binary search for segment
    do i = 1, seg_count
      if (code <= fmt4%end_code(i)) then
        if (code >= fmt4%start_code(i)) then
          ! Found the segment
          if (fmt4%id_range_offset(i) == 0) then
            ! Use idDelta
            glyph_index = int(mod(code + fmt4%id_delta(i), 65536), FT_UShort)
          else
            ! Use glyphIdArray
            offset_idx = fmt4%id_range_offset(i) / 2 + (code - fmt4%start_code(i)) - (seg_count - i)
            if (allocated(fmt4%glyph_id_array) .and. &
                offset_idx > 0 .and. offset_idx <= size(fmt4%glyph_id_array)) then
              glyph_index = fmt4%glyph_id_array(offset_idx)
              if (glyph_index /= 0) then
                glyph_index = int(mod(glyph_index + fmt4%id_delta(i), 65536), FT_UShort)
              end if
            end if
          end if
        end if
        exit
      end if
    end do
    
  end function char_to_glyph_format4
  
  ! Free cmap table memory
  subroutine tt_cmap_free(cmap)
    type(TT_CMap_Table), intent(inout) :: cmap
    
    if (allocated(cmap%encodings)) deallocate(cmap%encodings)
    
    if (cmap%has_format4) then
      call free_format4_arrays(cmap%format4)
    end if
    
    cmap%has_format4 = .false.
  end subroutine tt_cmap_free
  
  ! Free format 4 arrays
  subroutine free_format4_arrays(fmt4)
    type(TT_CMap_Format4), intent(inout) :: fmt4
    
    if (allocated(fmt4%end_code)) deallocate(fmt4%end_code)
    if (allocated(fmt4%start_code)) deallocate(fmt4%start_code)
    if (allocated(fmt4%id_delta)) deallocate(fmt4%id_delta)
    if (allocated(fmt4%id_range_offset)) deallocate(fmt4%id_range_offset)
    if (allocated(fmt4%glyph_id_array)) deallocate(fmt4%glyph_id_array)
  end subroutine free_format4_arrays

end module tt_cmap