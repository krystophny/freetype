module tt_head
  use ft_types
  use ft_stream
  use ft_geometry, only: FT_BBox
  use tt_types, only: FT_Short, FT_UShort, FT_Long, FT_ULong
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int16, int32, int64
  implicit none
  private
  
  ! Public types
  public :: TT_Header_Table
  
  ! Public functions
  public :: tt_load_head_table
  
  ! Head table structure (54 bytes)
  type, bind(C) :: TT_Header_Table
    integer(FT_Fixed) :: table_version      ! 0x00010000 for version 1.0
    integer(FT_Fixed) :: font_revision      ! Set by font manufacturer
    integer(FT_ULong) :: checksum_adjust    ! To compute: 0xB1B0AFBA - sum of entire font
    integer(FT_ULong) :: magic_number       ! Set to 0x5F0F3CF5
    integer(FT_UShort) :: flags             ! Bit flags
    integer(FT_UShort) :: units_per_em      ! Valid range: 16-16384
    integer(int64) :: created               ! International date (8 bytes)
    integer(int64) :: modified              ! International date (8 bytes)
    integer(FT_Short) :: x_min              ! For all glyph bounding boxes
    integer(FT_Short) :: y_min              ! For all glyph bounding boxes
    integer(FT_Short) :: x_max              ! For all glyph bounding boxes
    integer(FT_Short) :: y_max              ! For all glyph bounding boxes
    integer(FT_UShort) :: mac_style         ! Bit flags for style
    integer(FT_UShort) :: lowest_rec_ppem   ! Smallest readable size in pixels
    integer(FT_Short) :: font_direction     ! 0=mixed, 1=LTR, 2=L2R, -1=RTL, -2=R2L
    integer(FT_Short) :: index_to_loc_format ! 0=short offsets, 1=long offsets
    integer(FT_Short) :: glyph_data_format  ! 0=current format
  end type TT_Header_Table
  
  ! Magic number constant
  integer(FT_ULong), parameter :: HEAD_MAGIC = int(z'5F0F3CF5', FT_ULong)

contains

  ! Load head table from stream
  function tt_load_head_table(stream, offset, head, error) result(success)
    type(FT_Stream_Type), intent(inout) :: stream
    integer(c_size_t), intent(in) :: offset
    type(TT_Header_Table), intent(out) :: head
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer(int32) :: temp32
    integer(int16) :: temp16
    integer :: i
    integer(int64) :: date_bytes
    
    success = .false.
    error = FT_Err_Ok
    
    ! Seek to head table offset
    if (.not. ft_stream_seek(stream, offset, error)) then
      error = FT_Err_Invalid_Stream_Seek
      return
    end if
    
    ! Read table version
    if (.not. ft_stream_read_long(stream, temp32, error)) then
      error = FT_Err_Invalid_Table
      return
    end if
    head%table_version = temp32
    
    ! Check version
    if (head%table_version /= int(z'00010000', int32)) then
      error = FT_Err_Invalid_Table_Format
      return
    end if
    
    ! Read font revision
    if (.not. ft_stream_read_long(stream, temp32, error)) then
      return
    end if
    head%font_revision = temp32
    
    ! Read checksum adjust
    if (.not. ft_stream_read_long(stream, temp32, error)) then
      return
    end if
    head%checksum_adjust = temp32
    
    ! Read magic number
    if (.not. ft_stream_read_long(stream, temp32, error)) then
      return
    end if
    head%magic_number = temp32
    
    ! Validate magic number
    if (head%magic_number /= HEAD_MAGIC) then
      error = FT_Err_Invalid_Table_Format
      return
    end if
    
    ! Read flags
    if (.not. ft_stream_read_short(stream, temp16, error)) then
      return
    end if
    head%flags = temp16
    
    ! Read units per EM
    if (.not. ft_stream_read_short(stream, temp16, error)) then
      return
    end if
    head%units_per_em = temp16
    
    ! Validate units per EM
    if (head%units_per_em < 16 .or. head%units_per_em > 16384) then
      error = FT_Err_Invalid_Table
      return
    end if
    
    ! Read created date (8 bytes as two 32-bit reads)
    date_bytes = 0
    if (.not. ft_stream_read_long(stream, temp32, error)) then
      return
    end if
    date_bytes = ior(ishft(int(temp32, int64), 32), date_bytes)
    
    if (.not. ft_stream_read_long(stream, temp32, error)) then
      return
    end if
    date_bytes = ior(date_bytes, iand(int(temp32, int64), z'FFFFFFFF'))
    head%created = date_bytes
    
    ! Read modified date (8 bytes as two 32-bit reads)
    date_bytes = 0
    if (.not. ft_stream_read_long(stream, temp32, error)) then
      return
    end if
    date_bytes = ior(ishft(int(temp32, int64), 32), date_bytes)
    
    if (.not. ft_stream_read_long(stream, temp32, error)) then
      return
    end if
    date_bytes = ior(date_bytes, iand(int(temp32, int64), z'FFFFFFFF'))
    head%modified = date_bytes
    
    ! Read bounding box
    if (.not. ft_stream_read_short(stream, temp16, error)) then
      return
    end if
    head%x_min = temp16
    
    if (.not. ft_stream_read_short(stream, temp16, error)) then
      return
    end if
    head%y_min = temp16
    
    if (.not. ft_stream_read_short(stream, temp16, error)) then
      return
    end if
    head%x_max = temp16
    
    if (.not. ft_stream_read_short(stream, temp16, error)) then
      return
    end if
    head%y_max = temp16
    
    ! Read mac style
    if (.not. ft_stream_read_short(stream, temp16, error)) then
      return
    end if
    head%mac_style = temp16
    
    ! Read lowest rec ppem
    if (.not. ft_stream_read_short(stream, temp16, error)) then
      return
    end if
    head%lowest_rec_ppem = temp16
    
    ! Read font direction
    if (.not. ft_stream_read_short(stream, temp16, error)) then
      return
    end if
    head%font_direction = temp16
    
    ! Read index to loc format
    if (.not. ft_stream_read_short(stream, temp16, error)) then
      return
    end if
    head%index_to_loc_format = temp16
    
    ! Validate index format
    if (head%index_to_loc_format /= 0 .and. head%index_to_loc_format /= 1) then
      error = FT_Err_Invalid_Table
      return
    end if
    
    ! Read glyph data format
    if (.not. ft_stream_read_short(stream, temp16, error)) then
      return
    end if
    head%glyph_data_format = temp16
    
    success = .true.
  end function tt_load_head_table

end module tt_head