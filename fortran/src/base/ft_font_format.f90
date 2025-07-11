! Font format detection and routing module
module ft_font_format
  use ft_types
  use ft_stream
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int64
  implicit none
  private
  
  ! Public functions
  public :: ft_detect_font_format
  public :: ft_get_format_name
  
  ! Public constants
  public :: FT_FONT_FORMAT_UNKNOWN
  public :: FT_FONT_FORMAT_TRUETYPE
  public :: FT_FONT_FORMAT_TYPE1
  public :: FT_FONT_FORMAT_CFF
  public :: FT_FONT_FORMAT_OPENTYPE
  public :: FT_FONT_FORMAT_WOFF
  public :: FT_FONT_FORMAT_WOFF2
  public :: FT_FONT_FORMAT_PCF
  public :: FT_FONT_FORMAT_BDF
  
  ! Font format enumeration
  enum, bind(c)
    enumerator :: FT_FONT_FORMAT_UNKNOWN = 0
    enumerator :: FT_FONT_FORMAT_TRUETYPE = 1
    enumerator :: FT_FONT_FORMAT_TYPE1 = 2
    enumerator :: FT_FONT_FORMAT_CFF = 3
    enumerator :: FT_FONT_FORMAT_OPENTYPE = 4
    enumerator :: FT_FONT_FORMAT_WOFF = 5
    enumerator :: FT_FONT_FORMAT_WOFF2 = 6
    enumerator :: FT_FONT_FORMAT_PCF = 7
    enumerator :: FT_FONT_FORMAT_BDF = 8
  end enum
  
  ! Type alias for font format
  ! Note: Can't use FT_Font_Format as it conflicts with module name
  integer, parameter :: Font_Format_Type = c_int
  
contains

  ! Detect font format from stream
  function ft_detect_font_format(stream, format, error) result(success)
    type(FT_Stream_Type), intent(inout) :: stream
    integer(Font_Format_Type), intent(out) :: format
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    character(len=4) :: tag
    character(len=12) :: header
    integer(int8) :: bytes(12)
    integer :: pos_save, i
    
    success = .false.
    error = FT_Err_Ok
    format = FT_FONT_FORMAT_UNKNOWN
    
    ! Check if stream is valid
    if (.not. associated(stream%rec)) then
      error = FT_Err_Invalid_Stream_Handle
      return
    end if
    
    ! Save current position
    pos_save = int(stream%rec%pos)
    
    ! Read first 12 bytes for detection
    if (.not. ft_stream_seek(stream, 0_int64, error)) then
      print *, "Failed to seek to start, error=", error
      return
    end if
    
    ! Read bytes one by one
    do i = 1, 12
      if (.not. ft_stream_read_byte(stream, bytes(i), error)) then
        ! Restore position
        success = ft_stream_seek(stream, int(pos_save, int64), error)
        return
      end if
      header(i:i) = char(bytes(i))
    end do
    
    ! Check for TrueType/OpenType
    tag = header(1:4)
    if (tag == char(0) // char(1) // char(0) // char(0) .or. &  ! Version 1.0
        tag == 'true' .or. &                                     ! Mac TrueType
        tag == 'typ1' .or. &                                     ! Type 1 in SFNT
        tag == 'OTTO') then                                      ! OpenType with CFF
      if (tag == 'OTTO') then
        format = FT_FONT_FORMAT_OPENTYPE
      else
        format = FT_FONT_FORMAT_TRUETYPE
      end if
      success = .true.
    
    ! Check for CFF
    else if (ichar(header(1:1)) == 1 .or. ichar(header(1:1)) == 2) then
      ! CFF version 1 or 2
      format = FT_FONT_FORMAT_CFF
      success = .true.
    
    ! Check for Type 1
    else if (header(1:2) == '%!' .or. &                         ! PFA
             (ichar(header(1:1)) == 128 .and. &                 ! PFB
              ichar(header(2:2)) == 1)) then
      format = FT_FONT_FORMAT_TYPE1
      success = .true.
    
    ! Check for WOFF
    else if (tag == 'wOFF') then
      format = FT_FONT_FORMAT_WOFF
      success = .true.
    
    ! Check for WOFF2
    else if (tag == 'wOF2') then
      format = FT_FONT_FORMAT_WOFF2
      success = .true.
    
    ! Check for PCF
    else if (tag == char(1) // 'fcp') then
      format = FT_FONT_FORMAT_PCF
      success = .true.
    
    ! Check for BDF
    else if (index(header, 'STARTFONT') > 0) then
      format = FT_FONT_FORMAT_BDF
      success = .true.
    
    else
      error = FT_Err_Unknown_File_Format
    end if
    
    ! Restore stream position
    success = ft_stream_seek(stream, int(pos_save, int64), error)
    
  end function ft_detect_font_format

  ! Get human-readable format name
  function ft_get_format_name(format) result(name)
    integer(Font_Format_Type), intent(in) :: format
    character(len=32) :: name
    
    select case (format)
    case (FT_FONT_FORMAT_TRUETYPE)
      name = "TrueType"
    case (FT_FONT_FORMAT_TYPE1)
      name = "Type 1"
    case (FT_FONT_FORMAT_CFF)
      name = "CFF"
    case (FT_FONT_FORMAT_OPENTYPE)
      name = "OpenType"
    case (FT_FONT_FORMAT_WOFF)
      name = "WOFF"
    case (FT_FONT_FORMAT_WOFF2)
      name = "WOFF2"
    case (FT_FONT_FORMAT_PCF)
      name = "PCF"
    case (FT_FONT_FORMAT_BDF)
      name = "BDF"
    case default
      name = "Unknown"
    end select
    
  end function ft_get_format_name

end module ft_font_format