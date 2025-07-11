module ft_type1
  use ft_types
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  private
  
  ! Public types
  public :: FT_Type1_Format
  public :: FT_Type1_Parser
  
  ! Public constants
  public :: FT_TYPE1_FORMAT_UNKNOWN
  public :: FT_TYPE1_FORMAT_PFA
  public :: FT_TYPE1_FORMAT_PFB
  public :: FT_TYPE1_FORMAT_PFM
  
  ! Public functions
  public :: ft_type1_detect_format
  public :: ft_type1_parser_new
  public :: ft_type1_parser_done
  public :: ft_type1_parser_load_header
  
  ! Type 1 font format types
  integer, parameter :: FT_TYPE1_FORMAT_UNKNOWN = 0
  integer, parameter :: FT_TYPE1_FORMAT_PFA = 1  ! ASCII PostScript Type 1
  integer, parameter :: FT_TYPE1_FORMAT_PFB = 2  ! Binary PostScript Type 1
  integer, parameter :: FT_TYPE1_FORMAT_PFM = 3  ! Printer Font Metrics
  
  ! Type 1 font format enumeration
  type :: FT_Type1_Format
    integer :: format_type
    character(len=4) :: signature
    logical :: is_encrypted
    logical :: has_eexec
  end type FT_Type1_Format
  
  ! PFB segment types
  integer, parameter :: PFB_SEGMENT_ASCII = 1
  integer, parameter :: PFB_SEGMENT_BINARY = 2
  integer, parameter :: PFB_SEGMENT_EOF = 3
  
  ! PFB segment header
  type :: PFB_Segment
    integer :: segment_type
    integer :: length
    integer :: offset
  end type PFB_Segment
  
  ! Type 1 parser state
  type :: FT_Type1_Parser
    ! File data
    character(len=:), allocatable :: data
    integer :: data_length
    integer :: position
    
    ! Format information
    type(FT_Type1_Format) :: format
    
    ! PFB segments (if PFB format)
    type(PFB_Segment), allocatable :: segments(:)
    integer :: num_segments
    
    ! Current parsing context
    logical :: in_eexec
    character(len=256) :: current_line
    integer :: line_number
    
    ! Font information
    character(len=64) :: font_name
    character(len=64) :: full_name
    character(len=64) :: family_name
    character(len=64) :: weight
    character(len=64) :: version
    character(len=64) :: notice
    character(len=64) :: encoding
    logical :: is_fixed_pitch
    real :: italic_angle
    real :: underline_position
    real :: underline_thickness
    integer :: font_bbox(4)
    integer :: unique_id
    
    ! Character metrics
    integer :: num_chars
    
  end type FT_Type1_Parser
  
  ! Magic numbers for format detection
  character(len=*), parameter :: PFB_MAGIC = char(128) // 'PFB'
  character(len=*), parameter :: PFA_MAGIC = '%!PS-AdobeFont-'
  character(len=*), parameter :: PFM_MAGIC = char(0) // char(1) // char(0) // char(0)
  
contains

  ! Detect Type 1 font format from file data
  function ft_type1_detect_format(data, data_length, format, error) result(success)
    character(len=*), intent(in) :: data
    integer, intent(in) :: data_length
    type(FT_Type1_Format), intent(out) :: format
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    success = .false.
    error = FT_Err_Ok
    
    ! Initialize format
    format%format_type = FT_TYPE1_FORMAT_UNKNOWN
    format%signature = '    '
    format%is_encrypted = .false.
    format%has_eexec = .false.
    
    ! Check minimum length
    if (data_length < 4) then
      error = FT_Err_Invalid_File_Format
      return
    end if
    
    ! Check for PFB format (binary PostScript)
    if (data_length >= 2 .and. ichar(data(1:1)) == 128 .and. ichar(data(2:2)) == 1) then
      format%format_type = FT_TYPE1_FORMAT_PFB
      format%signature = 'PFB '
      format%is_encrypted = .true.
      format%has_eexec = .true.
      success = .true.
      return
    end if
    
    ! Check for PFA format (ASCII PostScript)
    if (data_length >= 13) then
      ! Use index function for more robust matching
      if (index(data, '%!PS-AdobeFont') == 1) then
        format%format_type = FT_TYPE1_FORMAT_PFA
        format%signature = 'PFA '
        format%is_encrypted = check_for_eexec(data, data_length)
        format%has_eexec = format%is_encrypted
        success = .true.
        return
      end if
    end if
    
    ! Check for alternative PFA signatures
    if (data_length >= 12 .and. data(1:12) == '%!FontType1-') then
      format%format_type = FT_TYPE1_FORMAT_PFA
      format%signature = 'PFA '
      format%is_encrypted = check_for_eexec(data, data_length)
      format%has_eexec = format%is_encrypted
      success = .true.
      return
    end if
    
    ! Check for PFM format (Windows Printer Font Metrics)
    if (data_length >= 4 .and. data(1:4) == char(0) // char(1) // char(0) // char(0)) then
      format%format_type = FT_TYPE1_FORMAT_PFM
      format%signature = 'PFM '
      format%is_encrypted = .false.
      format%has_eexec = .false.
      success = .true.
      return
    end if
    
    ! Unknown format
    error = FT_Err_Unknown_File_Format
    
  end function ft_type1_detect_format
  
  ! Create a new Type 1 parser
  function ft_type1_parser_new(data, data_length, parser, error) result(success)
    character(len=*), intent(in) :: data
    integer, intent(in) :: data_length
    type(FT_Type1_Parser), intent(out) :: parser
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    success = .false.
    error = FT_Err_Ok
    
    ! Allocate and copy data
    allocate(character(len=data_length) :: parser%data)
    parser%data = data(1:data_length)
    parser%data_length = data_length
    parser%position = 1
    
    ! Detect format
    if (.not. ft_type1_detect_format(data, data_length, parser%format, error)) then
      deallocate(parser%data)
      return
    end if
    
    ! Initialize parser state
    parser%in_eexec = .false.
    parser%current_line = ''
    parser%line_number = 0
    parser%num_segments = 0
    
    ! Initialize font information
    parser%font_name = ''
    parser%full_name = ''
    parser%family_name = ''
    parser%weight = ''
    parser%version = ''
    parser%notice = ''
    parser%encoding = 'StandardEncoding'
    parser%is_fixed_pitch = .false.
    parser%italic_angle = 0.0
    parser%underline_position = 0.0
    parser%underline_thickness = 0.0
    parser%font_bbox = [0, 0, 0, 0]
    parser%unique_id = 0
    parser%num_chars = 0
    
    ! Parse PFB segments if PFB format
    if (parser%format%format_type == FT_TYPE1_FORMAT_PFB) then
      if (.not. parse_pfb_segments(parser, error)) then
        deallocate(parser%data)
        return
      end if
    end if
    
    success = .true.
    
  end function ft_type1_parser_new
  
  ! Free Type 1 parser resources
  subroutine ft_type1_parser_done(parser)
    type(FT_Type1_Parser), intent(inout) :: parser
    
    if (allocated(parser%data)) then
      deallocate(parser%data)
    end if
    
    if (allocated(parser%segments)) then
      deallocate(parser%segments)
    end if
    
    parser%data_length = 0
    parser%position = 0
    parser%num_segments = 0
    
  end subroutine ft_type1_parser_done
  
  ! Load and parse Type 1 font header
  function ft_type1_parser_load_header(parser, error) result(success)
    type(FT_Type1_Parser), intent(inout) :: parser
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    success = .false.
    error = FT_Err_Ok
    
    ! Parse based on format type
    select case (parser%format%format_type)
    case (FT_TYPE1_FORMAT_PFA)
      success = parse_pfa_header(parser, error)
    case (FT_TYPE1_FORMAT_PFB)
      success = parse_pfb_header(parser, error)
    case (FT_TYPE1_FORMAT_PFM)
      success = parse_pfm_header(parser, error)
    case default
      error = FT_Err_Unknown_File_Format
    end select
    
  end function ft_type1_parser_load_header
  
  ! Check if data contains eexec section
  function check_for_eexec(data, data_length) result(has_eexec)
    character(len=*), intent(in) :: data
    integer, intent(in) :: data_length
    logical :: has_eexec
    
    integer :: i
    
    has_eexec = .false.
    
    ! Look for 'eexec' keyword
    do i = 1, data_length - 4
      if (data(i:i+4) == 'eexec') then
        has_eexec = .true.
        return
      end if
    end do
    
  end function check_for_eexec
  
  ! Parse PFB segments
  function parse_pfb_segments(parser, error) result(success)
    type(FT_Type1_Parser), intent(inout) :: parser
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: pos, segment_type, segment_length
    integer :: max_segments = 10
    
    success = .false.
    error = FT_Err_Ok
    
    ! Allocate segments array
    allocate(parser%segments(max_segments))
    parser%num_segments = 0
    
    pos = 1
    
    ! Parse all segments
    do while (pos < parser%data_length)
      ! Check for segment marker
      if (pos + 6 > parser%data_length) exit
      if (ichar(parser%data(pos:pos)) /= 128) then
        print '("DEBUG: Expected marker 128 at pos ", I0, " but got ", I0)', pos, ichar(parser%data(pos:pos))
        exit
      end if
      
      ! Get segment type
      segment_type = ichar(parser%data(pos+1:pos+1))
      if (segment_type == PFB_SEGMENT_EOF) exit
      
      ! Get segment length (little-endian 32-bit)
      segment_length = ichar(parser%data(pos+2:pos+2)) + &
                      ichar(parser%data(pos+3:pos+3)) * 256 + &
                      ichar(parser%data(pos+4:pos+4)) * 65536 + &
                      ichar(parser%data(pos+5:pos+5)) * 16777216
      
      print '("DEBUG: Segment ", I0, " type=", I0, " length=", I0, " offset=", I0)', &
            parser%num_segments + 1, segment_type, segment_length, pos + 6
      
      ! Store segment information
      parser%num_segments = parser%num_segments + 1
      if (parser%num_segments > max_segments) then
        error = FT_Err_Invalid_File_Format
        return
      end if
      
      parser%segments(parser%num_segments)%segment_type = segment_type
      parser%segments(parser%num_segments)%length = segment_length
      parser%segments(parser%num_segments)%offset = pos + 6
      
      ! Move to next segment
      pos = pos + 6 + segment_length
    end do
    
    if (parser%num_segments == 0) then
      error = FT_Err_Invalid_File_Format
      return
    end if
    
    success = .true.
    
  end function parse_pfb_segments
  
  ! Parse PFA header
  function parse_pfa_header(parser, error) result(success)
    type(FT_Type1_Parser), intent(inout) :: parser
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    success = .false.
    error = FT_Err_Ok
    
    ! Start from beginning
    parser%position = 1
    parser%line_number = 0
    
    ! Parse header information
    if (.not. parse_header_info(parser, error)) then
      return
    end if
    
    success = .true.
    
  end function parse_pfa_header
  
  ! Parse PFB header
  function parse_pfb_header(parser, error) result(success)
    type(FT_Type1_Parser), intent(inout) :: parser
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    success = .false.
    error = FT_Err_Ok
    
    ! Find ASCII segment (should be first)
    if (parser%num_segments < 1) then
      error = FT_Err_Invalid_File_Format
      return
    end if
    
    if (parser%segments(1)%segment_type /= PFB_SEGMENT_ASCII) then
      error = FT_Err_Invalid_File_Format
      return
    end if
    
    ! Set position to start of ASCII segment
    parser%position = parser%segments(1)%offset
    parser%line_number = 0
    
    ! Parse header information from ASCII segment
    if (.not. parse_header_info(parser, error)) then
      return
    end if
    
    success = .true.
    
  end function parse_pfb_header
  
  ! Parse PFM header (Windows Printer Font Metrics)
  function parse_pfm_header(parser, error) result(success)
    type(FT_Type1_Parser), intent(inout) :: parser
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    success = .false.
    error = FT_Err_Ok
    
    ! PFM format is not a font format itself, but metadata
    ! For now, mark as unsupported
    error = FT_Err_Unimplemented_Feature
    
  end function parse_pfm_header
  
  ! Parse header information from PostScript data
  function parse_header_info(parser, error) result(success)
    type(FT_Type1_Parser), intent(inout) :: parser
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    character(len=256) :: line
    integer :: line_end
    
    success = .false.
    error = FT_Err_Ok
    
    ! Read lines until we hit eexec or end of header
    do while (parser%position <= parser%data_length)
      ! Read next line
      if (.not. read_line(parser, line, line_end)) exit
      
      ! Check for eexec - end of header
      if (index(line, 'eexec') > 0) then
        parser%in_eexec = .true.
        exit
      end if
      
      ! Parse font information from line
      call parse_font_info_line(parser, line)
      
      ! Check for end of font dictionary
      if (index(line, 'end') > 0 .and. index(line, 'readonly') > 0) then
        exit
      end if
    end do
    
    success = .true.
    
  end function parse_header_info
  
  ! Read a line from the parser data
  function read_line(parser, line, line_end) result(success)
    type(FT_Type1_Parser), intent(inout) :: parser
    character(len=*), intent(out) :: line
    integer, intent(out) :: line_end
    logical :: success
    
    integer :: i, line_start
    
    success = .false.
    line = ''
    line_end = 0
    
    if (parser%position > parser%data_length) return
    
    line_start = parser%position
    
    ! Find end of line
    do i = parser%position, parser%data_length
      if (parser%data(i:i) == char(10) .or. parser%data(i:i) == char(13)) then
        line_end = i
        exit
      end if
    end do
    
    if (line_end == 0) then
      line_end = parser%data_length
    end if
    
    ! Copy line
    if (line_end > line_start) then
      line = parser%data(line_start:line_end-1)
      parser%position = line_end + 1
      
      ! Skip additional line terminators
      do while (parser%position <= parser%data_length)
        if (parser%data(parser%position:parser%position) == char(10) .or. &
            parser%data(parser%position:parser%position) == char(13)) then
          parser%position = parser%position + 1
        else
          exit
        end if
      end do
      
      parser%line_number = parser%line_number + 1
      success = .true.
    end if
    
  end function read_line
  
  ! Parse font information from a line
  subroutine parse_font_info_line(parser, line)
    type(FT_Type1_Parser), intent(inout) :: parser
    character(len=*), intent(in) :: line
    
    character(len=256) :: trimmed_line
    integer :: def_pos, value_start, value_end
    
    trimmed_line = trim(adjustl(line))
    
    ! Skip empty lines and comments
    if (len_trim(trimmed_line) == 0 .or. trimmed_line(1:1) == '%') return
    
    ! Look for font name
    if (index(trimmed_line, '/FontName') > 0) then
      call extract_name_value(trimmed_line, parser%font_name)
    end if
    
    ! Look for full name
    if (index(trimmed_line, '/FullName') > 0) then
      call extract_string_value(trimmed_line, parser%full_name)
    end if
    
    ! Look for family name
    if (index(trimmed_line, '/FamilyName') > 0) then
      call extract_string_value(trimmed_line, parser%family_name)
    end if
    
    ! Look for weight
    if (index(trimmed_line, '/Weight') > 0) then
      call extract_string_value(trimmed_line, parser%weight)
    end if
    
    ! Look for version
    if (index(trimmed_line, '/version') > 0) then
      call extract_string_value(trimmed_line, parser%version)
    end if
    
    ! Look for notice
    if (index(trimmed_line, '/Notice') > 0) then
      call extract_string_value(trimmed_line, parser%notice)
    end if
    
    ! Look for italic angle
    if (index(trimmed_line, '/ItalicAngle') > 0) then
      call extract_real_value(trimmed_line, parser%italic_angle)
    end if
    
    ! Look for underline position
    if (index(trimmed_line, '/UnderlinePosition') > 0) then
      call extract_real_value(trimmed_line, parser%underline_position)
    end if
    
    ! Look for underline thickness
    if (index(trimmed_line, '/UnderlineThickness') > 0) then
      call extract_real_value(trimmed_line, parser%underline_thickness)
    end if
    
    ! Look for fixed pitch
    if (index(trimmed_line, '/isFixedPitch') > 0) then
      parser%is_fixed_pitch = (index(trimmed_line, 'true') > 0)
    end if
    
  end subroutine parse_font_info_line
  
  ! Extract name value (e.g., /FontName /Times-Roman)
  subroutine extract_name_value(line, value)
    character(len=*), intent(in) :: line
    character(len=*), intent(out) :: value
    
    integer :: slash_pos, space_pos
    
    value = ''
    
    ! Find second slash
    slash_pos = index(line, '/')
    if (slash_pos > 0) then
      slash_pos = index(line(slash_pos+1:), '/') + slash_pos
      if (slash_pos > 0) then
        space_pos = index(line(slash_pos+1:), ' ')
        if (space_pos > 0) then
          value = line(slash_pos+1:slash_pos+space_pos-1)
        else
          value = line(slash_pos+1:)
        end if
      end if
    end if
    
  end subroutine extract_name_value
  
  ! Extract string value (e.g., /FullName (Times Roman))
  subroutine extract_string_value(line, value)
    character(len=*), intent(in) :: line
    character(len=*), intent(out) :: value
    
    integer :: paren_start, paren_end
    
    value = ''
    
    paren_start = index(line, '(')
    if (paren_start > 0) then
      paren_end = index(line(paren_start+1:), ')') + paren_start
      if (paren_end > paren_start) then
        value = line(paren_start+1:paren_end-1)
      end if
    end if
    
  end subroutine extract_string_value
  
  ! Extract real value (e.g., /ItalicAngle 0.0)
  subroutine extract_real_value(line, value)
    character(len=*), intent(in) :: line
    real, intent(out) :: value
    
    integer :: space_pos, def_pos
    character(len=32) :: num_str
    integer :: ios
    
    value = 0.0
    
    space_pos = index(line, ' ')
    if (space_pos > 0) then
      def_pos = index(line(space_pos+1:), ' ')
      if (def_pos > 0) then
        num_str = line(space_pos+1:space_pos+def_pos-1)
      else
        num_str = line(space_pos+1:)
      end if
      
      read(num_str, *, iostat=ios) value
      if (ios /= 0) value = 0.0
    end if
    
  end subroutine extract_real_value

end module ft_type1