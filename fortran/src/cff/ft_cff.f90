! FreeType CFF (Compact Font Format) parser module
! This module implements parsing for CFF/OpenType fonts
module ft_cff
  use ft_types
  use ft_stream
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  private
  
  ! Public types
  public :: FT_CFF_Header
  public :: FT_CFF_INDEX
  public :: FT_CFF_DICT
  public :: FT_CFF_Parser
  
  ! Public functions
  public :: ft_cff_detect_format
  public :: ft_cff_parser_new
  public :: ft_cff_parser_done
  public :: ft_cff_parse_header
  public :: ft_cff_parse_index
  public :: ft_cff_parse_dict
  
  ! CFF constants
  integer, parameter :: CFF_VERSION_1_0 = 1
  integer, parameter :: CFF_VERSION_2_0 = 2
  
  ! CFF header structure
  type :: FT_CFF_Header
    integer(int8) :: major_version
    integer(int8) :: minor_version
    integer(int8) :: header_size
    integer(int8) :: offset_size
  end type FT_CFF_Header
  
  ! CFF INDEX structure
  type :: FT_CFF_INDEX
    integer :: count
    integer(int8) :: offset_size
    integer, allocatable :: offsets(:)
    character(len=1), allocatable :: data(:)
    integer :: data_size
  end type FT_CFF_INDEX
  
  ! CFF DICT entry
  type :: FT_CFF_DICT_Entry
    integer :: operator
    real, allocatable :: operands(:)
    integer :: num_operands
  end type FT_CFF_DICT_Entry
  
  ! CFF DICT structure
  type :: FT_CFF_DICT
    type(FT_CFF_DICT_Entry), allocatable :: entries(:)
    integer :: num_entries
  end type FT_CFF_DICT
  
  ! CFF parser state
  type :: FT_CFF_Parser
    character(len=1), pointer :: data(:)
    integer :: data_length
    integer :: position
    type(FT_CFF_Header) :: header
    type(FT_CFF_INDEX) :: name_index
    type(FT_CFF_INDEX) :: top_dict_index
    type(FT_CFF_INDEX) :: string_index
    type(FT_CFF_INDEX) :: global_subr_index
    type(FT_CFF_DICT) :: top_dict
    logical :: is_cff2
  end type FT_CFF_Parser
  
  ! CFF DICT operators (subset of common ones)
  integer, parameter :: CFF_OP_VERSION = 0
  integer, parameter :: CFF_OP_NOTICE = 1
  integer, parameter :: CFF_OP_FULLNAME = 2
  integer, parameter :: CFF_OP_FAMILYNAME = 3
  integer, parameter :: CFF_OP_WEIGHT = 4
  integer, parameter :: CFF_OP_FONTBBOX = 5
  integer, parameter :: CFF_OP_CHARSET = 15
  integer, parameter :: CFF_OP_ENCODING = 16
  integer, parameter :: CFF_OP_CHARSTRINGS = 17
  integer, parameter :: CFF_OP_PRIVATE = 18
  integer, parameter :: CFF_OP_ROS = 1230  ! 12 30 - Registry-Ordering-Supplement
  integer, parameter :: CFF_OP_CIDCOUNT = 1234  ! 12 34
  
contains

  ! Detect CFF format in data
  function ft_cff_detect_format(data, data_length, is_cff2) result(is_cff)
    character(len=1), intent(in) :: data(:)
    integer, intent(in) :: data_length
    logical, intent(out) :: is_cff2
    logical :: is_cff
    
    integer :: major_version
    
    is_cff = .false.
    is_cff2 = .false.
    
    ! Check minimum length for CFF header
    if (data_length < 4) return
    
    ! Check major version
    major_version = ichar(data(1))
    
    if (major_version == CFF_VERSION_1_0) then
      is_cff = .true.
      is_cff2 = .false.
    else if (major_version == CFF_VERSION_2_0) then
      is_cff = .true.
      is_cff2 = .true.
    end if
    
  end function ft_cff_detect_format

  ! Create new CFF parser
  function ft_cff_parser_new(data, data_length, parser, error) result(success)
    character(len=1), target, intent(in) :: data(:)
    integer, intent(in) :: data_length
    type(FT_CFF_Parser), intent(out) :: parser
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    success = .false.
    error = FT_Err_Ok
    
    ! Check if data is CFF format
    if (.not. ft_cff_detect_format(data, data_length, parser%is_cff2)) then
      error = FT_Err_Invalid_File_Format
      return
    end if
    
    ! Initialize parser
    parser%data => data
    parser%data_length = data_length
    parser%position = 1
    
    ! Parse header
    if (.not. ft_cff_parse_header(parser, error)) then
      return
    end if
    
    success = .true.
    
  end function ft_cff_parser_new

  ! Clean up CFF parser
  subroutine ft_cff_parser_done(parser)
    type(FT_CFF_Parser), intent(inout) :: parser
    
    ! Clean up indices
    call cleanup_index(parser%name_index)
    call cleanup_index(parser%top_dict_index)
    call cleanup_index(parser%string_index)
    call cleanup_index(parser%global_subr_index)
    
    ! Clean up dictionary
    call cleanup_dict(parser%top_dict)
    
    ! Clear pointers
    parser%data => null()
    parser%data_length = 0
    parser%position = 0
    
  end subroutine ft_cff_parser_done

  ! Parse CFF header
  function ft_cff_parse_header(parser, error) result(success)
    type(FT_CFF_Parser), intent(inout) :: parser
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    success = .false.
    error = FT_Err_Ok
    
    ! Check we have enough data for header
    if (parser%data_length < 4) then
      error = FT_Err_Invalid_File_Format
      return
    end if
    
    ! Read header fields
    parser%header%major_version = int(ichar(parser%data(1)), int8)
    parser%header%minor_version = int(ichar(parser%data(2)), int8)
    parser%header%header_size = int(ichar(parser%data(3)), int8)
    parser%header%offset_size = int(ichar(parser%data(4)), int8)
    
    ! Validate header
    if (parser%header%major_version /= CFF_VERSION_1_0 .and. &
        parser%header%major_version /= CFF_VERSION_2_0) then
      error = FT_Err_Invalid_File_Format
      return
    end if
    
    if (parser%header%offset_size < 1 .or. parser%header%offset_size > 4) then
      error = FT_Err_Invalid_File_Format
      return
    end if
    
    ! Update position to after header
    parser%position = int(parser%header%header_size) + 1
    
    success = .true.
    
  end function ft_cff_parse_header

  ! Parse CFF INDEX structure
  function ft_cff_parse_index(parser, index, error) result(success)
    type(FT_CFF_Parser), intent(inout) :: parser
    type(FT_CFF_INDEX), intent(out) :: index
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: i, offset_pos, data_pos
    
    success = .false.
    error = FT_Err_Ok
    
    ! Check we have enough data for count
    if (parser%position + 2 > parser%data_length) then
      error = FT_Err_Invalid_File_Format
      return
    end if
    
    ! Read count (2 bytes, big-endian)
    index%count = ichar(parser%data(parser%position)) * 256 + &
                  ichar(parser%data(parser%position + 1))
    parser%position = parser%position + 2
    
    ! Handle empty index
    if (index%count == 0) then
      index%offset_size = 0
      success = .true.
      return
    end if
    
    ! Read offset size
    if (parser%position > parser%data_length) then
      error = FT_Err_Invalid_File_Format
      return
    end if
    
    index%offset_size = int(ichar(parser%data(parser%position)), int8)
    parser%position = parser%position + 1
    
    ! Validate offset size
    if (index%offset_size < 1 .or. index%offset_size > 4) then
      error = FT_Err_Invalid_File_Format
      return
    end if
    
    ! Allocate and read offsets
    allocate(index%offsets(index%count + 1))
    
    do i = 1, index%count + 1
      offset_pos = parser%position + (i - 1) * int(index%offset_size)
      if (offset_pos + int(index%offset_size) - 1 > parser%data_length) then
        error = FT_Err_Invalid_File_Format
        return
      end if
      
      index%offsets(i) = read_offset(parser%data, offset_pos, int(index%offset_size))
    end do
    
    parser%position = parser%position + (index%count + 1) * int(index%offset_size)
    
    ! Calculate data size and copy data
    index%data_size = index%offsets(index%count + 1) - index%offsets(1)
    
    if (index%data_size > 0) then
      allocate(index%data(index%data_size))
      
      data_pos = parser%position
      if (data_pos + index%data_size - 1 > parser%data_length) then
        error = FT_Err_Invalid_File_Format
        return
      end if
      
      index%data(1:index%data_size) = parser%data(data_pos:data_pos + index%data_size - 1)
      parser%position = parser%position + index%data_size
    end if
    
    success = .true.
    
  end function ft_cff_parse_index

  ! Parse CFF DICT
  function ft_cff_parse_dict(data, data_length, dict, error) result(success)
    character(len=1), intent(in) :: data(:)
    integer, intent(in) :: data_length
    type(FT_CFF_DICT), intent(out) :: dict
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: pos, max_entries
    integer :: operator, num_operands
    real :: operands(10)  ! Stack for operands
    
    success = .false.
    error = FT_Err_Ok
    
    ! Initialize dictionary
    max_entries = 50  ! Reasonable default
    allocate(dict%entries(max_entries))
    dict%num_entries = 0
    
    pos = 1
    num_operands = 0
    
    ! Parse dictionary entries
    do while (pos <= data_length)
      if (is_operator(data(pos))) then
        ! Read operator
        operator = read_operator(data, pos, data_length)
        if (operator == -1) exit  ! Invalid operator
        
        ! Store dictionary entry
        dict%num_entries = dict%num_entries + 1
        if (dict%num_entries > max_entries) then
          error = FT_Err_Invalid_File_Format
          return
        end if
        
        allocate(dict%entries(dict%num_entries)%operands(num_operands))
        dict%entries(dict%num_entries)%operator = operator
        dict%entries(dict%num_entries)%num_operands = num_operands
        if (num_operands > 0) then
          dict%entries(dict%num_entries)%operands(1:num_operands) = operands(1:num_operands)
        end if
        
        ! Reset operand stack
        num_operands = 0
      else
        ! Read operand
        if (num_operands >= 10) then
          error = FT_Err_Invalid_File_Format
          return
        end if
        
        num_operands = num_operands + 1
        operands(num_operands) = read_operand(data, pos, data_length)
        if (pos > data_length) exit
      end if
    end do
    
    success = .true.
    
  end function ft_cff_parse_dict

  ! Helper: Read variable-length offset
  function read_offset(data, pos, size) result(offset)
    character(len=1), intent(in) :: data(:)
    integer, intent(in) :: pos, size
    integer :: offset
    
    integer :: i
    
    offset = 0
    do i = 0, size - 1
      offset = offset * 256 + ichar(data(pos + i))
    end do
    
  end function read_offset

  ! Helper: Check if byte is an operator
  function is_operator(byte) result(is_op)
    character(len=1), intent(in) :: byte
    logical :: is_op
    
    integer :: val
    
    val = ichar(byte)
    is_op = (val >= 0 .and. val <= 21) .or. val == 12
    
  end function is_operator

  ! Helper: Read operator
  function read_operator(data, pos, data_length) result(operator)
    character(len=1), intent(in) :: data(:)
    integer, intent(inout) :: pos
    integer, intent(in) :: data_length
    integer :: operator
    
    integer :: b0, b1
    
    if (pos > data_length) then
      operator = -1
      return
    end if
    
    b0 = ichar(data(pos))
    pos = pos + 1
    
    if (b0 == 12) then
      ! Two-byte operator
      if (pos > data_length) then
        operator = -1
        return
      end if
      
      b1 = ichar(data(pos))
      pos = pos + 1
      operator = 1200 + b1
    else
      ! Single-byte operator
      operator = b0
    end if
    
  end function read_operator

  ! Helper: Read operand (simplified - just handles integers for now)
  function read_operand(data, pos, data_length) result(operand)
    character(len=1), intent(in) :: data(:)
    integer, intent(inout) :: pos
    integer, intent(in) :: data_length
    real :: operand
    
    integer :: b0, b1, b2, b3, b4
    
    if (pos > data_length) then
      operand = 0.0
      return
    end if
    
    b0 = ichar(data(pos))
    pos = pos + 1
    
    if (b0 >= 32 .and. b0 <= 246) then
      ! Small integer: -107 to +107
      operand = real(b0 - 139)
    else if (b0 >= 247 .and. b0 <= 250) then
      ! Positive integer: +108 to +1131
      if (pos > data_length) then
        operand = 0.0
        return
      end if
      b1 = ichar(data(pos))
      pos = pos + 1
      operand = real((b0 - 247) * 256 + b1 + 108)
    else if (b0 >= 251 .and. b0 <= 254) then
      ! Negative integer: -1131 to -108
      if (pos > data_length) then
        operand = 0.0
        return
      end if
      b1 = ichar(data(pos))
      pos = pos + 1
      operand = real(-(b0 - 251) * 256 - b1 - 108)
    else if (b0 == 28) then
      ! Short integer (3 bytes)
      if (pos + 1 > data_length) then
        operand = 0.0
        return
      end if
      b1 = ichar(data(pos))
      b2 = ichar(data(pos + 1))
      pos = pos + 2
      operand = real(b1 * 256 + b2)
    else if (b0 == 29) then
      ! Long integer (5 bytes)
      if (pos + 3 > data_length) then
        operand = 0.0
        return
      end if
      b1 = ichar(data(pos))
      b2 = ichar(data(pos + 1))
      b3 = ichar(data(pos + 2))
      b4 = ichar(data(pos + 3))
      pos = pos + 4
      operand = real(b1 * 16777216 + b2 * 65536 + b3 * 256 + b4)
    else
      ! Unsupported or invalid
      operand = 0.0
    end if
    
  end function read_operand

  ! Helper: Clean up INDEX
  subroutine cleanup_index(index)
    type(FT_CFF_INDEX), intent(inout) :: index
    
    if (allocated(index%offsets)) deallocate(index%offsets)
    if (allocated(index%data)) deallocate(index%data)
    index%count = 0
    index%data_size = 0
    
  end subroutine cleanup_index

  ! Helper: Clean up DICT
  subroutine cleanup_dict(dict)
    type(FT_CFF_DICT), intent(inout) :: dict
    
    integer :: i
    
    if (allocated(dict%entries)) then
      do i = 1, dict%num_entries
        if (allocated(dict%entries(i)%operands)) then
          deallocate(dict%entries(i)%operands)
        end if
      end do
      deallocate(dict%entries)
    end if
    
    dict%num_entries = 0
    
  end subroutine cleanup_dict

end module ft_cff