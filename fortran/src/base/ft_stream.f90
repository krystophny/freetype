module ft_stream
  use ft_types
  use ft_memory
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64
  implicit none
  private
  
  ! Public types
  public :: FT_Stream_Type
  public :: FT_Stream_Rec
  
  ! Public functions
  public :: ft_stream_open
  public :: ft_stream_open_memory
  public :: ft_stream_close
  public :: ft_stream_read
  public :: ft_stream_read_byte
  public :: ft_stream_read_short
  public :: ft_stream_read_ushort
  public :: ft_stream_read_long
  public :: ft_stream_seek
  public :: ft_stream_tell
  public :: ft_stream_size
  
  ! Stream flags - public exports
  public :: FT_STREAM_FLAG_NONE
  public :: FT_STREAM_FLAG_MEMORY
  public :: FT_STREAM_FLAG_EXTERNAL
  public :: FT_STREAM_MODE_READ
  public :: FT_STREAM_MODE_WRITE
  
  ! Stream flags
  integer, parameter :: FT_STREAM_FLAG_NONE = 0
  integer, parameter :: FT_STREAM_FLAG_MEMORY = 1
  integer, parameter :: FT_STREAM_FLAG_EXTERNAL = 2
  
  ! Stream access modes
  integer, parameter :: FT_STREAM_MODE_READ = 1
  integer, parameter :: FT_STREAM_MODE_WRITE = 2
  
  ! C file functions
  interface
    function c_fopen(filename, mode) bind(C, name="fopen")
      import :: c_ptr, c_char
      character(kind=c_char), intent(in) :: filename(*)
      character(kind=c_char), intent(in) :: mode(*)
      type(c_ptr) :: c_fopen
    end function c_fopen
    
    function c_fclose(file) bind(C, name="fclose")
      import :: c_ptr, c_int
      type(c_ptr), value :: file
      integer(c_int) :: c_fclose
    end function c_fclose
    
    function c_fread(ptr, size, nmemb, stream) bind(C, name="fread")
      import :: c_ptr, c_size_t
      type(c_ptr), value :: ptr
      integer(c_size_t), value :: size
      integer(c_size_t), value :: nmemb
      type(c_ptr), value :: stream
      integer(c_size_t) :: c_fread
    end function c_fread
    
    function c_fseek(stream, offset, whence) bind(C, name="fseek")
      import :: c_ptr, c_long, c_int
      type(c_ptr), value :: stream
      integer(c_long), value :: offset
      integer(c_int), value :: whence
      integer(c_int) :: c_fseek
    end function c_fseek
    
    function c_ftell(stream) bind(C, name="ftell")
      import :: c_ptr, c_long
      type(c_ptr), value :: stream
      integer(c_long) :: c_ftell
    end function c_ftell
  end interface
  
  ! Seek constants from C
  integer(c_int), parameter :: SEEK_SET = 0
  integer(c_int), parameter :: SEEK_CUR = 1
  integer(c_int), parameter :: SEEK_END = 2
  
  ! Stream record type
  type :: FT_Stream_Rec
    type(c_ptr) :: handle = c_null_ptr      ! File handle or memory pointer
    integer(c_size_t) :: size = 0           ! Stream size in bytes
    integer(c_size_t) :: pos = 0            ! Current position
    integer :: flags = FT_STREAM_FLAG_NONE   ! Stream flags
    logical :: is_open = .false.            ! Stream state
  end type FT_Stream_Rec
  
  ! Stream handle type
  type :: FT_Stream_Type
    type(FT_Stream_Rec), pointer :: rec => null()
  end type FT_Stream_Type

contains

  ! Open a file stream
  function ft_stream_open(stream, filename, error) result(success)
    type(FT_Stream_Type), intent(out) :: stream
    character(len=*), intent(in) :: filename
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    character(kind=c_char) :: c_filename(len(trim(filename))+1)
    character(kind=c_char) :: c_mode(3)
    integer :: i
    type(c_ptr) :: file_ptr
    
    success = .false.
    error = FT_Err_Ok
    
    ! Allocate stream record
    allocate(stream%rec)
    
    ! Convert filename to C string
    do i = 1, len(trim(filename))
      c_filename(i) = filename(i:i)
    end do
    c_filename(len(trim(filename))+1) = c_null_char
    
    ! Open in binary read mode
    c_mode(1) = 'r'
    c_mode(2) = 'b'
    c_mode(3) = c_null_char
    
    ! Open file
    file_ptr = c_fopen(c_filename, c_mode)
    
    if (.not. c_associated(file_ptr)) then
      error = FT_Err_Cannot_Open_Stream
      deallocate(stream%rec)
      stream%rec => null()
      return
    end if
    
    ! Initialize stream record
    stream%rec%handle = file_ptr
    stream%rec%flags = FT_STREAM_FLAG_NONE
    stream%rec%is_open = .true.
    stream%rec%pos = 0
    
    ! Get file size
    if (c_fseek(file_ptr, 0_c_long, SEEK_END) == 0) then
      stream%rec%size = int(c_ftell(file_ptr), c_size_t)
      if (c_fseek(file_ptr, 0_c_long, SEEK_SET) /= 0) then
        error = FT_Err_Cannot_Open_Stream
        call ft_stream_close(stream)
        return
      end if
    else
      error = FT_Err_Cannot_Open_Stream
      call ft_stream_close(stream)
      return
    end if
    
    success = .true.
  end function ft_stream_open
  
  ! Close a stream
  subroutine ft_stream_close(stream)
    type(FT_Stream_Type), intent(inout) :: stream
    integer(c_int) :: i
    
    if (.not. associated(stream%rec)) return
    
    if (stream%rec%is_open .and. c_associated(stream%rec%handle)) then
      if (stream%rec%flags == FT_STREAM_FLAG_NONE) then
        ! Close file handle
        i = c_fclose(stream%rec%handle)
      end if
    end if
    
    deallocate(stream%rec)
    stream%rec => null()
  end subroutine ft_stream_close
  
  ! Read data from stream
  function ft_stream_read(stream, buffer, count, error) result(bytes_read)
    type(FT_Stream_Type), intent(inout) :: stream
    type(c_ptr), intent(in) :: buffer
    integer(c_size_t), intent(in) :: count
    integer(FT_Error), intent(out) :: error
    integer(c_size_t) :: bytes_read
    
    bytes_read = 0
    error = FT_Err_Ok
    
    if (.not. associated(stream%rec) .or. .not. stream%rec%is_open) then
      error = FT_Err_Invalid_Stream_Handle
      return
    end if
    
    if (.not. c_associated(buffer)) then
      error = FT_Err_Invalid_Argument
      return
    end if
    
    ! Read from file
    bytes_read = c_fread(buffer, 1_c_size_t, count, stream%rec%handle)
    
    if (bytes_read < count) then
      if (bytes_read == 0) then
        error = FT_Err_Invalid_Stream_Read
      end if
    end if
    
    ! Update position
    stream%rec%pos = stream%rec%pos + bytes_read
    
  end function ft_stream_read
  
  ! Read a single byte
  function ft_stream_read_byte(stream, value, error) result(success)
    type(FT_Stream_Type), intent(inout) :: stream
    integer(int8), intent(out) :: value
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer(c_size_t) :: bytes_read
    type(c_ptr) :: buffer_ptr
    integer(int8), target :: buffer
    
    value = 0
    buffer_ptr = c_loc(buffer)
    
    bytes_read = ft_stream_read(stream, buffer_ptr, 1_c_size_t, error)
    
    if (bytes_read == 1 .and. error == FT_Err_Ok) then
      value = buffer
      success = .true.
    else
      success = .false.
    end if
    
  end function ft_stream_read_byte
  
  ! Read a 16-bit value (big-endian)
  function ft_stream_read_short(stream, value, error) result(success)
    type(FT_Stream_Type), intent(inout) :: stream
    integer(int16), intent(out) :: value
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer(int8) :: bytes(2)
    integer :: i
    
    value = 0
    success = .true.
    
    ! Read two bytes
    do i = 1, 2
      if (.not. ft_stream_read_byte(stream, bytes(i), error)) then
        success = .false.
        return
      end if
    end do
    
    ! Convert from big-endian (mask bytes to prevent sign extension)
    value = ior(ishft(iand(int(bytes(1), int16), int(255, int16)), 8), &
                iand(int(bytes(2), int16), int(255, int16)))
    
  end function ft_stream_read_short
  
  ! Read a 16-bit unsigned value (big-endian)
  function ft_stream_read_ushort(stream, value, error) result(success)
    type(FT_Stream_Type), intent(inout) :: stream
    integer(int16), intent(out) :: value
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer(int8) :: bytes(2)
    integer :: i
    
    value = 0
    success = .true.
    
    ! Read two bytes
    do i = 1, 2
      if (.not. ft_stream_read_byte(stream, bytes(i), error)) then
        success = .false.
        return
      end if
    end do
    
    ! Convert from big-endian (mask bytes to prevent sign extension)
    value = ior(ishft(iand(int(bytes(1), int32), 255), 8), &
                iand(int(bytes(2), int32), 255))
    value = int(value, int16)
    
  end function ft_stream_read_ushort
  
  ! Read a 32-bit value (big-endian)
  function ft_stream_read_long(stream, value, error) result(success)
    type(FT_Stream_Type), intent(inout) :: stream
    integer(int32), intent(out) :: value
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer(int8) :: bytes(4)
    integer :: i
    
    value = 0
    success = .true.
    
    ! Read four bytes
    do i = 1, 4
      if (.not. ft_stream_read_byte(stream, bytes(i), error)) then
        success = .false.
        return
      end if
    end do
    
    ! Convert from big-endian (mask bytes to prevent sign extension)
    value = ior(ior(ior(ishft(iand(int(bytes(1), int32), 255), 24), &
                        ishft(iand(int(bytes(2), int32), 255), 16)), &
                        ishft(iand(int(bytes(3), int32), 255), 8)), &
                        iand(int(bytes(4), int32), 255))
    
  end function ft_stream_read_long
  
  ! Seek to position in stream
  function ft_stream_seek(stream, offset, error) result(success)
    type(FT_Stream_Type), intent(inout) :: stream
    integer(c_size_t), intent(in) :: offset
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    error = FT_Err_Ok
    success = .false.
    
    if (.not. associated(stream%rec) .or. .not. stream%rec%is_open) then
      error = FT_Err_Invalid_Stream_Handle
      return
    end if
    
    if (offset > stream%rec%size) then
      error = FT_Err_Invalid_Stream_Seek
      return
    end if
    
    if (c_fseek(stream%rec%handle, int(offset, c_long), SEEK_SET) == 0) then
      stream%rec%pos = offset
      success = .true.
    else
      error = FT_Err_Invalid_Stream_Seek
    end if
    
  end function ft_stream_seek
  
  ! Get current position in stream
  function ft_stream_tell(stream) result(pos)
    type(FT_Stream_Type), intent(in) :: stream
    integer(c_size_t) :: pos
    
    if (associated(stream%rec) .and. stream%rec%is_open) then
      pos = stream%rec%pos
    else
      pos = 0
    end if
    
  end function ft_stream_tell
  
  ! Get stream size
  function ft_stream_size(stream) result(size)
    type(FT_Stream_Type), intent(in) :: stream
    integer(c_size_t) :: size
    
    if (associated(stream%rec)) then
      size = stream%rec%size
    else
      size = 0
    end if
    
  end function ft_stream_size

  ! Open a memory stream
  function ft_stream_open_memory(stream, buffer, size, error) result(success)
    type(FT_Stream_Type), intent(out) :: stream
    character(len=*), intent(in) :: buffer
    integer, intent(in) :: size
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    success = .false.
    error = FT_Err_Ok
    
    ! Allocate stream record
    allocate(stream%rec)
    
    ! Initialize stream record for memory
    stream%rec%handle = c_null_ptr  ! No file handle for memory stream
    stream%rec%size = int(size, c_size_t)
    stream%rec%pos = 0
    stream%rec%flags = FT_STREAM_FLAG_MEMORY
    stream%rec%is_open = .true.
    
    ! For memory streams, we'll need to store the buffer reference
    ! This is a simplified implementation - in practice, we'd need
    ! to properly manage the memory buffer
    
    success = .true.
    
  end function ft_stream_open_memory

end module ft_stream