module tt_load
  use ft_types
  use ft_stream
  use tt_types, only: TT_Header, TT_Table_Directory, TT_Table_Record, &
                      FT_UShort, FT_ULong, TTAG_true, TTAG_typ1, TTAG_OTTO
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int16, int32
  implicit none
  private
  
  ! Public functions
  public :: tt_load_font_header
  public :: tt_load_table_directory
  public :: tt_find_table
  
contains

  ! Load TrueType font header
  function tt_load_font_header(stream, header, error) result(success)
    type(FT_Stream_Type), intent(inout) :: stream
    type(TT_Header), intent(out) :: header
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer(int32) :: version
    integer(int16) :: temp16
    
    success = .false.
    error = FT_Err_Ok
    
    ! Seek to beginning
    if (.not. ft_stream_seek(stream, 0_c_size_t, error)) then
      error = FT_Err_Invalid_Stream_Seek
      return
    end if
    
    ! Read version (4 bytes)
    if (.not. ft_stream_read_long(stream, version, error)) then
      error = FT_Err_Invalid_File_Format
      return
    end if
    
    ! Check if it's a valid TrueType font
    if (version /= int(z'00010000', int32) .and. &  ! Version 1.0
        version /= TTAG_true .and. &
        version /= TTAG_typ1 .and. &
        version /= TTAG_OTTO) then
      error = FT_Err_Unknown_File_Format
      return
    end if
    
    header%table_version = version
    
    ! Read number of tables
    if (.not. ft_stream_read_short(stream, temp16, error)) then
      error = FT_Err_Invalid_File_Format
      return
    end if
    header%num_tables = temp16
    
    ! Read search range
    if (.not. ft_stream_read_short(stream, temp16, error)) then
      error = FT_Err_Invalid_File_Format
      return
    end if
    header%search_range = temp16
    
    ! Read entry selector
    if (.not. ft_stream_read_short(stream, temp16, error)) then
      error = FT_Err_Invalid_File_Format
      return
    end if
    header%entry_selector = temp16
    
    ! Read range shift
    if (.not. ft_stream_read_short(stream, temp16, error)) then
      error = FT_Err_Invalid_File_Format
      return
    end if
    header%range_shift = temp16
    
    success = .true.
  end function tt_load_font_header
  
  ! Load table directory
  function tt_load_table_directory(stream, directory, error) result(success)
    type(FT_Stream_Type), intent(inout) :: stream
    type(TT_Table_Directory), intent(out) :: directory
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: i
    integer(int32) :: temp32
    
    success = .false.
    
    ! First load the header
    if (.not. tt_load_font_header(stream, directory%header, error)) then
      return
    end if
    
    ! Allocate space for tables
    if (directory%header%num_tables > 0) then
      allocate(directory%tables(directory%header%num_tables))
    else
      error = FT_Err_Invalid_Table
      return
    end if
    
    ! Read each table record
    do i = 1, directory%header%num_tables
      ! Read tag
      if (.not. ft_stream_read_long(stream, temp32, error)) then
        deallocate(directory%tables)
        return
      end if
      directory%tables(i)%tag = temp32
      
      ! Read checksum
      if (.not. ft_stream_read_long(stream, temp32, error)) then
        deallocate(directory%tables)
        return
      end if
      directory%tables(i)%checksum = temp32
      
      ! Read offset
      if (.not. ft_stream_read_long(stream, temp32, error)) then
        deallocate(directory%tables)
        return
      end if
      directory%tables(i)%offset = temp32
      
      ! Read length
      if (.not. ft_stream_read_long(stream, temp32, error)) then
        deallocate(directory%tables)
        return
      end if
      directory%tables(i)%length = temp32
    end do
    
    success = .true.
  end function tt_load_table_directory
  
  ! Find a table by tag
  function tt_find_table(directory, tag) result(table_index)
    type(TT_Table_Directory), intent(in) :: directory
    integer(FT_ULong), intent(in) :: tag
    integer :: table_index
    
    integer :: i
    
    table_index = 0
    
    if (.not. allocated(directory%tables)) return
    
    do i = 1, size(directory%tables)
      if (directory%tables(i)%tag == tag) then
        table_index = i
        return
      end if
    end do
    
  end function tt_find_table

end module tt_load