module tt_maxp
  use ft_types
  use ft_stream
  use tt_types, only: FT_UShort, FT_Short
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int16, int32
  implicit none
  private
  
  ! Public types
  public :: TT_MaxProfile
  
  ! Public functions
  public :: tt_load_maxp_table
  
  ! Maximum profile table structure
  type :: TT_MaxProfile
    integer(FT_Fixed) :: version         ! Table version
    integer(FT_UShort) :: num_glyphs     ! Number of glyphs in font
    
    ! Version 1.0 fields (for TrueType outlines)
    integer(FT_UShort) :: max_points     ! Maximum points in non-composite glyph
    integer(FT_UShort) :: max_contours   ! Maximum contours in non-composite glyph
    integer(FT_UShort) :: max_composite_points    ! Maximum points in composite glyph
    integer(FT_UShort) :: max_composite_contours  ! Maximum contours in composite glyph
    integer(FT_UShort) :: max_zones      ! 1 if not using twilight zone, 2 otherwise
    integer(FT_UShort) :: max_twilight_points     ! Maximum points in Z0
    integer(FT_UShort) :: max_storage    ! Number of Storage Area locations
    integer(FT_UShort) :: max_function_defs       ! Number of FDEFs
    integer(FT_UShort) :: max_instruction_defs    ! Number of IDEFs
    integer(FT_UShort) :: max_stack_elements      ! Maximum stack depth
    integer(FT_UShort) :: max_size_of_instructions ! Maximum byte count for glyph instructions
    integer(FT_UShort) :: max_component_elements  ! Maximum number of components at top level
    integer(FT_UShort) :: max_component_depth     ! Maximum levels of recursion
  end type TT_MaxProfile

contains

  ! Load maxp table from stream
  function tt_load_maxp_table(stream, offset, maxp, error) result(success)
    type(FT_Stream_Type), intent(inout) :: stream
    integer(c_size_t), intent(in) :: offset
    type(TT_MaxProfile), intent(out) :: maxp
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer(int32) :: version
    integer(int16) :: temp16
    
    success = .false.
    error = FT_Err_Ok
    
    ! Initialize structure
    maxp%version = 0
    maxp%num_glyphs = 0
    
    ! Seek to maxp table offset
    if (.not. ft_stream_seek(stream, offset, error)) then
      error = FT_Err_Invalid_Stream_Seek
      return
    end if
    
    ! Read version
    if (.not. ft_stream_read_long(stream, version, error)) then
      error = FT_Err_Invalid_Table
      return
    end if
    maxp%version = version
    
    ! Read number of glyphs (present in all versions)
    if (.not. ft_stream_read_short(stream, temp16, error)) then
      error = FT_Err_Invalid_Table
      return
    end if
    maxp%num_glyphs = temp16
    
    ! Validate number of glyphs
    if (maxp%num_glyphs == 0) then
      error = FT_Err_Invalid_Table
      return
    end if
    
    ! Check version
    if (maxp%version == int(z'00005000', int32)) then
      ! Version 0.5 - CFF fonts, only has numGlyphs
      success = .true.
      return
    else if (maxp%version == int(z'00010000', int32)) then
      ! Version 1.0 - TrueType fonts, read additional fields
      
      ! max_points
      if (.not. ft_stream_read_short(stream, temp16, error)) return
      maxp%max_points = temp16
      
      ! max_contours
      if (.not. ft_stream_read_short(stream, temp16, error)) return
      maxp%max_contours = temp16
      
      ! max_composite_points
      if (.not. ft_stream_read_short(stream, temp16, error)) return
      maxp%max_composite_points = temp16
      
      ! max_composite_contours
      if (.not. ft_stream_read_short(stream, temp16, error)) return
      maxp%max_composite_contours = temp16
      
      ! max_zones
      if (.not. ft_stream_read_short(stream, temp16, error)) return
      maxp%max_zones = temp16
      
      ! max_twilight_points
      if (.not. ft_stream_read_short(stream, temp16, error)) return
      maxp%max_twilight_points = temp16
      
      ! max_storage
      if (.not. ft_stream_read_short(stream, temp16, error)) return
      maxp%max_storage = temp16
      
      ! max_function_defs
      if (.not. ft_stream_read_short(stream, temp16, error)) return
      maxp%max_function_defs = temp16
      
      ! max_instruction_defs
      if (.not. ft_stream_read_short(stream, temp16, error)) return
      maxp%max_instruction_defs = temp16
      
      ! max_stack_elements
      if (.not. ft_stream_read_short(stream, temp16, error)) return
      maxp%max_stack_elements = temp16
      
      ! max_size_of_instructions
      if (.not. ft_stream_read_short(stream, temp16, error)) return
      maxp%max_size_of_instructions = temp16
      
      ! max_component_elements
      if (.not. ft_stream_read_short(stream, temp16, error)) return
      maxp%max_component_elements = temp16
      
      ! max_component_depth
      if (.not. ft_stream_read_short(stream, temp16, error)) return
      maxp%max_component_depth = temp16
      
      success = .true.
    else
      ! Unknown version
      error = FT_Err_Invalid_Table_Format
      return
    end if
    
  end function tt_load_maxp_table

end module tt_maxp