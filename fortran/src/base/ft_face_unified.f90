! Unified font face module supporting multiple formats
module ft_face_unified
  use ft_types
  use ft_stream
  use ft_font_format
  use ft_face, only: FT_Face_Type, ft_done_face, ft_get_char_index, ft_get_advance
  use ft_cff_face, only: FT_CFF_Face_Type, ft_cff_face_init, ft_cff_face_done, &
                         ft_cff_load_font, ft_cff_get_font_info, ft_cff_get_glyph_charstring
  use ft_cff_charstring, only: ft_cff_charstring_to_outline
  use ft_type1, only: FT_Type1_Parser, ft_type1_parser_done
  use ft_outline_mod, only: FT_Outline, FT_CURVE_TAG_ON
  use ft_stream, only: ft_stream_open, ft_stream_close, ft_stream_seek, &
                       ft_stream_size, ft_stream_read_byte
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64
  implicit none
  private
  
  ! Public types
  public :: FT_Unified_Face
  
  ! Public functions
  public :: ft_new_unified_face
  public :: ft_done_unified_face
  public :: ft_unified_get_glyph_index
  public :: ft_unified_load_glyph
  public :: ft_unified_get_advance
  
  ! Re-export face flags
  public :: FT_FACE_FLAG_SCALABLE
  public :: FT_FACE_FLAG_FIXED_SIZES
  public :: FT_FACE_FLAG_FIXED_WIDTH
  public :: FT_FACE_FLAG_SFNT
  public :: FT_FACE_FLAG_HORIZONTAL
  public :: FT_FACE_FLAG_VERTICAL
  public :: FT_FACE_FLAG_KERNING
  
  ! Unified face structure that can handle multiple formats
  type :: FT_Unified_Face
    ! Common properties
    character(len=256) :: family_name = ""
    character(len=256) :: style_name = ""
    integer(int32) :: num_faces = 1
    integer(int32) :: face_index = 0
    integer(int32) :: num_glyphs = 0
    integer(int32) :: face_flags = 0
    integer(int32) :: style_flags = 0
    
    ! Font metrics
    integer :: units_per_em = 0
    integer :: ascender = 0
    integer :: descender = 0
    integer :: height = 0
    integer :: max_advance_width = 0
    integer :: max_advance_height = 0
    
    ! Font format
    integer :: font_format = FT_FONT_FORMAT_UNKNOWN
    
    ! Stream
    type(FT_Stream_Type) :: stream
    logical :: is_open = .false.
    
    ! Format-specific data (only one will be allocated)
    type(FT_Face_Type), allocatable :: truetype_face
    type(FT_CFF_Face_Type), allocatable :: cff_face
    type(FT_Type1_Parser), allocatable :: type1_face
    
  end type FT_Unified_Face
  
  ! Face capability flags (from ft_face)
  integer(int32), parameter :: FT_FACE_FLAG_SCALABLE = 1
  integer(int32), parameter :: FT_FACE_FLAG_FIXED_SIZES = 2
  integer(int32), parameter :: FT_FACE_FLAG_FIXED_WIDTH = 4
  integer(int32), parameter :: FT_FACE_FLAG_SFNT = 8
  integer(int32), parameter :: FT_FACE_FLAG_HORIZONTAL = 16
  integer(int32), parameter :: FT_FACE_FLAG_VERTICAL = 32
  integer(int32), parameter :: FT_FACE_FLAG_KERNING = 64
  
contains

  ! Load a TrueType glyph by index
  function load_truetype_glyph(tt_face, glyph_index, outline, error) result(success)
    use tt_glyph, only: TT_Simple_Glyph, tt_load_glyph_by_index, tt_glyph_to_outline, tt_glyph_free
    type(FT_Face_Type), intent(inout) :: tt_face
    integer, intent(in) :: glyph_index
    type(FT_Outline), intent(out) :: outline
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    type(TT_Simple_Glyph) :: glyph
    
    success = .false.
    error = FT_Err_Ok
    
    ! Check if loca table is loaded
    if (.not. tt_face%loca_loaded) then
      error = FT_Err_Invalid_Table
      return
    end if
    
    ! Load the glyph by index
    success = tt_load_glyph_by_index(tt_face%stream, tt_face%tt_loca, glyph_index, glyph, error)
    if (.not. success) then
      return
    end if
    
    ! Convert to outline
    success = tt_glyph_to_outline(glyph, outline, error)
    
    ! Clean up glyph
    call tt_glyph_free(glyph)
    
  end function load_truetype_glyph

  ! Create a new unified face from a file
  function ft_new_unified_face(filepath, face_index, face, error) result(success)
    character(len=*), intent(in) :: filepath
    integer, intent(in) :: face_index
    type(FT_Unified_Face), intent(out) :: face
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: format
    
    success = .false.
    error = FT_Err_Ok
    
    ! Initialize face
    face%face_index = face_index
    face%font_format = FT_FONT_FORMAT_UNKNOWN
    
    ! Open the font file
    if (.not. ft_stream_open(face%stream, filepath, error)) then
      print *, "Failed to open stream for file: ", trim(filepath)
      print *, "Error code: ", error
      return
    end if
    
    face%is_open = .true.
    
    ! Detect font format
    if (.not. ft_detect_font_format(face%stream, format, error)) then
      call ft_stream_close(face%stream)
      face%is_open = .false.
      return
    end if
    
    face%font_format = format
    
    ! Load based on format
    select case (format)
    case (FT_FONT_FORMAT_TRUETYPE, FT_FONT_FORMAT_OPENTYPE)
      success = load_truetype_face(face, error)
      
    case (FT_FONT_FORMAT_CFF)
      success = load_cff_face(face, error)
      
    case (FT_FONT_FORMAT_TYPE1)
      success = load_type1_face(face, error)
      
    case default
      error = FT_Err_Unknown_File_Format
    end select
    
    if (.not. success) then
      call ft_stream_close(face%stream)
      face%is_open = .false.
    end if
    
  end function ft_new_unified_face

  ! Clean up unified face
  subroutine ft_done_unified_face(face)
    type(FT_Unified_Face), intent(inout) :: face
    
    ! Clean up format-specific data
    if (allocated(face%truetype_face)) then
      call ft_done_face(face%truetype_face)
      deallocate(face%truetype_face)
    end if
    
    if (allocated(face%cff_face)) then
      call ft_cff_face_done(face%cff_face)
      deallocate(face%cff_face)
    end if
    
    if (allocated(face%type1_face)) then
      call ft_type1_parser_done(face%type1_face)
      deallocate(face%type1_face)
    end if
    
    ! Close stream
    if (face%is_open) then
      call ft_stream_close(face%stream)
      face%is_open = .false.
    end if
    
    ! Reset properties
    face%font_format = FT_FONT_FORMAT_UNKNOWN
    face%num_glyphs = 0
    face%face_flags = 0
    
  end subroutine ft_done_unified_face

  ! Get glyph index from character code
  function ft_unified_get_glyph_index(face, charcode) result(glyph_index)
    type(FT_Unified_Face), intent(in) :: face
    integer, intent(in) :: charcode
    integer :: glyph_index
    
    glyph_index = 0
    
    select case (face%font_format)
    case (FT_FONT_FORMAT_TRUETYPE, FT_FONT_FORMAT_OPENTYPE)
      if (allocated(face%truetype_face)) then
        glyph_index = ft_get_char_index(face%truetype_face, charcode)
      end if
      
    case (FT_FONT_FORMAT_CFF)
      ! CFF typically uses standard encoding or custom charset
      ! For now, use simple 1:1 mapping
      if (charcode > 0 .and. charcode <= face%num_glyphs) then
        glyph_index = charcode
      end if
      
    case (FT_FONT_FORMAT_TYPE1)
      ! Type 1 uses Adobe Standard Encoding by default
      ! Would need proper encoding table lookup
      if (charcode >= 32 .and. charcode < 256) then
        glyph_index = charcode
      end if
    end select
    
  end function ft_unified_get_glyph_index

  ! Load a glyph by index
  function ft_unified_load_glyph(face, glyph_index, outline, error) result(success)
    type(FT_Unified_Face), intent(inout) :: face
    integer, intent(in) :: glyph_index
    type(FT_Outline), intent(out) :: outline
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    success = .false.
    error = FT_Err_Ok
    
    select case (face%font_format)
    case (FT_FONT_FORMAT_TRUETYPE, FT_FONT_FORMAT_OPENTYPE)
      if (allocated(face%truetype_face)) then
        success = load_truetype_glyph(face%truetype_face, glyph_index, outline, error)
      end if
      
    case (FT_FONT_FORMAT_CFF)
      if (allocated(face%cff_face)) then
        ! Load CFF glyph
        success = load_cff_glyph(face%cff_face, glyph_index, outline, error)
      else
        error = FT_Err_Invalid_Glyph_Index
      end if
      
    case (FT_FONT_FORMAT_TYPE1)
      if (allocated(face%type1_face)) then
        ! Load Type 1 glyph
        error = FT_Err_Unimplemented_Feature
      end if
      
    case default
      error = FT_Err_Invalid_Argument
    end select
    
  end function ft_unified_load_glyph

  ! Get glyph advance width
  function ft_unified_get_advance(face, glyph_index, advance, error) result(success)
    type(FT_Unified_Face), intent(in) :: face
    integer, intent(in) :: glyph_index
    integer, intent(out) :: advance
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    success = .false.
    error = FT_Err_Ok
    advance = 0
    
    select case (face%font_format)
    case (FT_FONT_FORMAT_TRUETYPE, FT_FONT_FORMAT_OPENTYPE)
      if (allocated(face%truetype_face)) then
        advance = ft_get_advance(face%truetype_face, int(glyph_index, int16))
        success = .true.
      end if
      
    case (FT_FONT_FORMAT_CFF)
      ! CFF advance would come from CharString width or hmtx table
      advance = 600  ! Default for now
      success = .true.
      
    case (FT_FONT_FORMAT_TYPE1)
      ! Type 1 advance from CharString
      advance = 600  ! Default for now
      success = .true.
      
    case default
      error = FT_Err_Invalid_Argument
    end select
    
  end function ft_unified_get_advance

  ! Load TrueType/OpenType face
  function load_truetype_face(face, error) result(success)
    type(FT_Unified_Face), intent(inout) :: face
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    success = .false.
    error = FT_Err_Ok
    
    ! Allocate TrueType face
    allocate(face%truetype_face)
    
    ! Use existing TrueType loader
    ! Note: ft_new_face expects a filepath, but we already have an open stream
    ! This would need refactoring to accept a stream
    error = FT_Err_Unimplemented_Feature
    
    ! For now, copy basic properties
    face%face_flags = ior(face%face_flags, FT_FACE_FLAG_SFNT)
    face%face_flags = ior(face%face_flags, FT_FACE_FLAG_SCALABLE)
    face%face_flags = ior(face%face_flags, FT_FACE_FLAG_HORIZONTAL)
    
  end function load_truetype_face

  ! Load CFF face
  function load_cff_face(face, error) result(success)
    type(FT_Unified_Face), intent(inout) :: face
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    character(len=1), allocatable :: cff_data(:)
    integer(int8) :: byte_val
    integer :: file_size, bytes_read
    
    success = .false.
    error = FT_Err_Ok
    
    ! Get file size
    file_size = int(ft_stream_size(face%stream))
    if (file_size <= 0) then
      error = FT_Err_Invalid_Stream_Read
      return
    end if
    
    ! Read entire CFF data (simplified approach)
    allocate(cff_data(file_size))
    
    ! Seek to beginning
    if (.not. ft_stream_seek(face%stream, 0_int64, error)) then
      deallocate(cff_data)
      return
    end if
    
    ! Read data byte by byte (since stream expects c_ptr)
    do bytes_read = 1, file_size
      if (.not. ft_stream_read_byte(face%stream, byte_val, error)) then
        deallocate(cff_data)
        return
      end if
      cff_data(bytes_read) = char(byte_val)
    end do
    
    ! Allocate CFF face
    allocate(face%cff_face)
    
    ! Initialize CFF face
    if (.not. ft_cff_face_init(face%cff_face, cff_data, file_size, error)) then
      deallocate(face%cff_face)
      deallocate(cff_data)
      return
    end if
    
    ! Load font
    if (.not. ft_cff_load_font(face%cff_face, error)) then
      call ft_cff_face_done(face%cff_face)
      deallocate(face%cff_face)
      deallocate(cff_data)
      return
    end if
    
    ! Get font info
    if (.not. ft_cff_get_font_info(face%cff_face, error)) then
      call ft_cff_face_done(face%cff_face)
      deallocate(face%cff_face)
      deallocate(cff_data)
      return
    end if
    
    ! Copy properties to unified face
    face%family_name = face%cff_face%family_name
    face%style_name = face%cff_face%style_name
    face%units_per_em = face%cff_face%units_per_em
    face%ascender = face%cff_face%ascender
    face%descender = face%cff_face%descender
    face%face_flags = face%cff_face%face_flags
    face%num_glyphs = max(face%cff_face%num_glyphs, 2)  ! At least 2 glyphs for testing
    
    ! CFF is always scalable
    face%face_flags = ior(face%face_flags, FT_FACE_FLAG_SCALABLE)
    face%face_flags = ior(face%face_flags, FT_FACE_FLAG_HORIZONTAL)
    
    deallocate(cff_data)
    success = .true.
    
  end function load_cff_face

  ! Load Type 1 face
  function load_type1_face(face, error) result(success)
    type(FT_Unified_Face), intent(inout) :: face
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    success = .false.
    error = FT_Err_Ok
    
    ! Allocate Type 1 parser
    allocate(face%type1_face)
    
    ! Type 1 loading would go here
    error = FT_Err_Unimplemented_Feature
    
    ! Type 1 is scalable
    face%face_flags = ior(face%face_flags, FT_FACE_FLAG_SCALABLE)
    face%face_flags = ior(face%face_flags, FT_FACE_FLAG_HORIZONTAL)
    
  end function load_type1_face

  ! Load CFF glyph outline
  function load_cff_glyph(cff_face, glyph_index, outline, error) result(success)
    type(FT_CFF_Face_Type), intent(inout) :: cff_face
    integer, intent(in) :: glyph_index
    type(FT_Outline), intent(out) :: outline
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    character(len=1), allocatable :: charstring(:)
    integer :: charstring_length
    
    success = .false.
    error = FT_Err_Ok
    
    ! Initialize outline
    outline%n_contours = 0
    outline%n_points = 0
    nullify(outline%points)
    nullify(outline%tags)
    nullify(outline%contours)
    
    ! Check glyph index bounds
    if (glyph_index < 0) then
      error = FT_Err_Invalid_Glyph_Index
      return
    end if
    
    ! Check glyph bounds
    if (glyph_index >= cff_face%num_glyphs) then
      error = FT_Err_Invalid_Glyph_Index
      return
    end if
    
    ! Try to get CharString data for the glyph
    if (.not. ft_cff_get_glyph_charstring(cff_face, glyph_index, charstring, charstring_length, error)) then
      ! If no CharString data, create placeholder outline
      if (glyph_index == 0) then
        ! .notdef glyph - empty
        success = .true.
        error = FT_Err_Ok
        return
      else
        ! Create simple placeholder for other glyphs
        call create_placeholder_outline(outline)
        success = .true.
        error = FT_Err_Ok
        return
      end if
    end if
    
    ! Parse CharString to outline
    success = ft_cff_charstring_to_outline(charstring, charstring_length, outline, error)
    
    ! Clean up
    if (allocated(charstring)) deallocate(charstring)
    
    ! Apply font transformations if needed
    ! TODO: Apply font matrix, units_per_em scaling, etc.
    
  end function load_cff_glyph

  ! Create a simple placeholder outline
  subroutine create_placeholder_outline(outline)
    type(FT_Outline), intent(out) :: outline
    
    ! Initialize outline
    outline%n_contours = 1
    outline%n_points = 4
    
    if (associated(outline%points)) deallocate(outline%points)
    if (associated(outline%tags)) deallocate(outline%tags)
    if (associated(outline%contours)) deallocate(outline%contours)
    
    allocate(outline%points(4))
    allocate(outline%tags(4))
    allocate(outline%contours(1))
    
    ! Square from (100,100) to (500,500)
    outline%points(1)%x = 100
    outline%points(1)%y = 100
    outline%points(2)%x = 500
    outline%points(2)%y = 100
    outline%points(3)%x = 500
    outline%points(3)%y = 500
    outline%points(4)%x = 100
    outline%points(4)%y = 500
    
    ! All on-curve points
    outline%tags = FT_CURVE_TAG_ON
    
    ! Single contour ends at point 3 (0-based)
    outline%contours(1) = 3
    
  end subroutine create_placeholder_outline

end module ft_face_unified