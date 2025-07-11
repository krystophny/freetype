module ft_face
  use ft_types
  use ft_stream
  use tt_types
  use tt_load, only: tt_load_table_directory
  use tt_head, only: TT_Header_Table, tt_load_head_table
  use tt_maxp, only: TT_MaxProfile, tt_load_maxp_table
  use tt_cmap, only: TT_CMap_Table, tt_load_cmap_table, tt_cmap_free, tt_cmap_char_to_glyph
  use tt_loca, only: TT_Loca_Table, tt_load_loca_table, tt_loca_free
  use tt_hmtx, only: TT_HMtx_Table, tt_load_hmtx_table, tt_hmtx_free, tt_hmtx_get_advance
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int32, int64
  implicit none
  private
  
  ! Public types
  public :: FT_Face_Type
  public :: FT_CharMap
  
  ! Public functions
  public :: ft_new_face
  public :: ft_done_face
  public :: ft_get_char_index
  public :: ft_load_glyph
  public :: ft_get_advance
  
  ! Public constants
  public :: FT_FACE_FLAG_SCALABLE
  public :: FT_FACE_FLAG_FIXED_SIZES
  public :: FT_FACE_FLAG_FIXED_WIDTH
  public :: FT_FACE_FLAG_SFNT
  public :: FT_FACE_FLAG_HORIZONTAL
  public :: FT_FACE_FLAG_VERTICAL
  public :: FT_FACE_FLAG_KERNING
  public :: FT_FACE_FLAG_MULTIPLE_MASTERS
  public :: FT_FACE_FLAG_GLYPH_NAMES
  public :: FT_FACE_FLAG_EXTERNAL_STREAM
  
  ! Character map structure
  type :: FT_CharMap
    integer :: platform_id     ! Platform identifier
    integer :: encoding_id     ! Platform-specific encoding
    integer :: format          ! Character map format
  end type FT_CharMap
  
  ! Font face structure - main interface to a font
  type :: FT_Face_Type
    ! Basic font information
    character(len=256) :: family_name = ""
    character(len=256) :: style_name = ""
    integer(FT_Long) :: num_faces = 1        ! Number of faces in file
    integer(FT_Long) :: face_index = 0       ! Current face index
    integer(FT_Long) :: num_glyphs = 0       ! Number of glyphs
    integer(FT_Long) :: num_charmaps = 0     ! Number of character maps
    
    ! Font metrics
    integer(FT_UShort) :: units_per_em = 0   ! Font units per EM
    integer(FT_Short) :: ascender = 0        ! Typographic ascender
    integer(FT_Short) :: descender = 0       ! Typographic descender
    integer(FT_Short) :: height = 0          ! Text height
    integer(FT_Short) :: max_advance_width = 0  ! Maximum advance width
    integer(FT_Short) :: max_advance_height = 0 ! Maximum advance height
    
    ! Font format and flags
    character(len=32) :: font_format = "TrueType"
    integer(FT_Long) :: face_flags = 0       ! Face capability flags
    integer(FT_Long) :: style_flags = 0      ! Style flags
    
    ! Internal data
    type(FT_Stream_Type) :: stream           ! File stream
    type(TT_Table_Directory) :: directory    ! TrueType table directory
    logical :: is_open = .false.             ! Whether face is loaded
    
    ! Individual table data
    type(TT_Header_Table) :: tt_head
    type(TT_MaxProfile) :: tt_maxp
    type(TT_CMap_Table) :: tt_cmap
    type(TT_Loca_Table) :: tt_loca
    type(TT_HMtx_Table) :: tt_hmtx
    
    ! Table load flags
    logical :: head_loaded = .false.
    logical :: maxp_loaded = .false.
    logical :: cmap_loaded = .false.
    logical :: loca_loaded = .false.
    logical :: hmtx_loaded = .false.
    
    ! Character maps
    type(FT_CharMap), allocatable :: charmaps(:)
    integer :: charmap_index = -1            ! Current charmap (-1 = none)
    
  end type FT_Face_Type
  
  ! Face capability flags
  integer(FT_Long), parameter :: FT_FACE_FLAG_SCALABLE = 1        ! Font is scalable
  integer(FT_Long), parameter :: FT_FACE_FLAG_FIXED_SIZES = 2     ! Has bitmap strikes
  integer(FT_Long), parameter :: FT_FACE_FLAG_FIXED_WIDTH = 4     ! Fixed-width font
  integer(FT_Long), parameter :: FT_FACE_FLAG_SFNT = 8            ! SFNT-based format
  integer(FT_Long), parameter :: FT_FACE_FLAG_HORIZONTAL = 16     ! Has horizontal metrics
  integer(FT_Long), parameter :: FT_FACE_FLAG_VERTICAL = 32       ! Has vertical metrics
  integer(FT_Long), parameter :: FT_FACE_FLAG_KERNING = 64        ! Has kerning info
  integer(FT_Long), parameter :: FT_FACE_FLAG_MULTIPLE_MASTERS = 128 ! Multiple master
  integer(FT_Long), parameter :: FT_FACE_FLAG_GLYPH_NAMES = 256   ! Has glyph names
  integer(FT_Long), parameter :: FT_FACE_FLAG_EXTERNAL_STREAM = 512 ! External stream

contains

  ! Create a new face from a file
  function ft_new_face(filepath, face_index, face, error) result(success)
    character(len=*), intent(in) :: filepath
    integer(FT_Long), intent(in) :: face_index
    type(FT_Face_Type), intent(out) :: face
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer(FT_Error) :: temp_error
    
    success = .false.
    error = FT_Err_Ok
    
    ! Initialize face
    face%face_index = face_index
    
    ! Open the font file
    if (.not. ft_stream_open(face%stream, filepath, temp_error)) then
      error = temp_error
      return
    end if
    
    face%is_open = .true.
    
    ! Load TrueType table directory
    if (.not. tt_load_table_directory(face%stream, face%directory, temp_error)) then
      call ft_stream_close(face%stream)
      face%is_open = .false.
      error = temp_error
      return
    end if
    
    ! Load essential tables
    call load_essential_tables(face, temp_error)
    if (temp_error /= FT_Err_Ok) then
      call ft_stream_close(face%stream)
      face%is_open = .false.
      error = temp_error
      return
    end if
    
    ! Extract basic face information
    call extract_face_info(face, temp_error)
    if (temp_error /= FT_Err_Ok) then
      call ft_done_face(face)
      error = temp_error
      return
    end if
    
    ! Set up character maps
    call setup_charmaps(face, temp_error)
    if (temp_error /= FT_Err_Ok) then
      call ft_done_face(face)
      error = temp_error
      return
    end if
    
    success = .true.
    
  end function ft_new_face
  
  ! Clean up face resources
  subroutine ft_done_face(face)
    type(FT_Face_Type), intent(inout) :: face
    
    ! Close stream if open
    if (face%is_open) then
      call ft_stream_close(face%stream)
      face%is_open = .false.
    end if
    
    ! Free TrueType table data
    if (face%cmap_loaded) call tt_cmap_free(face%tt_cmap)
    if (face%loca_loaded) call tt_loca_free(face%tt_loca)
    if (face%hmtx_loaded) call tt_hmtx_free(face%tt_hmtx)
    
    ! Reset flags
    face%head_loaded = .false.
    face%maxp_loaded = .false.
    face%cmap_loaded = .false.
    face%loca_loaded = .false.
    face%hmtx_loaded = .false.
    
    ! Free table directory
    if (allocated(face%directory%tables)) deallocate(face%directory%tables)
    
    ! Free charmaps
    if (allocated(face%charmaps)) deallocate(face%charmaps)
    face%charmap_index = -1
    
    ! Reset face data
    face%num_glyphs = 0
    face%num_charmaps = 0
    face%units_per_em = 0
    
    ! Reset face state
    face%family_name = ""
    face%style_name = ""
    
  end subroutine ft_done_face
  
  ! Get glyph index for character code
  function ft_get_char_index(face, charcode) result(glyph_index)
    type(FT_Face_Type), intent(in) :: face
    integer(FT_ULong), intent(in) :: charcode
    integer(FT_UShort) :: glyph_index
    
    glyph_index = 0
    
    ! Check if face has character maps
    if (face%charmap_index >= 0 .and. face%charmap_index < face%num_charmaps) then
      if (face%cmap_loaded) then
        glyph_index = tt_cmap_char_to_glyph(face%tt_cmap, charcode)
      end if
    end if
    
  end function ft_get_char_index
  
  ! Load glyph (placeholder - will be expanded in glyph rendering phase)
  function ft_load_glyph(face, glyph_index, flags, error) result(success)
    type(FT_Face_Type), intent(inout) :: face
    integer(FT_UShort), intent(in) :: glyph_index
    integer(FT_Long), intent(in) :: flags
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    ! Basic validation for now
    success = .false.
    error = FT_Err_Ok
    
    if (glyph_index >= face%num_glyphs) then
      error = FT_Err_Invalid_Glyph_Index
      return
    end if
    
    ! TODO: Implement actual glyph loading in Phase 3
    success = .true.
    
  end function ft_load_glyph
  
  ! Get advance width for a glyph
  function ft_get_advance(face, glyph_index) result(advance)
    type(FT_Face_Type), intent(in) :: face
    integer(FT_UShort), intent(in) :: glyph_index
    integer(FT_Fixed) :: advance
    
    advance = 0
    
    if (face%hmtx_loaded .and. glyph_index < face%num_glyphs) then
      ! Get advance width from hmtx table and convert to 16.16 fixed point
      advance = int(tt_hmtx_get_advance(face%tt_hmtx, int(glyph_index)), FT_Fixed) * 65536
    end if
    
  end function ft_get_advance
  
  ! Extract basic information from loaded font
  subroutine extract_face_info(face, error)
    type(FT_Face_Type), intent(inout) :: face
    integer(FT_Error), intent(out) :: error
    
    error = FT_Err_Ok
    
    ! Get number of glyphs from maxp table
    if (face%maxp_loaded) then
      face%num_glyphs = face%tt_maxp%num_glyphs
    else
      error = FT_Err_Invalid_Table
      return
    end if
    
    ! Get font metrics from head table
    if (face%head_loaded) then
      face%units_per_em = face%tt_head%units_per_em
      
      ! Set face flags
      face%face_flags = ior(face%face_flags, FT_FACE_FLAG_SCALABLE)
      face%face_flags = ior(face%face_flags, FT_FACE_FLAG_SFNT)
      face%face_flags = ior(face%face_flags, FT_FACE_FLAG_HORIZONTAL)
      
      ! Check if it's a fixed-width font
      if (face%hmtx_loaded) then
        ! Simple heuristic: if num_metrics = 1, it's likely monospaced
        if (face%tt_hmtx%num_metrics == 1) then
          face%face_flags = ior(face%face_flags, FT_FACE_FLAG_FIXED_WIDTH)
        end if
      end if
      
      ! Set basic names (placeholder)
      face%family_name = "TrueType Font"
      face%style_name = "Regular"
      face%font_format = "TrueType"
    else
      error = FT_Err_Invalid_Table
      return
    end if
    
  end subroutine extract_face_info
  
  ! Set up character maps
  subroutine setup_charmaps(face, error)
    type(FT_Face_Type), intent(inout) :: face
    integer(FT_Error), intent(out) :: error
    
    error = FT_Err_Ok
    
    ! For now, we only support one charmap if cmap table is loaded
    if (face%cmap_loaded .and. face%tt_cmap%has_format4) then
      face%num_charmaps = 1
      allocate(face%charmaps(1))
      
      ! Set up Unicode BMP charmap
      face%charmaps(1)%platform_id = 3  ! Microsoft
      face%charmaps(1)%encoding_id = 1  ! Unicode BMP
      face%charmaps(1)%format = 4       ! Format 4
      
      ! Set as current charmap
      face%charmap_index = 0
    else
      face%num_charmaps = 0
      face%charmap_index = -1
    end if
    
  end subroutine setup_charmaps
  
  ! Load essential tables for the face
  subroutine load_essential_tables(face, error)
    type(FT_Face_Type), intent(inout) :: face
    integer(FT_Error), intent(out) :: error
    
    integer :: i
    integer(c_size_t) :: table_offset
    logical :: found
    
    error = FT_Err_Ok
    
    ! Load head table
    call find_table(face%directory, TTAG_head, found, table_offset)
    if (found) then
      if (tt_load_head_table(face%stream, table_offset, face%tt_head, error)) then
        face%head_loaded = .true.
      else
        return
      end if
    end if
    
    ! Load maxp table
    call find_table(face%directory, TTAG_maxp, found, table_offset)
    if (found) then
      if (tt_load_maxp_table(face%stream, table_offset, face%tt_maxp, error)) then
        face%maxp_loaded = .true.
      else
        return
      end if
    end if
    
    ! Load cmap table
    call find_table(face%directory, TTAG_cmap, found, table_offset)
    if (found) then
      if (tt_load_cmap_table(face%stream, table_offset, face%tt_cmap, error)) then
        face%cmap_loaded = .true.
      end if
    end if
    
    ! Load loca table (needs maxp and head first)
    if (face%maxp_loaded .and. face%head_loaded) then
      call find_table(face%directory, TTAG_loca, found, table_offset)
      if (found) then
        ! Get table length
        do i = 1, size(face%directory%tables)
          if (face%directory%tables(i)%tag == TTAG_loca) then
            if (tt_load_loca_table(face%stream, table_offset, &
                                   int(face%directory%tables(i)%length, c_size_t), &
                                   int(face%tt_maxp%num_glyphs), &
                                   face%tt_head%index_to_loc_format == 1, &
                                   face%tt_loca, error)) then
              face%loca_loaded = .true.
            end if
            exit
          end if
        end do
      end if
    end if
    
    ! Load hmtx table (need to know numLongHorMetrics from hhea, for now assume all)
    if (face%maxp_loaded) then
      call find_table(face%directory, TTAG_hmtx, found, table_offset)
      if (found) then
        ! For now, assume all glyphs have metrics (will need hhea table later)
        if (tt_load_hmtx_table(face%stream, table_offset, int(face%tt_maxp%num_glyphs), &
                               int(face%tt_maxp%num_glyphs), face%tt_hmtx, error)) then
          face%hmtx_loaded = .true.
        end if
      end if
    end if
    
  end subroutine load_essential_tables
  
  ! Find a table in the directory
  subroutine find_table(directory, tag, found, offset)
    type(TT_Table_Directory), intent(in) :: directory
    integer(FT_ULong), intent(in) :: tag
    logical, intent(out) :: found
    integer(c_size_t), intent(out) :: offset
    
    integer :: i
    
    found = .false.
    offset = 0
    
    if (allocated(directory%tables)) then
      do i = 1, size(directory%tables)
        if (directory%tables(i)%tag == tag) then
          found = .true.
          offset = directory%tables(i)%offset
          exit
        end if
      end do
    end if
    
  end subroutine find_table

end module ft_face