module tt_types
  use ft_types
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int16, int32, int64
  implicit none
  private
  
  ! Public types
  public :: TT_Header
  public :: TT_Table_Directory
  public :: TT_Table_Record
  
  ! TrueType magic numbers
  public :: TTAG_true
  public :: TTAG_typ1
  public :: TTAG_OTTO
  
  ! Type parameters
  public :: FT_UShort
  public :: FT_Short
  public :: FT_ULong
  public :: FT_Long
  
  ! Table tags
  public :: TTAG_head
  public :: TTAG_cmap
  public :: TTAG_glyf
  public :: TTAG_loca
  public :: TTAG_maxp
  public :: TTAG_name
  public :: TTAG_hmtx
  public :: TTAG_hhea
  public :: TTAG_post
  public :: TTAG_OS2
  
  ! Composite glyph flags
  public :: ARGS_ARE_WORDS
  public :: ARGS_ARE_XY_VALUES
  public :: ROUND_XY_TO_GRID
  public :: WE_HAVE_A_SCALE
  public :: MORE_COMPONENTS
  public :: WE_HAVE_AN_XY_SCALE
  public :: WE_HAVE_A_2X2
  public :: WE_HAVE_INSTR
  public :: USE_MY_METRICS
  public :: OVERLAP_COMPOUND
  public :: SCALED_COMPONENT_OFFSET
  public :: UNSCALED_COMPONENT_OFFSET
  
  ! Composite glyph types
  public :: TT_Component
  public :: TT_Composite_Glyph
  
  ! Type definitions for TrueType
  integer, parameter :: FT_UShort = c_short
  integer, parameter :: FT_Short = c_short  
  integer, parameter :: FT_ULong = c_int32_t
  integer, parameter :: FT_Long = c_int32_t
  
  ! TrueType tags (4-byte identifiers)
  integer(FT_ULong), parameter :: TTAG_true = int(z'74727565', FT_ULong)  ! 'true'
  integer(FT_ULong), parameter :: TTAG_typ1 = int(z'74797031', FT_ULong)  ! 'typ1'
  integer(FT_ULong), parameter :: TTAG_OTTO = int(z'4F54544F', FT_ULong)  ! 'OTTO'
  
  ! Table tags
  integer(FT_ULong), parameter :: TTAG_head = int(z'68656164', FT_ULong)  ! 'head'
  integer(FT_ULong), parameter :: TTAG_cmap = int(z'636D6170', FT_ULong)  ! 'cmap'
  integer(FT_ULong), parameter :: TTAG_glyf = int(z'676C7966', FT_ULong)  ! 'glyf'
  integer(FT_ULong), parameter :: TTAG_loca = int(z'6C6F6361', FT_ULong)  ! 'loca'
  integer(FT_ULong), parameter :: TTAG_maxp = int(z'6D617870', FT_ULong)  ! 'maxp'
  integer(FT_ULong), parameter :: TTAG_name = int(z'6E616D65', FT_ULong)  ! 'name'
  integer(FT_ULong), parameter :: TTAG_hmtx = int(z'686D7478', FT_ULong)  ! 'hmtx'
  integer(FT_ULong), parameter :: TTAG_hhea = int(z'68686561', FT_ULong)  ! 'hhea'
  integer(FT_ULong), parameter :: TTAG_post = int(z'706F7374', FT_ULong)  ! 'post'
  integer(FT_ULong), parameter :: TTAG_OS2  = int(z'4F532F32', FT_ULong)  ! 'OS/2'
  
  ! Composite glyph flags
  integer(FT_UShort), parameter :: ARGS_ARE_WORDS = int(z'0001', FT_UShort)
  integer(FT_UShort), parameter :: ARGS_ARE_XY_VALUES = int(z'0002', FT_UShort)
  integer(FT_UShort), parameter :: ROUND_XY_TO_GRID = int(z'0004', FT_UShort)
  integer(FT_UShort), parameter :: WE_HAVE_A_SCALE = int(z'0008', FT_UShort)
  integer(FT_UShort), parameter :: MORE_COMPONENTS = int(z'0020', FT_UShort)
  integer(FT_UShort), parameter :: WE_HAVE_AN_XY_SCALE = int(z'0040', FT_UShort)
  integer(FT_UShort), parameter :: WE_HAVE_A_2X2 = int(z'0080', FT_UShort)
  integer(FT_UShort), parameter :: WE_HAVE_INSTR = int(z'0100', FT_UShort)
  integer(FT_UShort), parameter :: USE_MY_METRICS = int(z'0200', FT_UShort)
  integer(FT_UShort), parameter :: OVERLAP_COMPOUND = int(z'0400', FT_UShort)
  integer(FT_UShort), parameter :: SCALED_COMPONENT_OFFSET = int(z'0800', FT_UShort)
  integer(FT_UShort), parameter :: UNSCALED_COMPONENT_OFFSET = int(z'1000', FT_UShort)
  
  ! TrueType Font Header
  type, bind(C) :: TT_Header
    integer(FT_Fixed) :: table_version    ! Should be 0x00010000 for version 1.0
    integer(FT_UShort) :: num_tables      ! Number of tables
    integer(FT_UShort) :: search_range    ! (Maximum power of 2 <= numTables) x 16
    integer(FT_UShort) :: entry_selector  ! Log2(maximum power of 2 <= numTables)
    integer(FT_UShort) :: range_shift     ! NumTables x 16 - searchRange
  end type TT_Header
  
  ! Table directory entry
  type, bind(C) :: TT_Table_Record
    integer(FT_ULong) :: tag              ! 4-byte identifier
    integer(FT_ULong) :: checksum         ! Checksum for this table
    integer(FT_ULong) :: offset           ! Offset from beginning of file
    integer(FT_ULong) :: length           ! Length of this table
  end type TT_Table_Record
  
  ! Table directory
  type :: TT_Table_Directory
    type(TT_Header) :: header
    type(TT_Table_Record), allocatable :: tables(:)
  end type TT_Table_Directory
  
  ! Composite glyph component
  type :: TT_Component
    integer(FT_UShort) :: flags
    integer(FT_UShort) :: glyph_index
    integer(FT_Short) :: arg1, arg2
    real :: xx, xy, yx, yy  ! transformation matrix (2x2)
  end type TT_Component
  
  ! Composite glyph
  type :: TT_Composite_Glyph
    integer :: num_components
    type(TT_Component), allocatable :: components(:)
  end type TT_Composite_Glyph
  

contains

  ! No methods needed for now - these are just type definitions

end module tt_types