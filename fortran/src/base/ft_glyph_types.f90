module ft_glyph_types
  use ft_types, only: FT_Fixed, ft_f26dot6_from_pixels, ft_f26dot6_to_int
  use ft_geometry, only: FT_Vector, FT_BBox
  use, intrinsic :: iso_c_binding
  implicit none
  private
  
  ! Import FT_Pos type (same as FT_Fixed)
  integer, parameter :: FT_Pos = FT_Fixed
  
  ! Public types
  public :: FT_Glyph_Metrics
  public :: FT_Size_Metrics
  public :: FT_Glyph_Format
  
  ! Glyph format constants
  public :: FT_GLYPH_FORMAT_NONE
  public :: FT_GLYPH_FORMAT_COMPOSITE
  public :: FT_GLYPH_FORMAT_BITMAP
  public :: FT_GLYPH_FORMAT_OUTLINE
  public :: FT_GLYPH_FORMAT_PLOTTER
  
  ! Utility functions
  public :: ft_glyph_metrics_init
  public :: ft_size_metrics_init
  public :: ft_glyph_metrics_scale
  
  ! Type definitions
  
  ! FT_UShort type
  integer, parameter :: FT_UShort = c_short
  
  ! Glyph format enumeration
  integer, parameter :: FT_Glyph_Format = c_int
  
  ! Glyph format constants (matching FreeType's FT_GLYPH_FORMAT_xxx)
  integer(FT_Glyph_Format), parameter :: FT_GLYPH_FORMAT_NONE      = 0
  integer(FT_Glyph_Format), parameter :: FT_GLYPH_FORMAT_COMPOSITE = 1668246896  ! 'comp'
  integer(FT_Glyph_Format), parameter :: FT_GLYPH_FORMAT_BITMAP    = 1651078259  ! 'bits'
  integer(FT_Glyph_Format), parameter :: FT_GLYPH_FORMAT_OUTLINE   = 1869968492  ! 'outl'
  integer(FT_Glyph_Format), parameter :: FT_GLYPH_FORMAT_PLOTTER   = 1886154612  ! 'plot'
  
  ! Glyph metrics - measurements for individual glyphs
  type, bind(C) :: FT_Glyph_Metrics
    integer(FT_Pos) :: width          ! Glyph width
    integer(FT_Pos) :: height         ! Glyph height
    
    integer(FT_Pos) :: horiBearingX   ! Left side bearing (horizontal)
    integer(FT_Pos) :: horiBearingY   ! Top side bearing (horizontal)
    integer(FT_Pos) :: horiAdvance    ! Advance width (horizontal)
    
    integer(FT_Pos) :: vertBearingX   ! Left side bearing (vertical)
    integer(FT_Pos) :: vertBearingY   ! Top side bearing (vertical)
    integer(FT_Pos) :: vertAdvance    ! Advance height (vertical)
  end type FT_Glyph_Metrics
  
  ! Size metrics - measurements for a specific size
  type, bind(C) :: FT_Size_Metrics
    integer(FT_UShort) :: x_ppem      ! Horizontal pixels per EM
    integer(FT_UShort) :: y_ppem      ! Vertical pixels per EM
    
    integer(FT_Fixed) :: x_scale      ! Horizontal scaling factor
    integer(FT_Fixed) :: y_scale      ! Vertical scaling factor
    
    integer(FT_Pos) :: ascender       ! Ascender in 26.6 pixels
    integer(FT_Pos) :: descender      ! Descender in 26.6 pixels (negative)
    integer(FT_Pos) :: height         ! Text height in 26.6 pixels
    integer(FT_Pos) :: max_advance    ! Maximum advance width
  end type FT_Size_Metrics

contains

  ! Initialize glyph metrics to zero
  pure subroutine ft_glyph_metrics_init(metrics)
    type(FT_Glyph_Metrics), intent(out) :: metrics
    
    metrics%width = 0
    metrics%height = 0
    metrics%horiBearingX = 0
    metrics%horiBearingY = 0
    metrics%horiAdvance = 0
    metrics%vertBearingX = 0
    metrics%vertBearingY = 0
    metrics%vertAdvance = 0
  end subroutine ft_glyph_metrics_init
  
  ! Initialize size metrics to zero
  pure subroutine ft_size_metrics_init(metrics)
    type(FT_Size_Metrics), intent(out) :: metrics
    
    metrics%x_ppem = 0
    metrics%y_ppem = 0
    metrics%x_scale = 0
    metrics%y_scale = 0
    metrics%ascender = 0
    metrics%descender = 0
    metrics%height = 0
    metrics%max_advance = 0
  end subroutine ft_size_metrics_init
  
  ! Scale glyph metrics by a given factor
  pure subroutine ft_glyph_metrics_scale(metrics, scale_x, scale_y)
    type(FT_Glyph_Metrics), intent(inout) :: metrics
    integer(FT_Fixed), intent(in) :: scale_x, scale_y
    integer(8) :: temp
    
    ! Scale horizontal metrics
    temp = int(metrics%width, 8) * int(scale_x, 8) / 65536
    metrics%width = int(temp, FT_Pos)
    
    temp = int(metrics%horiBearingX, 8) * int(scale_x, 8) / 65536
    metrics%horiBearingX = int(temp, FT_Pos)
    
    temp = int(metrics%horiAdvance, 8) * int(scale_x, 8) / 65536
    metrics%horiAdvance = int(temp, FT_Pos)
    
    ! Scale vertical metrics
    temp = int(metrics%height, 8) * int(scale_y, 8) / 65536
    metrics%height = int(temp, FT_Pos)
    
    temp = int(metrics%horiBearingY, 8) * int(scale_y, 8) / 65536
    metrics%horiBearingY = int(temp, FT_Pos)
    
    temp = int(metrics%vertBearingX, 8) * int(scale_x, 8) / 65536
    metrics%vertBearingX = int(temp, FT_Pos)
    
    temp = int(metrics%vertBearingY, 8) * int(scale_y, 8) / 65536
    metrics%vertBearingY = int(temp, FT_Pos)
    
    temp = int(metrics%vertAdvance, 8) * int(scale_y, 8) / 65536
    metrics%vertAdvance = int(temp, FT_Pos)
  end subroutine ft_glyph_metrics_scale

end module ft_glyph_types