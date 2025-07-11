module ft_raster_types
  use ft_types
  use ft_geometry, only: FT_Vector, FT_BBox
  use ft_bitmap_mod, only: FT_Bitmap
  use ft_outline_mod, only: FT_Outline
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  private
  
  ! Public types
  public :: FT_Raster_Cell
  public :: FT_Raster_State
  public :: FT_Raster_Params
  
  ! Public constants
  public :: FT_RASTER_FLAG_AA
  public :: FT_RASTER_FLAG_DIRECT
  public :: FT_RASTER_FLAG_CLIP
  
  ! Rasterizer cell for scan conversion
  type :: FT_Raster_Cell
    integer :: x          ! Cell x position
    integer :: cover      ! Coverage value
    integer :: area       ! Cell area
    type(FT_Raster_Cell), pointer :: next => null()  ! Next cell in list
  end type FT_Raster_Cell
  
  ! Rasterizer state/worker
  type :: FT_Raster_State
    ! Bounding box
    type(FT_BBox) :: cbox
    
    ! Pixel bounds
    integer :: min_ex, max_ex  ! Min/max x coordinates
    integer :: min_ey, max_ey  ! Min/max y coordinates
    integer :: count_ey        ! Height in pixels (max_ey - min_ey)
    
    ! Current position
    integer(FT_Fixed) :: x, y  ! Last point position
    
    ! Cell management
    type(FT_Raster_Cell), pointer :: cell => null()       ! Current cell
    type(FT_Raster_Cell), pointer :: cells(:) => null()   ! Cell pool
    integer :: num_cells                                   ! Total cells allocated
    integer :: cell_index                                  ! Next free cell index
    
    ! Y-sorted cell lists
    type(FT_Raster_Cell), pointer :: ycells(:) => null()  ! Array of cell lists per y
    
    ! Outline being rasterized
    type(FT_Outline), pointer :: outline => null()
    
    ! Target bitmap
    type(FT_Bitmap), pointer :: target => null()
    
    ! Rasterizer flags
    integer :: flags
    
    ! For monochrome rendering
    integer :: band_size      ! Height of rendering band
    integer :: band_shoot     ! Current band position
  end type FT_Raster_State
  
  ! Raster parameters
  type :: FT_Raster_Params
    type(FT_Bitmap), pointer :: target => null()
    type(c_ptr) :: source = c_null_ptr
    integer :: flags
    type(c_ptr) :: gray_spans = c_null_ptr
    type(c_ptr) :: user = c_null_ptr
    type(FT_BBox) :: clip_box
  end type FT_Raster_Params
  
  ! Rasterizer flags
  integer, parameter :: FT_RASTER_FLAG_DEFAULT = 0
  integer, parameter :: FT_RASTER_FLAG_AA = 1        ! Anti-aliased rendering
  integer, parameter :: FT_RASTER_FLAG_DIRECT = 2    ! Direct rendering
  integer, parameter :: FT_RASTER_FLAG_CLIP = 4      ! Use clipping box

end module ft_raster_types