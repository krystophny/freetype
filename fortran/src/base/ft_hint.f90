module ft_hint
  use ft_types
  use ft_geometry, only: FT_Vector, FT_Matrix
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int16, int32
  implicit none
  private
  
  ! Public types
  public :: FT_Hint_Type
  public :: FT_Hint_Engine
  
  ! Public functions  
  public :: ft_hint_engine_new
  public :: ft_hint_engine_done
  public :: ft_hint_apply_outline
  public :: ft_hint_grid_fit_outline
  
  ! Hinting types
  integer, parameter :: FT_HINT_NONE = 0
  integer, parameter :: FT_HINT_LIGHT = 1
  integer, parameter :: FT_HINT_NORMAL = 2
  integer, parameter :: FT_HINT_STRONG = 3
  
  ! Hint structure
  type :: FT_Hint_Type
    integer :: hint_type = FT_HINT_NONE
    integer :: ppem_size = 0
    real :: scale_factor = 1.0
    logical :: grid_fitting = .false.
  end type FT_Hint_Type
  
  ! Hinting engine
  type :: FT_Hint_Engine
    type(FT_Hint_Type) :: hint_settings
    integer :: units_per_em = 1000
    real :: pixel_size = 12.0
    logical :: initialized = .false.
  end type FT_Hint_Engine

contains

  ! Create new hinting engine
  function ft_hint_engine_new(units_per_em, pixel_size, engine, error) result(success)
    integer, intent(in) :: units_per_em
    real, intent(in) :: pixel_size
    type(FT_Hint_Engine), intent(out) :: engine
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    success = .false.
    error = FT_Err_Ok
    
    ! Validate inputs
    if (units_per_em <= 0 .or. pixel_size <= 0.0) then
      error = FT_Err_Invalid_Pixel_Size
      return
    end if
    
    ! Initialize engine
    engine%units_per_em = units_per_em
    engine%pixel_size = pixel_size
    engine%hint_settings%scale_factor = pixel_size / real(units_per_em)
    engine%hint_settings%ppem_size = int(pixel_size)
    
    ! Set default hinting based on pixel size
    if (pixel_size < 9.0) then
      engine%hint_settings%hint_type = FT_HINT_STRONG
      engine%hint_settings%grid_fitting = .true.
    else if (pixel_size < 16.0) then
      engine%hint_settings%hint_type = FT_HINT_NORMAL
      engine%hint_settings%grid_fitting = .true.
    else if (pixel_size < 24.0) then
      engine%hint_settings%hint_type = FT_HINT_LIGHT
      engine%hint_settings%grid_fitting = .false.
    else
      engine%hint_settings%hint_type = FT_HINT_NONE
      engine%hint_settings%grid_fitting = .false.
    end if
    
    engine%initialized = .true.
    success = .true.
    
  end function ft_hint_engine_new
  
  ! Clean up hinting engine
  subroutine ft_hint_engine_done(engine)
    type(FT_Hint_Engine), intent(inout) :: engine
    
    engine%initialized = .false.
    engine%units_per_em = 0
    engine%pixel_size = 0.0
    engine%hint_settings%hint_type = FT_HINT_NONE
    engine%hint_settings%scale_factor = 1.0
    engine%hint_settings%grid_fitting = .false.
    
  end subroutine ft_hint_engine_done
  
  ! Apply hinting to outline
  function ft_hint_apply_outline(engine, outline, error) result(success)
    use ft_outline_mod, only: FT_Outline
    type(FT_Hint_Engine), intent(in) :: engine
    type(FT_Outline), intent(inout) :: outline
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: i
    real :: scale
    
    success = .false.
    error = FT_Err_Ok
    
    if (.not. engine%initialized) then
      error = FT_Err_Invalid_Handle
      return
    end if
    
    ! Skip hinting if disabled
    if (engine%hint_settings%hint_type == FT_HINT_NONE) then
      success = .true.
      return
    end if
    
    scale = engine%hint_settings%scale_factor
    
    ! Apply scaling to all points
    do i = 1, outline%n_points
      outline%points(i)%x = int(real(outline%points(i)%x) * scale, kind(outline%points(i)%x))
      outline%points(i)%y = int(real(outline%points(i)%y) * scale, kind(outline%points(i)%y))
    end do
    
    ! Apply grid fitting if enabled
    if (engine%hint_settings%grid_fitting) then
      success = ft_hint_grid_fit_outline(engine, outline, error)
    else
      success = .true.
    end if
    
  end function ft_hint_apply_outline
  
  ! Apply grid fitting to outline  
  function ft_hint_grid_fit_outline(engine, outline, error) result(success)
    use ft_outline_mod, only: FT_Outline
    type(FT_Hint_Engine), intent(in) :: engine
    type(FT_Outline), intent(inout) :: outline
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: i
    integer :: grid_size
    
    success = .false.
    error = FT_Err_Ok
    
    ! Determine grid size based on hint strength
    select case (engine%hint_settings%hint_type)
    case (FT_HINT_STRONG)
      grid_size = 64  ! 1 pixel at 64 units per pixel
    case (FT_HINT_NORMAL)
      grid_size = 32  ! 0.5 pixel
    case (FT_HINT_LIGHT)
      grid_size = 16  ! 0.25 pixel
    case default
      grid_size = 64
    end select
    
    ! Snap points to grid
    do i = 1, outline%n_points
      outline%points(i)%x = snap_to_grid(int(outline%points(i)%x), grid_size)
      outline%points(i)%y = snap_to_grid(int(outline%points(i)%y), grid_size)
    end do
    
    success = .true.
    
  end function ft_hint_grid_fit_outline
  
  ! Snap coordinate to grid
  function snap_to_grid(coord, grid_size) result(snapped)
    integer, intent(in) :: coord
    integer, intent(in) :: grid_size
    integer :: snapped
    
    integer :: half_grid
    
    half_grid = grid_size / 2
    
    if (coord >= 0) then
      snapped = ((coord + half_grid) / grid_size) * grid_size
    else
      snapped = ((coord - half_grid) / grid_size) * grid_size
    end if
    
  end function snap_to_grid

end module ft_hint