! Enhanced antialiasing module for improved quality and performance
module ft_antialiasing_enhanced
  use ft_types
  use ft_bitmap_mod
  use ft_outline_mod
  use ft_geometry, only: FT_Vector, FT_BBox
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, real32, real64
  implicit none
  private
  
  ! Public types
  public :: FT_AA_Engine
  public :: FT_AA_Quality
  
  ! Public functions
  public :: ft_aa_engine_new
  public :: ft_aa_engine_done
  public :: ft_aa_render_outline
  public :: ft_aa_render_outline_hq
  public :: point_in_outline
  public :: coverage_to_bitmap
  public :: render_normal_aa
  
  ! Public constants
  public :: FT_AA_QUALITY_FAST
  public :: FT_AA_QUALITY_NORMAL
  public :: FT_AA_QUALITY_HIGH
  public :: FT_AA_QUALITY_ULTRA
  
  ! Antialiasing quality levels
  integer, parameter :: FT_AA_QUALITY_FAST = 1
  integer, parameter :: FT_AA_QUALITY_NORMAL = 2
  integer, parameter :: FT_AA_QUALITY_HIGH = 3
  integer, parameter :: FT_AA_QUALITY_ULTRA = 4
  
  ! Enhanced antialiasing quality type
  type :: FT_AA_Quality
    integer :: level = FT_AA_QUALITY_NORMAL
    integer :: samples_per_pixel = 4     ! Number of subpixel samples
    real :: gamma_correction = 2.2       ! Gamma correction value
    logical :: linear_filtering = .true. ! Use linear filtering
    logical :: edge_enhancement = .false. ! Enhance edge contrast
  end type FT_AA_Quality
  
  ! Enhanced antialiasing engine
  type :: FT_AA_Engine
    type(FT_AA_Quality) :: quality
    integer :: width = 0
    integer :: height = 0
    real(real32), allocatable :: coverage_buffer(:,:)
    real(real32), allocatable :: temp_buffer(:,:)
    logical :: initialized = .false.
  end type FT_AA_Engine
  
  ! Subpixel sampling patterns
  real(real32), parameter :: SAMPLE_PATTERNS_4X(2,4) = reshape([ &
    0.25, 0.25, &
    0.75, 0.25, &
    0.25, 0.75, &
    0.75, 0.75  &
  ], [2, 4])
  
  real(real32), parameter :: SAMPLE_PATTERNS_8X(2,8) = reshape([ &
    0.125, 0.125, &
    0.375, 0.125, &
    0.625, 0.125, &
    0.875, 0.125, &
    0.125, 0.875, &
    0.375, 0.875, &
    0.625, 0.875, &
    0.875, 0.875  &
  ], [2, 8])

contains

  ! Create new enhanced antialiasing engine
  function ft_aa_engine_new(width, height, quality, engine, error) result(success)
    integer, intent(in) :: width, height
    type(FT_AA_Quality), intent(in) :: quality
    type(FT_AA_Engine), intent(out) :: engine
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    success = .false.
    error = FT_Err_Ok
    
    ! Validate parameters
    if (width <= 0 .or. height <= 0) then
      error = FT_Err_Invalid_Argument
      return
    end if
    
    ! Initialize engine
    engine%width = width
    engine%height = height
    engine%quality = quality
    
    ! Allocate coverage buffer
    allocate(engine%coverage_buffer(width, height), stat=error)
    if (error /= 0) then
      error = FT_Err_Out_Of_Memory
      return
    end if
    
    ! Allocate temp buffer for high-quality modes
    if (quality%level >= FT_AA_QUALITY_HIGH) then
      allocate(engine%temp_buffer(width, height), stat=error)
      if (error /= 0) then
        deallocate(engine%coverage_buffer)
        error = FT_Err_Out_Of_Memory
        return
      end if
    end if
    
    ! Clear buffers
    engine%coverage_buffer = 0.0
    if (allocated(engine%temp_buffer)) then
      engine%temp_buffer = 0.0
    end if
    
    engine%initialized = .true.
    success = .true.
    
  end function ft_aa_engine_new

  ! Clean up antialiasing engine
  subroutine ft_aa_engine_done(engine)
    type(FT_AA_Engine), intent(inout) :: engine
    
    if (allocated(engine%coverage_buffer)) then
      deallocate(engine%coverage_buffer)
    end if
    
    if (allocated(engine%temp_buffer)) then
      deallocate(engine%temp_buffer)
    end if
    
    engine%width = 0
    engine%height = 0
    engine%initialized = .false.
    
  end subroutine ft_aa_engine_done

  ! Enhanced antialiased outline rendering
  function ft_aa_render_outline(engine, outline, bitmap, error) result(success)
    type(FT_AA_Engine), intent(inout) :: engine
    type(FT_Outline), intent(in) :: outline
    type(FT_Bitmap), intent(inout) :: bitmap
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    success = .false.
    error = FT_Err_Ok
    
    if (.not. engine%initialized) then
      error = FT_Err_Invalid_Handle
      return
    end if
    
    ! Clear coverage buffer
    engine%coverage_buffer = 0.0
    
    ! Render based on quality level
    select case (engine%quality%level)
    case (FT_AA_QUALITY_FAST)
      success = render_fast_aa(engine, outline, error)
    case (FT_AA_QUALITY_NORMAL)
      success = render_normal_aa(engine, outline, error)
    case (FT_AA_QUALITY_HIGH)
      success = render_high_aa(engine, outline, error)
    case (FT_AA_QUALITY_ULTRA)
      success = render_ultra_aa(engine, outline, error)
    case default
      success = render_normal_aa(engine, outline, error)
    end select
    
    if (.not. success) return
    
    ! Convert coverage to bitmap with gamma correction
    success = coverage_to_bitmap(engine, bitmap, error)
    
  end function ft_aa_render_outline

  ! High-quality antialiased rendering with advanced features
  function ft_aa_render_outline_hq(engine, outline, bitmap, error) result(success)
    type(FT_AA_Engine), intent(inout) :: engine
    type(FT_Outline), intent(in) :: outline
    type(FT_Bitmap), intent(inout) :: bitmap
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    success = .false.
    error = FT_Err_Ok
    
    if (.not. engine%initialized) then
      error = FT_Err_Invalid_Handle
      return
    end if
    
    ! Clear buffers
    engine%coverage_buffer = 0.0
    if (allocated(engine%temp_buffer)) then
      engine%temp_buffer = 0.0
    end if
    
    ! Multi-pass rendering for maximum quality
    success = render_multipass_aa(engine, outline, error)
    if (.not. success) return
    
    ! Apply post-processing filters
    if (engine%quality%edge_enhancement) then
      call apply_edge_enhancement(engine)
    end if
    
    if (engine%quality%linear_filtering) then
      call apply_linear_filter(engine)
    end if
    
    ! Convert to final bitmap with gamma correction
    success = coverage_to_bitmap_hq(engine, bitmap, error)
    
  end function ft_aa_render_outline_hq

  ! Fast antialiasing (2x2 supersampling)
  function render_fast_aa(engine, outline, error) result(success)
    type(FT_AA_Engine), intent(inout) :: engine
    type(FT_Outline), intent(in) :: outline
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: x, y, sx, sy
    real(real32) :: sample_x, sample_y
    integer :: hits
    
    success = .false.
    error = FT_Err_Ok
    
    ! Simple 2x2 supersampling
    do y = 1, engine%height
      do x = 1, engine%width
        hits = 0
        
        ! Sample 4 points per pixel
        do sy = 1, 2
          do sx = 1, 2
            sample_x = real(x - 1) + (real(sx) - 0.5) / 2.0
            sample_y = real(y - 1) + (real(sy) - 0.5) / 2.0
            
            if (point_in_outline(outline, sample_x, sample_y)) then
              hits = hits + 1
            end if
          end do
        end do
        
        engine%coverage_buffer(x, y) = real(hits) / 4.0
      end do
    end do
    
    success = .true.
    
  end function render_fast_aa

  ! Normal antialiasing (4x4 supersampling)
  function render_normal_aa(engine, outline, error) result(success)
    type(FT_AA_Engine), intent(inout) :: engine
    type(FT_Outline), intent(in) :: outline
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: x, y, s
    real(real32) :: sample_x, sample_y
    integer :: hits
    
    success = .false.
    error = FT_Err_Ok
    
    ! 4x supersampling with optimized pattern
    do y = 1, engine%height
      do x = 1, engine%width
        hits = 0
        
        ! Sample using optimized 4-point pattern
        do s = 1, 4
          sample_x = real(x - 1) + SAMPLE_PATTERNS_4X(1, s)
          sample_y = real(y - 1) + SAMPLE_PATTERNS_4X(2, s)
          
          if (point_in_outline(outline, sample_x, sample_y)) then
            hits = hits + 1
          end if
        end do
        
        engine%coverage_buffer(x, y) = real(hits) / 4.0
      end do
    end do
    
    success = .true.
    
  end function render_normal_aa

  ! High-quality antialiasing (8x8 supersampling)
  function render_high_aa(engine, outline, error) result(success)
    type(FT_AA_Engine), intent(inout) :: engine
    type(FT_Outline), intent(in) :: outline
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: x, y, s
    real(real32) :: sample_x, sample_y
    integer :: hits
    
    success = .false.
    error = FT_Err_Ok
    
    ! 8x supersampling
    do y = 1, engine%height
      do x = 1, engine%width
        hits = 0
        
        ! Sample using 8-point pattern
        do s = 1, 8
          sample_x = real(x - 1) + SAMPLE_PATTERNS_8X(1, s)
          sample_y = real(y - 1) + SAMPLE_PATTERNS_8X(2, s)
          
          if (point_in_outline(outline, sample_x, sample_y)) then
            hits = hits + 1
          end if
        end do
        
        engine%coverage_buffer(x, y) = real(hits) / 8.0
      end do
    end do
    
    success = .true.
    
  end function render_high_aa

  ! Ultra-quality antialiasing (16x16 supersampling)
  function render_ultra_aa(engine, outline, error) result(success)
    type(FT_AA_Engine), intent(inout) :: engine
    type(FT_Outline), intent(in) :: outline
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: x, y, sx, sy
    real(real32) :: sample_x, sample_y
    integer :: hits
    
    success = .false.
    error = FT_Err_Ok
    
    ! 16x supersampling (4x4 grid per pixel)
    do y = 1, engine%height
      do x = 1, engine%width
        hits = 0
        
        ! Sample 16 points per pixel
        do sy = 1, 4
          do sx = 1, 4
            sample_x = real(x - 1) + (real(sx) - 0.5) / 4.0
            sample_y = real(y - 1) + (real(sy) - 0.5) / 4.0
            
            if (point_in_outline(outline, sample_x, sample_y)) then
              hits = hits + 1
            end if
          end do
        end do
        
        engine%coverage_buffer(x, y) = real(hits) / 16.0
      end do
    end do
    
    success = .true.
    
  end function render_ultra_aa

  ! Multi-pass rendering for maximum quality
  function render_multipass_aa(engine, outline, error) result(success)
    type(FT_AA_Engine), intent(inout) :: engine
    type(FT_Outline), intent(in) :: outline
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    ! For now, use ultra-quality as multipass
    success = render_ultra_aa(engine, outline, error)
    
  end function render_multipass_aa

  ! Apply edge enhancement
  subroutine apply_edge_enhancement(engine)
    type(FT_AA_Engine), intent(inout) :: engine
    
    integer :: x, y
    real(real32) :: edge_strength, center, neighbors
    
    ! Copy to temp buffer
    engine%temp_buffer = engine%coverage_buffer
    
    ! Apply edge enhancement kernel
    do y = 2, engine%height - 1
      do x = 2, engine%width - 1
        center = engine%temp_buffer(x, y)
        
        ! Calculate neighbor average
        neighbors = (engine%temp_buffer(x-1, y) + engine%temp_buffer(x+1, y) + &
                    engine%temp_buffer(x, y-1) + engine%temp_buffer(x, y+1)) / 4.0
        
        ! Enhance edges
        edge_strength = center - neighbors
        engine%coverage_buffer(x, y) = max(0.0, min(1.0, center + edge_strength * 0.3))
      end do
    end do
    
  end subroutine apply_edge_enhancement

  ! Apply linear filtering for smoother results
  subroutine apply_linear_filter(engine)
    type(FT_AA_Engine), intent(inout) :: engine
    
    integer :: x, y
    real(real32) :: filtered
    
    ! Copy to temp buffer
    engine%temp_buffer = engine%coverage_buffer
    
    ! Apply 3x3 linear filter
    do y = 2, engine%height - 1
      do x = 2, engine%width - 1
        filtered = (engine%temp_buffer(x-1, y-1) + 2*engine%temp_buffer(x, y-1) + engine%temp_buffer(x+1, y-1) + &
                   2*engine%temp_buffer(x-1, y) + 4*engine%temp_buffer(x, y) + 2*engine%temp_buffer(x+1, y) + &
                   engine%temp_buffer(x-1, y+1) + 2*engine%temp_buffer(x, y+1) + engine%temp_buffer(x+1, y+1)) / 16.0
        
        engine%coverage_buffer(x, y) = filtered
      end do
    end do
    
  end subroutine apply_linear_filter

  ! Convert coverage to bitmap with gamma correction
  function coverage_to_bitmap(engine, bitmap, error) result(success)
    type(FT_AA_Engine), intent(in) :: engine
    type(FT_Bitmap), intent(inout) :: bitmap
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: x, y, pixel_value
    real(real32) :: coverage, gamma_corrected
    
    success = .false.
    error = FT_Err_Ok
    
    do y = 1, min(engine%height, bitmap%rows)
      do x = 1, min(engine%width, bitmap%width)
        coverage = engine%coverage_buffer(x, y)
        
        if (coverage > 0.0) then
          ! Apply gamma correction
          if (engine%quality%gamma_correction > 0.0) then
            gamma_corrected = coverage ** (1.0 / engine%quality%gamma_correction)
          else
            gamma_corrected = coverage
          end if
          
          ! Convert to 8-bit pixel value
          pixel_value = int(gamma_corrected * 255.0)
          
          ! Set pixel in bitmap
          call ft_bitmap_set_pixel_gray(bitmap, x-1, y-1, pixel_value)
        end if
      end do
    end do
    
    success = .true.
    
  end function coverage_to_bitmap

  ! High-quality coverage to bitmap conversion
  function coverage_to_bitmap_hq(engine, bitmap, error) result(success)
    type(FT_AA_Engine), intent(in) :: engine
    type(FT_Bitmap), intent(inout) :: bitmap
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    ! For now, use the same conversion as regular quality
    success = coverage_to_bitmap(engine, bitmap, error)
    
  end function coverage_to_bitmap_hq

  ! Point-in-outline test with proper contour handling
  function point_in_outline(outline, x, y) result(inside)
    type(FT_Outline), intent(in) :: outline
    real(real32), intent(in) :: x, y
    logical :: inside
    
    integer :: i, j, crossings, contour_idx, start_point, end_point
    real(real32) :: x1, y1, x2, y2
    
    inside = .false.
    crossings = 0
    
    ! Process each contour separately
    start_point = 1
    do contour_idx = 1, outline%n_contours
      end_point = outline%contours(contour_idx) + 1  ! Convert from 0-based to 1-based
      
      ! Ray casting for this contour
      do i = start_point, end_point - 1
        if (i == end_point - 1) then
          ! Last point in contour connects back to first point
          j = start_point
        else
          j = i + 1
        end if
        
        x1 = real(outline%points(i)%x)
        y1 = real(outline%points(i)%y)
        x2 = real(outline%points(j)%x)
        y2 = real(outline%points(j)%y)
        
        ! Check if ray from point crosses this edge
        if (((y1 <= y) .and. (y < y2)) .or. ((y2 <= y) .and. (y < y1))) then
          if (x < x1 + (y - y1) / (y2 - y1) * (x2 - x1)) then
            crossings = crossings + 1
          end if
        end if
      end do
      
      start_point = end_point
    end do
    
    inside = (mod(crossings, 2) == 1)
    
  end function point_in_outline

end module ft_antialiasing_enhanced