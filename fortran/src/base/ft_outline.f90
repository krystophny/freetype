module ft_outline_mod
  use ft_types
  use ft_geometry, only: FT_Vector, FT_Matrix, FT_BBox, ft_vector_transform
  use tt_types, only: FT_Short, FT_UShort
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  private
  
  ! Public types
  public :: FT_Outline
  public :: FT_Outline_Funcs
  
  ! Public functions
  public :: ft_outline_new
  public :: ft_outline_done
  public :: ft_outline_copy
  public :: ft_outline_translate
  public :: ft_outline_transform
  public :: ft_outline_reverse
  public :: ft_outline_get_bbox
  public :: ft_outline_get_cbox
  public :: ft_outline_check
  
  ! Outline flags
  public :: FT_OUTLINE_NONE
  public :: FT_OUTLINE_OWNER
  public :: FT_OUTLINE_EVEN_ODD_FILL
  public :: FT_OUTLINE_REVERSE_FILL
  public :: FT_OUTLINE_IGNORE_DROPOUTS
  public :: FT_OUTLINE_SMART_DROPOUTS
  public :: FT_OUTLINE_INCLUDE_STUBS
  public :: FT_OUTLINE_HIGH_PRECISION
  public :: FT_OUTLINE_SINGLE_PASS
  
  ! Move/line/curve tags
  public :: FT_CURVE_TAG_ON
  public :: FT_CURVE_TAG_CONIC
  public :: FT_CURVE_TAG_CUBIC
  public :: FT_CURVE_TAG_TOUCH_X
  public :: FT_CURVE_TAG_TOUCH_Y
  
  ! Font outline structure
  type :: FT_Outline
    integer(FT_Short) :: n_contours     ! Number of contours
    integer(FT_Short) :: n_points       ! Number of points
    type(FT_Vector), pointer :: points(:) => null()     ! Point coordinates
    integer(int8), pointer :: tags(:) => null()         ! Point tags (on/off curve)
    integer(FT_Short), pointer :: contours(:) => null() ! End point of each contour
    integer(int32) :: flags            ! Outline flags
  end type FT_Outline
  
  ! Outline function table (for decomposition)
  type :: FT_Outline_Funcs
    procedure(move_to_func), pointer, nopass :: move_to => null()
    procedure(line_to_func), pointer, nopass :: line_to => null()
    procedure(conic_to_func), pointer, nopass :: conic_to => null()
    procedure(cubic_to_func), pointer, nopass :: cubic_to => null()
    integer :: shift    ! Shift value for coordinates
    integer :: delta    ! Delta for rounding
  end type FT_Outline_Funcs
  
  ! Outline flags
  integer(int32), parameter :: FT_OUTLINE_NONE = 0
  integer(int32), parameter :: FT_OUTLINE_OWNER = 1
  integer(int32), parameter :: FT_OUTLINE_EVEN_ODD_FILL = 2
  integer(int32), parameter :: FT_OUTLINE_REVERSE_FILL = 4
  integer(int32), parameter :: FT_OUTLINE_IGNORE_DROPOUTS = 8
  integer(int32), parameter :: FT_OUTLINE_SMART_DROPOUTS = 16
  integer(int32), parameter :: FT_OUTLINE_INCLUDE_STUBS = 32
  integer(int32), parameter :: FT_OUTLINE_HIGH_PRECISION = 256
  integer(int32), parameter :: FT_OUTLINE_SINGLE_PASS = 512
  
  ! Curve tags
  integer(int8), parameter :: FT_CURVE_TAG_ON = 1
  integer(int8), parameter :: FT_CURVE_TAG_CONIC = 0
  integer(int8), parameter :: FT_CURVE_TAG_CUBIC = 2
  integer(int8), parameter :: FT_CURVE_TAG_TOUCH_X = 8
  integer(int8), parameter :: FT_CURVE_TAG_TOUCH_Y = 16
  
  ! Function interfaces for outline decomposition
  abstract interface
    function move_to_func(to, user) result(error)
      import :: FT_Vector, FT_Error, c_ptr
      type(FT_Vector), intent(in) :: to
      type(c_ptr), intent(in) :: user
      integer :: error
    end function move_to_func
    
    function line_to_func(to, user) result(error)
      import :: FT_Vector, FT_Error, c_ptr
      type(FT_Vector), intent(in) :: to
      type(c_ptr), intent(in) :: user
      integer :: error
    end function line_to_func
    
    function conic_to_func(control, to, user) result(error)
      import :: FT_Vector, FT_Error, c_ptr
      type(FT_Vector), intent(in) :: control, to
      type(c_ptr), intent(in) :: user
      integer :: error
    end function conic_to_func
    
    function cubic_to_func(control1, control2, to, user) result(error)
      import :: FT_Vector, FT_Error, c_ptr
      type(FT_Vector), intent(in) :: control1, control2, to
      type(c_ptr), intent(in) :: user
      integer :: error
    end function cubic_to_func
  end interface

contains

  ! Create a new outline
  function ft_outline_new(n_points, n_contours, outline, error) result(success)
    integer, intent(in) :: n_points
    integer, intent(in) :: n_contours
    type(FT_Outline), intent(out) :: outline
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    success = .false.
    error = FT_Err_Ok
    
    ! Initialize outline
    outline%n_points = int(n_points, FT_Short)
    outline%n_contours = int(n_contours, FT_Short)
    outline%flags = ior(FT_OUTLINE_NONE, FT_OUTLINE_OWNER)
    
    ! Allocate arrays
    if (n_points > 0) then
      allocate(outline%points(n_points), stat=error)
      if (error /= 0) then
        error = FT_Err_Out_Of_Memory
        return
      end if
      
      allocate(outline%tags(n_points), stat=error)
      if (error /= 0) then
        deallocate(outline%points)
        error = FT_Err_Out_Of_Memory
        return
      end if
      
      ! Initialize points and tags
      outline%points = FT_Vector(0, 0)
      outline%tags = FT_CURVE_TAG_ON
    end if
    
    if (n_contours > 0) then
      allocate(outline%contours(n_contours), stat=error)
      if (error /= 0) then
        if (associated(outline%points)) deallocate(outline%points)
        if (associated(outline%tags)) deallocate(outline%tags)
        error = FT_Err_Out_Of_Memory
        return
      end if
      
      ! Initialize contours
      outline%contours = -1
    end if
    
    error = FT_Err_Ok
    success = .true.
    
  end function ft_outline_new
  
  ! Free outline memory
  subroutine ft_outline_done(outline)
    type(FT_Outline), intent(inout) :: outline
    
    ! Only free if we own the memory
    if (iand(outline%flags, FT_OUTLINE_OWNER) /= 0) then
      if (associated(outline%points)) then
        deallocate(outline%points)
        outline%points => null()
      end if
      
      if (associated(outline%tags)) then
        deallocate(outline%tags)
        outline%tags => null()
      end if
      
      if (associated(outline%contours)) then
        deallocate(outline%contours)
        outline%contours => null()
      end if
    end if
    
    outline%n_points = 0
    outline%n_contours = 0
    outline%flags = FT_OUTLINE_NONE
    
  end subroutine ft_outline_done
  
  ! Copy outline
  function ft_outline_copy(source, target, error) result(success)
    type(FT_Outline), intent(in) :: source
    type(FT_Outline), intent(out) :: target
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: i
    
    success = .false.
    
    ! Create new outline with same dimensions
    if (.not. ft_outline_new(int(source%n_points), &
                             int(source%n_contours), target, error)) then
      return
    end if
    
    ! Copy data
    if (associated(source%points) .and. associated(target%points)) then
      do i = 1, source%n_points
        target%points(i) = source%points(i)
      end do
    end if
    
    if (associated(source%tags) .and. associated(target%tags)) then
      do i = 1, source%n_points
        target%tags(i) = source%tags(i)
      end do
    end if
    
    if (associated(source%contours) .and. associated(target%contours)) then
      do i = 1, source%n_contours
        target%contours(i) = source%contours(i)
      end do
    end if
    
    ! Copy flags (but keep OWNER flag for target)
    target%flags = ior(iand(source%flags, not(FT_OUTLINE_OWNER)), FT_OUTLINE_OWNER)
    
    success = .true.
    
  end function ft_outline_copy
  
  ! Translate outline
  subroutine ft_outline_translate(outline, x_offset, y_offset)
    type(FT_Outline), intent(inout) :: outline
    integer(FT_Fixed), intent(in) :: x_offset, y_offset
    
    integer :: i
    
    if (.not. associated(outline%points)) return
    
    do i = 1, outline%n_points
      outline%points(i)%x = outline%points(i)%x + x_offset
      outline%points(i)%y = outline%points(i)%y + y_offset
    end do
    
  end subroutine ft_outline_translate
  
  ! Transform outline (placeholder for matrix transformation)
  subroutine ft_outline_transform(outline, matrix)
    type(FT_Outline), intent(inout) :: outline
    type(FT_Matrix), intent(in) :: matrix
    
    integer :: i
    
    if (.not. associated(outline%points)) return
    
    do i = 1, outline%n_points
      outline%points(i) = ft_vector_transform(outline%points(i), matrix)
    end do
    
  end subroutine ft_outline_transform
  
  ! Reverse outline orientation
  subroutine ft_outline_reverse(outline)
    type(FT_Outline), intent(inout) :: outline
    
    integer :: first, last, i, j
    type(FT_Vector) :: temp_point
    integer(int8) :: temp_tag
    
    if (.not. associated(outline%points) .or. outline%n_contours == 0) return
    
    ! Reverse each contour
    first = 1
    do i = 1, outline%n_contours
      last = outline%contours(i) + 1  ! Convert from 0-based to 1-based
      
      ! Reverse points and tags in this contour
      do j = 0, (last - first) / 2 - 1
        ! Swap points
        temp_point = outline%points(first + j)
        outline%points(first + j) = outline%points(last - 1 - j)
        outline%points(last - 1 - j) = temp_point
        
        ! Swap tags
        temp_tag = outline%tags(first + j)
        outline%tags(first + j) = outline%tags(last - 1 - j)
        outline%tags(last - 1 - j) = temp_tag
      end do
      
      first = last + 1
    end do
    
  end subroutine ft_outline_reverse
  
  ! Get outline bounding box (exact)
  function ft_outline_get_bbox(outline, bbox, error) result(success)
    type(FT_Outline), intent(in) :: outline
    type(FT_BBox), intent(out) :: bbox
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: i
    
    success = .false.
    error = FT_Err_Ok
    
    if (.not. associated(outline%points) .or. outline%n_points == 0) then
      bbox%xMin = 0
      bbox%yMin = 0
      bbox%xMax = 0
      bbox%yMax = 0
      success = .true.
      return
    end if
    
    ! Initialize with first point
    bbox%xMin = outline%points(1)%x
    bbox%yMin = outline%points(1)%y
    bbox%xMax = outline%points(1)%x
    bbox%yMax = outline%points(1)%y
    
    ! Find min/max coordinates
    do i = 2, outline%n_points
      if (outline%points(i)%x < bbox%xMin) bbox%xMin = outline%points(i)%x
      if (outline%points(i)%x > bbox%xMax) bbox%xMax = outline%points(i)%x
      if (outline%points(i)%y < bbox%yMin) bbox%yMin = outline%points(i)%y
      if (outline%points(i)%y > bbox%yMax) bbox%yMax = outline%points(i)%y
    end do
    
    success = .true.
    
  end function ft_outline_get_bbox
  
  ! Get outline control box (fast approximation)
  function ft_outline_get_cbox(outline, cbox, error) result(success)
    type(FT_Outline), intent(in) :: outline
    type(FT_BBox), intent(out) :: cbox
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    ! For now, control box is the same as bounding box
    success = ft_outline_get_bbox(outline, cbox, error)
    
  end function ft_outline_get_cbox
  
  ! Check outline validity
  function ft_outline_check(outline, error) result(success)
    type(FT_Outline), intent(in) :: outline
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: i, start, end_pt
    
    success = .false.
    error = FT_Err_Ok
    
    ! Check basic structure
    if (outline%n_contours < 0 .or. outline%n_points < 0) then
      error = FT_Err_Invalid_Outline
      return
    end if
    
    if (outline%n_contours > 0 .and. .not. associated(outline%contours)) then
      error = FT_Err_Invalid_Outline
      return
    end if
    
    if (outline%n_points > 0) then
      if (.not. associated(outline%points) .or. .not. associated(outline%tags)) then
        error = FT_Err_Invalid_Outline
        return
      end if
    end if
    
    ! Check contour endpoints
    start = 0
    do i = 1, outline%n_contours
      end_pt = outline%contours(i)
      
      if (end_pt < start .or. end_pt >= outline%n_points) then
        error = FT_Err_Invalid_Outline
        return
      end if
      
      start = end_pt + 1
    end do
    
    success = .true.
    
  end function ft_outline_check

end module ft_outline_mod