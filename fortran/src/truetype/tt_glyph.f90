module tt_glyph
  use ft_types
  use ft_stream
  use tt_types, only: FT_Short, FT_UShort, &
       ARGS_ARE_WORDS, ARGS_ARE_XY_VALUES, WE_HAVE_A_SCALE, MORE_COMPONENTS, &
       WE_HAVE_AN_XY_SCALE, WE_HAVE_A_2X2, TT_Component, TT_Composite_Glyph
  use, intrinsic :: iso_c_binding, only: c_size_t
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  private
  
  ! Public types
  public :: TT_Glyph_Header
  public :: TT_Simple_Glyph
  public :: TT_Glyph_Metrics
  
  ! Public functions
  public :: tt_load_glyph_header
  public :: tt_load_simple_glyph
  public :: tt_load_composite_glyph
  public :: tt_load_glyph_by_index
  public :: tt_load_glyph_by_index_with_offset
  public :: tt_glyph_to_outline
  public :: tt_glyph_free
  
  ! Glyph flags
  public :: TT_FLAG_ON_CURVE
  public :: TT_FLAG_X_SHORT_VECTOR
  public :: TT_FLAG_Y_SHORT_VECTOR
  public :: TT_FLAG_REPEAT
  public :: TT_FLAG_X_SAME_OR_POSITIVE
  public :: TT_FLAG_Y_SAME_OR_POSITIVE
  
  ! TrueType glyph header (10 bytes)
  type, bind(C) :: TT_Glyph_Header
    integer(FT_Short) :: num_contours    ! >= 0 for simple glyph, -1 for composite
    integer(FT_Short) :: x_min           ! Minimum x coordinate
    integer(FT_Short) :: y_min           ! Minimum y coordinate
    integer(FT_Short) :: x_max           ! Maximum x coordinate
    integer(FT_Short) :: y_max           ! Maximum y coordinate
  end type TT_Glyph_Header
  
  ! Glyph metrics (derived from header and advance info)
  type :: TT_Glyph_Metrics
    integer(FT_Short) :: width           ! Glyph width (x_max - x_min)
    integer(FT_Short) :: height          ! Glyph height (y_max - y_min)
    integer(FT_Short) :: horiBearingX    ! Left side bearing
    integer(FT_Short) :: horiBearingY    ! Top side bearing
    integer(FT_UShort) :: horiAdvance    ! Horizontal advance width
    integer(FT_Short) :: vertBearingX    ! Vertical bearing X
    integer(FT_Short) :: vertBearingY    ! Vertical bearing Y
    integer(FT_UShort) :: vertAdvance    ! Vertical advance height
  end type TT_Glyph_Metrics
  
  ! Simple glyph data (non-composite)
  type :: TT_Simple_Glyph
    type(TT_Glyph_Header) :: header
    type(TT_Glyph_Metrics) :: metrics
    
    ! Contour data
    integer(FT_UShort), allocatable :: end_pts_of_contours(:)  ! Last point of each contour
    integer(FT_UShort) :: instruction_length                   ! Length of instructions
    integer(int8), allocatable :: instructions(:)              ! Bytecode instructions
    
    ! Point data
    integer :: num_points                                       ! Total number of points
    integer(int8), allocatable :: flags(:)                     ! Point flags
    integer(FT_Short), allocatable :: x_coordinates(:)         ! X coordinates
    integer(FT_Short), allocatable :: y_coordinates(:)         ! Y coordinates
    
    ! Derived data
    logical, allocatable :: on_curve(:)                        ! Whether point is on curve
  end type TT_Simple_Glyph
  
  ! TrueType glyph flags
  integer(int8), parameter :: TT_FLAG_ON_CURVE = 1              ! Point is on the curve
  integer(int8), parameter :: TT_FLAG_X_SHORT_VECTOR = 2        ! X coordinate is 1 byte
  integer(int8), parameter :: TT_FLAG_Y_SHORT_VECTOR = 4        ! Y coordinate is 1 byte
  integer(int8), parameter :: TT_FLAG_REPEAT = 8                ! Repeat flag for next N points
  integer(int8), parameter :: TT_FLAG_X_SAME_OR_POSITIVE = 16   ! X coordinate same or positive
  integer(int8), parameter :: TT_FLAG_Y_SAME_OR_POSITIVE = 32   ! Y coordinate same or positive

contains

  ! Load glyph header from stream
  function tt_load_glyph_header(stream, header, error) result(success)
    type(FT_Stream_Type), intent(inout) :: stream
    type(TT_Glyph_Header), intent(out) :: header
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer(int16) :: temp16
    
    success = .false.
    error = FT_Err_Ok
    
    ! Read number of contours
    if (.not. ft_stream_read_short(stream, temp16, error)) return
    header%num_contours = temp16
    
    ! Read bounding box
    if (.not. ft_stream_read_short(stream, temp16, error)) return
    header%x_min = temp16
    
    if (.not. ft_stream_read_short(stream, temp16, error)) return
    header%y_min = temp16
    
    if (.not. ft_stream_read_short(stream, temp16, error)) return
    header%x_max = temp16
    
    if (.not. ft_stream_read_short(stream, temp16, error)) return
    header%y_max = temp16
    
    success = .true.
    
  end function tt_load_glyph_header
  
  ! Load simple glyph data (non-composite)
  function tt_load_simple_glyph(stream, glyph, error) result(success)
    type(FT_Stream_Type), intent(inout) :: stream
    type(TT_Simple_Glyph), intent(out) :: glyph
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: i, j, repeat_count, flag_idx, coord_idx
    integer(int16) :: temp16
    integer(int8) :: temp8
    integer(FT_Short) :: delta, coord
    logical :: x_short, y_short, x_same, y_same
    
    success = .false.
    error = FT_Err_Ok
    
    ! Load glyph header first
    if (.not. tt_load_glyph_header(stream, glyph%header, error)) return
    
    ! Verify this is a simple glyph (header should already be loaded by caller)
    if (glyph%header%num_contours < 0) then
      error = FT_Err_Invalid_Glyph_Format  ! Composite glyph
      return
    end if
    
    if (glyph%header%num_contours == 0) then
      ! Empty glyph (e.g., space character)
      glyph%num_points = 0
      call compute_glyph_metrics(glyph)
      success = .true.
      return
    end if
    
    ! Allocate and read contour endpoints
    allocate(glyph%end_pts_of_contours(glyph%header%num_contours))
    
    do i = 1, glyph%header%num_contours
      if (.not. ft_stream_read_ushort(stream, temp16, error)) then
        deallocate(glyph%end_pts_of_contours)
        return
      end if
      glyph%end_pts_of_contours(i) = temp16
    end do
    
    ! Calculate total number of points
    glyph%num_points = glyph%end_pts_of_contours(glyph%header%num_contours) + 1
    
    ! Read instruction length
    if (.not. ft_stream_read_ushort(stream, temp16, error)) then
      deallocate(glyph%end_pts_of_contours)
      return
    end if
    glyph%instruction_length = temp16
    
    ! Read instructions (if any)
    if (glyph%instruction_length > 0) then
      allocate(glyph%instructions(glyph%instruction_length))
      
      do i = 1, glyph%instruction_length
        if (.not. ft_stream_read_byte(stream, temp8, error)) then
          call cleanup_simple_glyph(glyph)
          return
        end if
        glyph%instructions(i) = temp8
      end do
    end if
    
    ! If no points, we're done
    if (glyph%num_points == 0) then
      call compute_glyph_metrics(glyph)
      success = .true.
      return
    end if
    
    ! Allocate point arrays with proper error checking
    allocate(glyph%flags(glyph%num_points), stat=error)
    if (error /= 0) then
      call cleanup_simple_glyph(glyph)
      error = FT_Err_Out_Of_Memory
      return
    end if
    
    allocate(glyph%x_coordinates(glyph%num_points), stat=error)
    if (error /= 0) then
      call cleanup_simple_glyph(glyph)
      error = FT_Err_Out_Of_Memory
      return
    end if
    
    allocate(glyph%y_coordinates(glyph%num_points), stat=error)
    if (error /= 0) then
      call cleanup_simple_glyph(glyph)
      error = FT_Err_Out_Of_Memory
      return
    end if
    
    allocate(glyph%on_curve(glyph%num_points), stat=error)
    if (error /= 0) then
      call cleanup_simple_glyph(glyph)
      error = FT_Err_Out_Of_Memory
      return
    end if
    
    ! Read flags array (with repeat flag handling)
    flag_idx = 1
    do while (flag_idx <= glyph%num_points .and. flag_idx > 0)
      ! Validate flag_idx before using it
      if (flag_idx < 1 .or. flag_idx > glyph%num_points) then
        call cleanup_simple_glyph(glyph)
        error = FT_Err_Invalid_Glyph_Format
        return
      end if
      
      if (.not. ft_stream_read_byte(stream, temp8, error)) then
        call cleanup_simple_glyph(glyph)
        return
      end if
      
      glyph%flags(flag_idx) = temp8
      glyph%on_curve(flag_idx) = (iand(temp8, TT_FLAG_ON_CURVE) /= 0)
      
      ! Handle repeat flag
      if (iand(temp8, TT_FLAG_REPEAT) /= 0) then
        if (.not. ft_stream_read_byte(stream, temp8, error)) then
          call cleanup_simple_glyph(glyph)
          return
        end if
        repeat_count = temp8
        
        ! Validate repeat count to prevent overflow
        if (repeat_count < 0 .or. flag_idx + repeat_count > glyph%num_points) then
          call cleanup_simple_glyph(glyph)
          error = FT_Err_Invalid_Glyph_Format
          return
        end if
        
        do j = 1, repeat_count
          if (flag_idx + j > glyph%num_points) exit
          glyph%flags(flag_idx + j) = glyph%flags(flag_idx)
          glyph%on_curve(flag_idx + j) = glyph%on_curve(flag_idx)
        end do
        
        flag_idx = flag_idx + repeat_count + 1
      else
        flag_idx = flag_idx + 1
      end if
    end do
    
    ! Read X coordinates
    coord = 0
    do i = 1, glyph%num_points
      x_short = (iand(glyph%flags(i), TT_FLAG_X_SHORT_VECTOR) /= 0)
      x_same = (iand(glyph%flags(i), TT_FLAG_X_SAME_OR_POSITIVE) /= 0)
      
      if (x_short) then
        if (.not. ft_stream_read_byte(stream, temp8, error)) then
          call cleanup_simple_glyph(glyph)
          return
        end if
        delta = temp8
        if (.not. x_same) delta = -delta
      else if (.not. x_same) then
        if (.not. ft_stream_read_short(stream, delta, error)) then
          call cleanup_simple_glyph(glyph)
          return
        end if
      else
        delta = 0
      end if
      
      coord = coord + delta
      glyph%x_coordinates(i) = coord
    end do
    
    ! Read Y coordinates
    coord = 0
    do i = 1, glyph%num_points
      y_short = (iand(glyph%flags(i), TT_FLAG_Y_SHORT_VECTOR) /= 0)
      y_same = (iand(glyph%flags(i), TT_FLAG_Y_SAME_OR_POSITIVE) /= 0)
      
      if (y_short) then
        if (.not. ft_stream_read_byte(stream, temp8, error)) then
          call cleanup_simple_glyph(glyph)
          return
        end if
        delta = temp8
        if (.not. y_same) delta = -delta
      else if (.not. y_same) then
        if (.not. ft_stream_read_short(stream, delta, error)) then
          call cleanup_simple_glyph(glyph)
          return
        end if
      else
        delta = 0
      end if
      
      coord = coord + delta
      glyph%y_coordinates(i) = coord
    end do
    
    ! Compute glyph metrics
    call compute_glyph_metrics(glyph)
    
    success = .true.
    
  end function tt_load_simple_glyph
  
  ! Compute glyph metrics from header
  subroutine compute_glyph_metrics(glyph)
    type(TT_Simple_Glyph), intent(inout) :: glyph
    
    glyph%metrics%width = glyph%header%x_max - glyph%header%x_min
    glyph%metrics%height = glyph%header%y_max - glyph%header%y_min
    glyph%metrics%horiBearingX = glyph%header%x_min
    glyph%metrics%horiBearingY = glyph%header%y_max
    
    ! These will be filled in by the face when advance info is available
    glyph%metrics%horiAdvance = 0
    glyph%metrics%vertBearingX = 0
    glyph%metrics%vertBearingY = 0
    glyph%metrics%vertAdvance = 0
    
  end subroutine compute_glyph_metrics
  
  ! Free glyph memory
  subroutine tt_glyph_free(glyph)
    type(TT_Simple_Glyph), intent(inout) :: glyph
    
    call cleanup_simple_glyph(glyph)
    glyph%num_points = 0
    glyph%instruction_length = 0
    
  end subroutine tt_glyph_free
  
  ! Internal cleanup helper
  subroutine cleanup_simple_glyph(glyph)
    type(TT_Simple_Glyph), intent(inout) :: glyph
    
    if (allocated(glyph%end_pts_of_contours)) deallocate(glyph%end_pts_of_contours)
    if (allocated(glyph%instructions)) deallocate(glyph%instructions)
    if (allocated(glyph%flags)) deallocate(glyph%flags)
    if (allocated(glyph%x_coordinates)) deallocate(glyph%x_coordinates)
    if (allocated(glyph%y_coordinates)) deallocate(glyph%y_coordinates)
    if (allocated(glyph%on_curve)) deallocate(glyph%on_curve)
    
  end subroutine cleanup_simple_glyph

  ! Load a glyph by index using loca table and glyf table (with glyf offset)
  function tt_load_glyph_by_index_with_offset(stream, loca_table, glyph_index, glyf_offset, glyph, error) result(success)
    use tt_loca, only: TT_Loca_Table
    type(FT_Stream_Type), intent(inout) :: stream
    type(TT_Loca_Table), intent(in) :: loca_table
    integer, intent(in) :: glyph_index
    integer(c_size_t), intent(in) :: glyf_offset
    type(TT_Simple_Glyph), intent(out) :: glyph
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: glyph_offset, glyph_size
    
    success = .false.
    error = FT_Err_Ok
    
    ! Validate glyph index
    if (glyph_index < 0 .or. glyph_index >= size(loca_table%offsets) - 1) then
      error = FT_Err_Invalid_Glyph_Index
      return
    end if
    
    ! Get glyph offset and size from loca table
    glyph_offset = loca_table%offsets(glyph_index)      ! 0-based indexing matches C
    glyph_size = loca_table%offsets(glyph_index + 1) - glyph_offset
    
    ! DEBUG: Show what we're reading for glyph 36
    if (glyph_index == 36) then
      print *, "DEBUG tt_glyph.f90: glyph_index =", glyph_index
      print *, "DEBUG tt_glyph.f90: loca_table%offsets(36) =", loca_table%offsets(36)
      print *, "DEBUG tt_glyph.f90: loca_table%offsets(37) =", loca_table%offsets(37)
      print *, "DEBUG tt_glyph.f90: glyph_offset =", glyph_offset
      print *, "DEBUG tt_glyph.f90: glyph_size =", glyph_size
    end if
    
    ! Empty glyph (size 0)
    if (glyph_size == 0) then
      ! Initialize as empty glyph
      glyph%header%num_contours = 0
      glyph%header%x_min = 0
      glyph%header%y_min = 0
      glyph%header%x_max = 0
      glyph%header%y_max = 0
      glyph%num_points = 0
      glyph%instruction_length = 0
      call compute_glyph_metrics(glyph)
      success = .true.
      return
    end if
    
    ! Seek to glyph data (glyph_offset is relative to glyf table)
    if (.not. ft_stream_seek(stream, glyf_offset + int(glyph_offset, c_size_t), error)) then
      return
    end if
    
    ! Load glyph header first to determine type
    if (.not. tt_load_glyph_header(stream, glyph%header, error)) then
      return
    end if
    
    
    ! Reset stream position to start of glyph data (after seeking to glyph)
    if (.not. ft_stream_seek(stream, glyf_offset + int(glyph_offset, c_size_t), error)) then
      return
    end if
    
    ! For simple glyphs (num_contours >= 0)
    if (glyph%header%num_contours >= 0) then
      ! Let simple glyph loader read the header again
      success = tt_load_simple_glyph(stream, glyph, error)
    else
      ! Composite glyph - load and process components  
      success = tt_load_composite_glyph(stream, glyph, error)
    end if
    
  end function tt_load_glyph_by_index_with_offset

  ! Load composite glyph - converts to simple glyph representation
  function tt_load_composite_glyph(stream, glyph, error) result(success)
    type(FT_Stream_Type), intent(inout) :: stream
    type(TT_Simple_Glyph), intent(out) :: glyph
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    type(TT_Composite_Glyph) :: composite
    integer(FT_UShort) :: flags
    integer :: component_count, i, flags_int
    integer :: mask_words, mask_scale, mask_xy_scale, mask_2x2, mask_more
    logical :: more_components
    integer(int8) :: temp8
    integer(int16) :: temp16
    
    success = .false.
    error = FT_Err_Ok
    
    ! Load glyph header first
    if (.not. tt_load_glyph_header(stream, glyph%header, error)) return
    
    ! Initialize composite glyph and allocate storage for components
    component_count = 0
    more_components = .true.
    
    ! Define flag masks as integers (hex values from TrueType spec)
    mask_words = int(z'0001')      ! ARGS_ARE_WORDS
    mask_scale = int(z'0008')      ! WE_HAVE_A_SCALE
    mask_xy_scale = int(z'0040')   ! WE_HAVE_AN_XY_SCALE
    mask_2x2 = int(z'0080')        ! WE_HAVE_A_2X2
    mask_more = int(z'0020')       ! MORE_COMPONENTS
    
    ! Allocate initial space for up to 16 components (will resize if needed)
    allocate(composite%components(16), stat=error)
    if (error /= 0) then
      error = FT_Err_Out_Of_Memory
      return
    end if
    
    ! Parse and store components in a single pass
    do while (more_components)
      component_count = component_count + 1
      
      ! Expand array if needed
      if (component_count > size(composite%components)) then
        ! TODO: Implement array resizing - for now just error out
        deallocate(composite%components)
        error = FT_Err_Invalid_Glyph_Format
        return
      end if
      
      if (.not. ft_stream_read_ushort(stream, flags, error)) then
        deallocate(composite%components)
        return
      end if
      
      composite%components(component_count)%flags = flags
      flags_int = int(flags)
      
      ! Read glyph index
      if (.not. ft_stream_read_ushort(stream, temp16, error)) then
        deallocate(composite%components)
        return
      end if
      composite%components(component_count)%glyph_index = temp16
      
      ! Read arguments (offsets or point indices)
      if (iand(flags_int, mask_words) /= 0) then
        if (.not. ft_stream_read_short(stream, composite%components(component_count)%arg1, error)) then
          deallocate(composite%components)
          return
        end if
        if (.not. ft_stream_read_short(stream, composite%components(component_count)%arg2, error)) then
          deallocate(composite%components)
          return
        end if
      else
        if (.not. ft_stream_read_byte(stream, temp8, error)) then
          deallocate(composite%components)
          return
        end if
        composite%components(component_count)%arg1 = int(temp8, FT_Short)
        if (.not. ft_stream_read_byte(stream, temp8, error)) then
          deallocate(composite%components)
          return
        end if
        composite%components(component_count)%arg2 = int(temp8, FT_Short)
      end if
      
      ! Initialize transformation matrix to identity
      composite%components(component_count)%xx = 1.0
      composite%components(component_count)%xy = 0.0
      composite%components(component_count)%yx = 0.0
      composite%components(component_count)%yy = 1.0
      
      ! Read transformation matrix if present
      if (iand(flags_int, mask_scale) /= 0) then
        if (.not. ft_stream_read_short(stream, temp16, error)) then
          deallocate(composite%components)
          return
        end if
        ! Convert from F2Dot14 to float
        composite%components(component_count)%xx = real(temp16) / 16384.0
        composite%components(component_count)%yy = composite%components(component_count)%xx
      else if (iand(flags_int, mask_xy_scale) /= 0) then
        if (.not. ft_stream_read_short(stream, temp16, error)) then
          deallocate(composite%components)
          return
        end if
        composite%components(component_count)%xx = real(temp16) / 16384.0
        if (.not. ft_stream_read_short(stream, temp16, error)) then
          deallocate(composite%components)
          return
        end if
        composite%components(component_count)%yy = real(temp16) / 16384.0
      else if (iand(flags_int, mask_2x2) /= 0) then
        if (.not. ft_stream_read_short(stream, temp16, error)) then
          deallocate(composite%components)
          return
        end if
        composite%components(component_count)%xx = real(temp16) / 16384.0
        if (.not. ft_stream_read_short(stream, temp16, error)) then
          deallocate(composite%components)
          return
        end if
        composite%components(component_count)%xy = real(temp16) / 16384.0
        if (.not. ft_stream_read_short(stream, temp16, error)) then
          deallocate(composite%components)
          return
        end if
        composite%components(component_count)%yx = real(temp16) / 16384.0
        if (.not. ft_stream_read_short(stream, temp16, error)) then
          deallocate(composite%components)
          return
        end if
        composite%components(component_count)%yy = real(temp16) / 16384.0
      end if
      
      more_components = (iand(flags_int, mask_more) /= 0)
    end do
    
    ! Store the actual component count
    composite%num_components = component_count
    
    ! For now, still create empty glyph but store composite info for future enhancement
    ! TODO: Implement actual composite glyph resolution
    glyph%num_points = 0
    glyph%header%num_contours = 0
    glyph%header%x_min = 0
    glyph%header%y_min = 0
    glyph%header%x_max = 0
    glyph%header%y_max = 0
    
    ! Clean up composite data (not needed for current simple implementation)
    if (allocated(composite%components)) then
      deallocate(composite%components)
    end if
    
    success = .true.
    
  end function tt_load_composite_glyph

  ! Convert TrueType glyph to FT_Outline
  function tt_glyph_to_outline(glyph, outline, error) result(success)
    use ft_outline_mod, only: FT_Outline, ft_outline_new, ft_outline_done, FT_CURVE_TAG_ON
    use ft_geometry, only: FT_Vector
    type(TT_Simple_Glyph), intent(in) :: glyph
    type(FT_Outline), intent(out) :: outline
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: i
    
    success = .false.
    error = FT_Err_Ok
    
    ! Handle empty glyph
    if (glyph%num_points == 0 .or. glyph%header%num_contours == 0) then
      success = ft_outline_new(0, 0, outline, error)
      return
    end if
    
    ! Create outline with glyph points and contours
    success = ft_outline_new(int(glyph%num_points), int(glyph%header%num_contours), outline, error)
    if (.not. success) return
    
    ! Copy points
    do i = 1, glyph%num_points
      outline%points(i)%x = glyph%x_coordinates(i)
      outline%points(i)%y = glyph%y_coordinates(i)
      
      ! Set point tags (on-curve vs off-curve)
      if (glyph%on_curve(i)) then
        outline%tags(i) = FT_CURVE_TAG_ON
      else
        outline%tags(i) = 0  ! Off-curve (control point)
      end if
    end do
    
    ! Copy contour end points
    do i = 1, glyph%header%num_contours
      outline%contours(i) = glyph%end_pts_of_contours(i)
    end do
    
    ! Set outline flags
    outline%flags = 0  ! Default flags
    
    success = .true.
    
  end function tt_glyph_to_outline

  ! Wrapper function for backward compatibility
  function tt_load_glyph_by_index(stream, loca_table, glyph_index, glyph, error) result(success)
    use tt_loca, only: TT_Loca_Table
    type(FT_Stream_Type), intent(inout) :: stream
    type(TT_Loca_Table), intent(in) :: loca_table
    integer, intent(in) :: glyph_index
    type(TT_Simple_Glyph), intent(out) :: glyph
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    ! For backward compatibility, assume glyf offset is 0
    ! This will need to be fixed in callers
    success = tt_load_glyph_by_index_with_offset(stream, loca_table, glyph_index, 0_c_size_t, glyph, error)
    
  end function tt_load_glyph_by_index

end module tt_glyph