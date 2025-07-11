! CFF CharString interpreter module
! This module implements the Type 2 CharString interpreter for CFF fonts
module ft_cff_charstring
  use ft_types, only: FT_Error, FT_Err_Ok, FT_Err_Invalid_Argument, FT_Err_Invalid_File_Format
  use ft_outline_mod, only: FT_Outline, ft_outline_new, ft_outline_done, &
                           FT_CURVE_TAG_ON, FT_CURVE_TAG_CUBIC
  use ft_geometry, only: FT_Vector
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, real32, real64
  implicit none
  private
  
  ! Public types
  public :: FT_CFF_CharString_Context
  
  ! Public functions
  public :: ft_cff_charstring_init
  public :: ft_cff_charstring_done
  public :: ft_cff_charstring_parse
  public :: ft_cff_charstring_to_outline
  public :: ft_cff_charstring_to_outline_with_subr
  
  ! Type 2 CharString operators
  integer, parameter :: CS_HSTEM = 1
  integer, parameter :: CS_VSTEM = 3
  integer, parameter :: CS_VMOVETO = 4
  integer, parameter :: CS_RLINETO = 5
  integer, parameter :: CS_HLINETO = 6
  integer, parameter :: CS_VLINETO = 7
  integer, parameter :: CS_RRCURVETO = 8
  integer, parameter :: CS_CALLSUBR = 10
  integer, parameter :: CS_RETURN = 11
  integer, parameter :: CS_ESCAPE = 12
  integer, parameter :: CS_ENDCHAR = 14
  integer, parameter :: CS_HSTEMHM = 18
  integer, parameter :: CS_HINTMASK = 19
  integer, parameter :: CS_CNTRMASK = 20
  integer, parameter :: CS_RMOVETO = 21
  integer, parameter :: CS_HMOVETO = 22
  integer, parameter :: CS_VSTEMHM = 23
  integer, parameter :: CS_RCURVELINE = 24
  integer, parameter :: CS_RLINECURVE = 25
  integer, parameter :: CS_VVCURVETO = 26
  integer, parameter :: CS_HHCURVETO = 27
  integer, parameter :: CS_SHORTINT = 28
  integer, parameter :: CS_CALLGSUBR = 29
  integer, parameter :: CS_VHCURVETO = 30
  integer, parameter :: CS_HVCURVETO = 31
  
  ! Two-byte operators (escape + second byte)
  integer, parameter :: CS_DOTSECTION = 1200  ! 12 0
  integer, parameter :: CS_VSTEM3 = 1201      ! 12 1
  integer, parameter :: CS_HSTEM3 = 1202      ! 12 2
  integer, parameter :: CS_AND = 1203         ! 12 3
  integer, parameter :: CS_OR = 1204          ! 12 4
  integer, parameter :: CS_NOT = 1205         ! 12 5
  integer, parameter :: CS_ABS = 1209         ! 12 9
  integer, parameter :: CS_ADD = 1210         ! 12 10
  integer, parameter :: CS_SUB = 1211         ! 12 11
  integer, parameter :: CS_DIV = 1212         ! 12 12
  integer, parameter :: CS_NEG = 1214         ! 12 14
  integer, parameter :: CS_EQ = 1215          ! 12 15
  integer, parameter :: CS_DROP = 1218        ! 12 18
  integer, parameter :: CS_PUT = 1220         ! 12 20
  integer, parameter :: CS_GET = 1221         ! 12 21
  integer, parameter :: CS_IFELSE = 1222      ! 12 22
  integer, parameter :: CS_RANDOM = 1223      ! 12 23
  integer, parameter :: CS_MUL = 1224         ! 12 24
  integer, parameter :: CS_SQRT = 1226        ! 12 26
  integer, parameter :: CS_DUP = 1227         ! 12 27
  integer, parameter :: CS_EXCH = 1228        ! 12 28
  integer, parameter :: CS_INDEX = 1229       ! 12 29
  integer, parameter :: CS_ROLL = 1230        ! 12 30
  integer, parameter :: CS_HFLEX = 1234       ! 12 34
  integer, parameter :: CS_FLEX = 1235        ! 12 35
  integer, parameter :: CS_HFLEX1 = 1236      ! 12 36
  integer, parameter :: CS_FLEX1 = 1237       ! 12 37
  
  ! Subroutine constants
  integer, parameter :: MAX_CALL_DEPTH = 10
  
  ! Call stack frame for subroutines
  type :: CharString_Call_Frame
    character(len=1), allocatable :: charstring(:)
    integer :: charstring_length
    integer :: position
  end type CharString_Call_Frame
  
  ! CharString interpreter context
  type :: FT_CFF_CharString_Context
    ! Stack for operands
    real(real32), allocatable :: stack(:)
    integer :: stack_top = 0
    integer :: stack_size = 256
    
    ! Current position
    real(real32) :: x = 0.0
    real(real32) :: y = 0.0
    
    ! Path construction
    logical :: path_open = .false.
    integer :: num_points = 0
    
    ! Hint information
    integer :: num_hints = 0
    integer :: hint_mask_bytes = 0
    
    ! Width information
    real(real32) :: width = 0.0
    logical :: width_parsed = .false.
    
    ! Transient array for temporary storage
    real(real32), allocatable :: transient(:)
    integer :: transient_size = 32
    
    ! Random number state
    integer :: random_seed = 1
    
    ! Subroutine support
    integer :: call_depth = 0
    type(CharString_Call_Frame) :: call_stack(MAX_CALL_DEPTH)
    
  end type FT_CFF_CharString_Context
  
contains

  ! Initialize CharString context
  function ft_cff_charstring_init(context, error) result(success)
    type(FT_CFF_CharString_Context), intent(out) :: context
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    success = .false.
    error = FT_Err_Ok
    
    ! Allocate stack
    allocate(context%stack(context%stack_size))
    context%stack = 0.0
    context%stack_top = 0
    
    ! Allocate transient array
    allocate(context%transient(context%transient_size))
    context%transient = 0.0
    
    ! Initialize position
    context%x = 0.0
    context%y = 0.0
    
    ! Initialize state
    context%path_open = .false.
    context%num_points = 0
    context%num_hints = 0
    context%hint_mask_bytes = 0
    context%width = 0.0
    context%width_parsed = .false.
    context%random_seed = 1
    context%call_depth = 0
    
    success = .true.
    
  end function ft_cff_charstring_init

  ! Clean up CharString context
  subroutine ft_cff_charstring_done(context)
    type(FT_CFF_CharString_Context), intent(inout) :: context
    
    if (allocated(context%stack)) deallocate(context%stack)
    if (allocated(context%transient)) deallocate(context%transient)
    
    ! Clean up call stack
    call cleanup_call_stack(context)
    
    context%stack_top = 0
    context%x = 0.0
    context%y = 0.0
    context%path_open = .false.
    context%num_points = 0
    
  end subroutine ft_cff_charstring_done

  ! Parse CharString data
  function ft_cff_charstring_parse(context, charstring, length, outline, error) result(success)
    type(FT_CFF_CharString_Context), intent(inout) :: context
    character(len=1), intent(in) :: charstring(:)
    integer, intent(in) :: length
    type(FT_Outline), intent(inout) :: outline
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: pos, operator
    real(real32) :: operand
    
    success = .false.
    error = FT_Err_Ok
    
    pos = 1
    
    ! Main parsing loop
    do while (pos <= length)
      if (is_operator(charstring(pos))) then
        ! Read operator
        operator = read_operator(charstring, pos, length)
        if (operator == -1) then
          error = FT_Err_Invalid_File_Format
          return
        end if
        
        ! Execute operator
        if (.not. execute_operator(context, operator, outline, error)) then
          return
        end if
        
        ! Clear stack after operator (except for special cases)
        if (operator /= CS_HINTMASK .and. operator /= CS_CNTRMASK) then
          context%stack_top = 0
        end if
      else
        ! Read operand
        operand = read_operand(charstring, pos, length)
        
        ! Push onto stack
        if (.not. push_stack(context, operand)) then
          error = FT_Err_Invalid_Argument
          return
        end if
      end if
      
      ! Check for end of CharString
      if (operator == CS_ENDCHAR) exit
    end do
    
    success = .true.
    
  end function ft_cff_charstring_parse

  ! Convert CharString to outline
  function ft_cff_charstring_to_outline(charstring, length, outline, error) result(success)
    character(len=1), intent(in) :: charstring(:)
    integer, intent(in) :: length
    type(FT_Outline), intent(out) :: outline
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    type(FT_CFF_CharString_Context) :: context
    
    success = .false.
    error = FT_Err_Ok
    
    ! Initialize context
    if (.not. ft_cff_charstring_init(context, error)) then
      return
    end if
    
    ! Count points first (simplified - assume max points)
    ! In a real implementation, we'd pre-scan to count
    if (.not. ft_outline_new(1000, 100, outline, error)) then
      call ft_cff_charstring_done(context)
      return
    end if
    
    ! Initialize outline counters
    outline%n_points = 0
    outline%n_contours = 0
    
    ! Parse CharString
    if (.not. ft_cff_charstring_parse(context, charstring, length, outline, error)) then
      call ft_cff_charstring_done(context)
      call ft_outline_done(outline)
      return
    end if
    
    ! Clean up
    call ft_cff_charstring_done(context)
    
    success = .true.
    
  end function ft_cff_charstring_to_outline

  ! Check if byte is an operator
  function is_operator(byte) result(is_op)
    character(len=1), intent(in) :: byte
    logical :: is_op
    
    integer :: val
    
    val = ichar(byte)
    is_op = (val >= 0 .and. val <= 31) .and. val /= 28
    
  end function is_operator

  ! Read operator from CharString
  function read_operator(charstring, pos, length) result(operator)
    character(len=1), intent(in) :: charstring(:)
    integer, intent(inout) :: pos
    integer, intent(in) :: length
    integer :: operator
    
    integer :: b0, b1
    
    if (pos > length) then
      operator = -1
      return
    end if
    
    b0 = ichar(charstring(pos))
    pos = pos + 1
    
    if (b0 == CS_ESCAPE) then
      ! Two-byte operator
      if (pos > length) then
        operator = -1
        return
      end if
      
      b1 = ichar(charstring(pos))
      pos = pos + 1
      operator = 1200 + b1
    else
      ! Single-byte operator
      operator = b0
    end if
    
  end function read_operator

  ! Read operand from CharString
  function read_operand(charstring, pos, length) result(operand)
    character(len=1), intent(in) :: charstring(:)
    integer, intent(inout) :: pos
    integer, intent(in) :: length
    real(real32) :: operand
    
    integer :: b0, b1, b2, b3, b4
    integer :: int_val
    
    if (pos > length) then
      operand = 0.0
      return
    end if
    
    b0 = ichar(charstring(pos))
    pos = pos + 1
    
    if (b0 >= 32 .and. b0 <= 246) then
      ! Small integer: -107 to +107
      operand = real(b0 - 139)
    else if (b0 >= 247 .and. b0 <= 250) then
      ! Positive integer: +108 to +1131
      if (pos > length) then
        operand = 0.0
        return
      end if
      b1 = ichar(charstring(pos))
      pos = pos + 1
      operand = real((b0 - 247) * 256 + b1 + 108)
    else if (b0 >= 251 .and. b0 <= 254) then
      ! Negative integer: -1131 to -108
      if (pos > length) then
        operand = 0.0
        return
      end if
      b1 = ichar(charstring(pos))
      pos = pos + 1
      operand = real(-(b0 - 251) * 256 - b1 - 108)
    else if (b0 == 28) then
      ! Short integer (3 bytes)
      if (pos + 1 > length) then
        operand = 0.0
        return
      end if
      b1 = ichar(charstring(pos))
      b2 = ichar(charstring(pos + 1))
      pos = pos + 2
      
      ! Sign extend from 16-bit
      int_val = b1 * 256 + b2
      if (int_val >= 32768) int_val = int_val - 65536
      operand = real(int_val)
    else if (b0 == 255) then
      ! Fixed point (5 bytes)
      if (pos + 3 > length) then
        operand = 0.0
        return
      end if
      b1 = ichar(charstring(pos))
      b2 = ichar(charstring(pos + 1))
      b3 = ichar(charstring(pos + 2))
      b4 = ichar(charstring(pos + 3))
      pos = pos + 4
      
      ! Interpret as 16.16 fixed point
      int_val = b1 * 16777216 + b2 * 65536 + b3 * 256 + b4
      ! Check for negative (sign bit set)
      if (b1 >= 128) then
        int_val = int_val - int(z'100000000', int32)
      end if
      operand = real(int_val) / 65536.0
    else
      ! Invalid operand
      operand = 0.0
    end if
    
  end function read_operand

  ! Push value onto stack
  function push_stack(context, value) result(success)
    type(FT_CFF_CharString_Context), intent(inout) :: context
    real(real32), intent(in) :: value
    logical :: success
    
    success = .false.
    
    if (context%stack_top >= context%stack_size) return
    
    context%stack_top = context%stack_top + 1
    context%stack(context%stack_top) = value
    
    success = .true.
    
  end function push_stack

  ! Pop value from stack
  function pop_stack(context, value) result(success)
    type(FT_CFF_CharString_Context), intent(inout) :: context
    real(real32), intent(out) :: value
    logical :: success
    
    success = .false.
    value = 0.0
    
    if (context%stack_top <= 0) return
    
    value = context%stack(context%stack_top)
    context%stack_top = context%stack_top - 1
    
    success = .true.
    
  end function pop_stack

  ! Execute CharString operator
  function execute_operator(context, operator, outline, error) result(success)
    type(FT_CFF_CharString_Context), intent(inout) :: context
    integer, intent(in) :: operator
    type(FT_Outline), intent(inout) :: outline
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    success = .false.
    error = FT_Err_Ok
    
    select case (operator)
    case (CS_RMOVETO)
      success = op_rmoveto(context, outline)
    case (CS_HMOVETO)
      success = op_hmoveto(context, outline)
    case (CS_VMOVETO)
      success = op_vmoveto(context, outline)
    case (CS_RLINETO)
      success = op_rlineto(context, outline)
    case (CS_HLINETO)
      success = op_hlineto(context, outline)
    case (CS_VLINETO)
      success = op_vlineto(context, outline)
    case (CS_RRCURVETO)
      success = op_rrcurveto(context, outline)
    case (CS_ENDCHAR)
      success = op_endchar(context, outline)
    case (CS_HSTEM, CS_VSTEM, CS_HSTEMHM, CS_VSTEMHM)
      success = op_stem(context)
    case (CS_CALLSUBR)
      success = op_callsubr(context, outline, error)
    case (CS_CALLGSUBR)
      success = op_callgsubr(context, outline, error)
    case (CS_RETURN)
      success = op_return(context, outline, error)
    case default
      ! Unimplemented operator - for now just clear stack and continue
      context%stack_top = 0
      success = .true.
    end select
    
    if (.not. success) error = FT_Err_Invalid_Argument
    
  end function execute_operator

  ! Operator: rmoveto - relative move
  function op_rmoveto(context, outline) result(success)
    type(FT_CFF_CharString_Context), intent(inout) :: context
    type(FT_Outline), intent(inout) :: outline
    logical :: success
    
    real(real32) :: dx, dy
    
    success = .false.
    
    ! Check for width
    if (context%stack_top > 2 .and. .not. context%width_parsed) then
      context%width = context%stack(1)
      context%width_parsed = .true.
      ! Shift stack
      context%stack(1:context%stack_top-1) = context%stack(2:context%stack_top)
      context%stack_top = context%stack_top - 1
    end if
    
    if (context%stack_top < 2) return
    
    dx = context%stack(context%stack_top - 1)
    dy = context%stack(context%stack_top)
    
    context%x = context%x + dx
    context%y = context%y + dy
    
    ! Start new contour if needed
    if (context%path_open) then
      call close_outline_contour(outline)
    end if
    
    context%path_open = .true.
    success = .true.
    
  end function op_rmoveto

  ! Operator: hmoveto - horizontal move
  function op_hmoveto(context, outline) result(success)
    type(FT_CFF_CharString_Context), intent(inout) :: context
    type(FT_Outline), intent(inout) :: outline
    logical :: success
    
    real(real32) :: dx
    
    success = .false.
    
    ! Check for width
    if (context%stack_top > 1 .and. .not. context%width_parsed) then
      context%width = context%stack(1)
      context%width_parsed = .true.
      ! Shift stack
      context%stack(1:context%stack_top-1) = context%stack(2:context%stack_top)
      context%stack_top = context%stack_top - 1
    end if
    
    if (context%stack_top < 1) return
    
    dx = context%stack(context%stack_top)
    
    context%x = context%x + dx
    
    ! Start new contour if needed
    if (context%path_open) then
      call close_outline_contour(outline)
    end if
    
    context%path_open = .true.
    success = .true.
    
  end function op_hmoveto

  ! Operator: vmoveto - vertical move
  function op_vmoveto(context, outline) result(success)
    type(FT_CFF_CharString_Context), intent(inout) :: context
    type(FT_Outline), intent(inout) :: outline
    logical :: success
    
    real(real32) :: dy
    
    success = .false.
    
    ! Check for width
    if (context%stack_top > 1 .and. .not. context%width_parsed) then
      context%width = context%stack(1)
      context%width_parsed = .true.
      ! Shift stack
      context%stack(1:context%stack_top-1) = context%stack(2:context%stack_top)
      context%stack_top = context%stack_top - 1
    end if
    
    if (context%stack_top < 1) return
    
    dy = context%stack(context%stack_top)
    
    context%y = context%y + dy
    
    ! Start new contour if needed
    if (context%path_open) then
      call close_outline_contour(outline)
    end if
    
    context%path_open = .true.
    success = .true.
    
  end function op_vmoveto

  ! Operator: rlineto - relative line
  function op_rlineto(context, outline) result(success)
    type(FT_CFF_CharString_Context), intent(inout) :: context
    type(FT_Outline), intent(inout) :: outline
    logical :: success
    
    integer :: i
    real(real32) :: dx, dy
    type(FT_Vector) :: point
    
    success = .false.
    
    if (context%stack_top < 2 .or. mod(context%stack_top, 2) /= 0) return
    
    ! Process line segments
    do i = 1, context%stack_top, 2
      dx = context%stack(i)
      dy = context%stack(i + 1)
      
      context%x = context%x + dx
      context%y = context%y + dy
      
      point%x = nint(context%x * 64.0)  ! Convert to 26.6 format
      point%y = nint(context%y * 64.0)
      
      call add_outline_point(outline, point, FT_CURVE_TAG_ON)
    end do
    
    success = .true.
    
  end function op_rlineto

  ! Operator: hlineto - horizontal line
  function op_hlineto(context, outline) result(success)
    type(FT_CFF_CharString_Context), intent(inout) :: context
    type(FT_Outline), intent(inout) :: outline
    logical :: success
    
    integer :: i
    real(real32) :: delta
    type(FT_Vector) :: point
    logical :: is_horizontal
    
    success = .false.
    
    if (context%stack_top < 1) return
    
    is_horizontal = .true.
    
    ! Process alternating horizontal/vertical lines
    do i = 1, context%stack_top
      delta = context%stack(i)
      
      if (is_horizontal) then
        context%x = context%x + delta
      else
        context%y = context%y + delta
      end if
      
      point%x = nint(context%x * 64.0)
      point%y = nint(context%y * 64.0)
      
      call add_outline_point(outline, point, FT_CURVE_TAG_ON)
      
      is_horizontal = .not. is_horizontal
    end do
    
    success = .true.
    
  end function op_hlineto

  ! Operator: vlineto - vertical line
  function op_vlineto(context, outline) result(success)
    type(FT_CFF_CharString_Context), intent(inout) :: context
    type(FT_Outline), intent(inout) :: outline
    logical :: success
    
    integer :: i
    real(real32) :: delta
    type(FT_Vector) :: point
    logical :: is_vertical
    
    success = .false.
    
    if (context%stack_top < 1) return
    
    is_vertical = .true.
    
    ! Process alternating vertical/horizontal lines
    do i = 1, context%stack_top
      delta = context%stack(i)
      
      if (is_vertical) then
        context%y = context%y + delta
      else
        context%x = context%x + delta
      end if
      
      point%x = nint(context%x * 64.0)
      point%y = nint(context%y * 64.0)
      
      call add_outline_point(outline, point, FT_CURVE_TAG_ON)
      
      is_vertical = .not. is_vertical
    end do
    
    success = .true.
    
  end function op_vlineto

  ! Operator: rrcurveto - relative curve
  function op_rrcurveto(context, outline) result(success)
    type(FT_CFF_CharString_Context), intent(inout) :: context
    type(FT_Outline), intent(inout) :: outline
    logical :: success
    
    integer :: i
    real(real32) :: dx1, dy1, dx2, dy2, dx3, dy3
    type(FT_Vector) :: cp1, cp2, p3
    
    success = .false.
    
    if (context%stack_top < 6 .or. mod(context%stack_top, 6) /= 0) return
    
    ! Process Bezier curves
    do i = 1, context%stack_top, 6
      dx1 = context%stack(i)
      dy1 = context%stack(i + 1)
      dx2 = context%stack(i + 2)
      dy2 = context%stack(i + 3)
      dx3 = context%stack(i + 4)
      dy3 = context%stack(i + 5)
      
      ! First control point
      cp1%x = nint((context%x + dx1) * 64.0)
      cp1%y = nint((context%y + dy1) * 64.0)
      
      ! Second control point
      cp2%x = nint((context%x + dx1 + dx2) * 64.0)
      cp2%y = nint((context%y + dy1 + dy2) * 64.0)
      
      ! End point
      context%x = context%x + dx1 + dx2 + dx3
      context%y = context%y + dy1 + dy2 + dy3
      p3%x = nint(context%x * 64.0)
      p3%y = nint(context%y * 64.0)
      
      ! Add Bezier curve
      call add_outline_point(outline, cp1, FT_CURVE_TAG_CUBIC)
      call add_outline_point(outline, cp2, FT_CURVE_TAG_CUBIC)
      call add_outline_point(outline, p3, FT_CURVE_TAG_ON)
    end do
    
    success = .true.
    
  end function op_rrcurveto

  ! Operator: endchar - end character
  function op_endchar(context, outline) result(success)
    type(FT_CFF_CharString_Context), intent(inout) :: context
    type(FT_Outline), intent(inout) :: outline
    logical :: success
    
    ! Check for width
    if (context%stack_top > 0 .and. .not. context%width_parsed) then
      context%width = context%stack(1)
      context%width_parsed = .true.
    end if
    
    ! Close any open contour
    if (context%path_open) then
      call close_outline_contour(outline)
      context%path_open = .false.
    end if
    
    success = .true.
    
  end function op_endchar

  ! Operator: stem hints
  function op_stem(context) result(success)
    type(FT_CFF_CharString_Context), intent(inout) :: context
    logical :: success
    
    integer :: num_args
    
    success = .false.
    
    ! Check for width
    if (mod(context%stack_top, 2) /= 0 .and. .not. context%width_parsed) then
      context%width = context%stack(1)
      context%width_parsed = .true.
      ! Shift stack
      context%stack(1:context%stack_top-1) = context%stack(2:context%stack_top)
      context%stack_top = context%stack_top - 1
    end if
    
    ! Count hint pairs
    num_args = context%stack_top / 2
    context%num_hints = context%num_hints + num_args
    
    ! Calculate hint mask bytes
    context%hint_mask_bytes = (context%num_hints + 7) / 8
    
    success = .true.
    
  end function op_stem

  ! Helper: Add point to outline
  subroutine add_outline_point(outline, point, tag)
    type(FT_Outline), intent(inout) :: outline
    type(FT_Vector), intent(in) :: point
    integer(int8), intent(in) :: tag
    
    if (outline%n_points >= size(outline%points)) return
    
    outline%n_points = outline%n_points + 1
    outline%points(outline%n_points) = point
    outline%tags(outline%n_points) = tag
    
  end subroutine add_outline_point

  ! Helper: Close current contour
  subroutine close_outline_contour(outline)
    type(FT_Outline), intent(inout) :: outline
    
    if (outline%n_points == 0) return
    if (outline%n_contours >= size(outline%contours)) return
    
    outline%n_contours = outline%n_contours + 1
    outline%contours(outline%n_contours) = outline%n_points - 1
    
  end subroutine close_outline_contour

  ! Helper: Clean up call stack
  subroutine cleanup_call_stack(context)
    type(FT_CFF_CharString_Context), intent(inout) :: context
    integer :: i
    
    do i = 1, context%call_depth
      if (allocated(context%call_stack(i)%charstring)) then
        deallocate(context%call_stack(i)%charstring)
      end if
    end do
    
    context%call_depth = 0
    
  end subroutine cleanup_call_stack

  ! Operator: callsubr - call local subroutine
  function op_callsubr(context, outline, error) result(success)
    type(FT_CFF_CharString_Context), intent(inout) :: context
    type(FT_Outline), intent(inout) :: outline
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    success = .false.
    error = FT_Err_Ok
    
    ! For now, just clear the stack (subroutine not implemented)
    ! In a full implementation, we would:
    ! 1. Pop subroutine index from stack
    ! 2. Get subroutine data from local subroutines
    ! 3. Push current position onto call stack
    ! 4. Execute subroutine
    
    if (context%stack_top > 0) then
      context%stack_top = context%stack_top - 1
    end if
    
    success = .true.
    
  end function op_callsubr

  ! Operator: callgsubr - call global subroutine
  function op_callgsubr(context, outline, error) result(success)
    type(FT_CFF_CharString_Context), intent(inout) :: context
    type(FT_Outline), intent(inout) :: outline
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    success = .false.
    error = FT_Err_Ok
    
    ! For now, just clear the stack (subroutine not implemented)
    ! In a full implementation, we would:
    ! 1. Pop subroutine index from stack
    ! 2. Get subroutine data from global subroutines
    ! 3. Push current position onto call stack
    ! 4. Execute subroutine
    
    if (context%stack_top > 0) then
      context%stack_top = context%stack_top - 1
    end if
    
    success = .true.
    
  end function op_callgsubr

  ! Operator: return - return from subroutine
  function op_return(context, outline, error) result(success)
    type(FT_CFF_CharString_Context), intent(inout) :: context
    type(FT_Outline), intent(inout) :: outline
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    success = .false.
    error = FT_Err_Ok
    
    ! For now, just succeed (return not implemented)
    ! In a full implementation, we would:
    ! 1. Pop position from call stack
    ! 2. Continue execution from that position
    
    success = .true.
    
  end function op_return

  ! Convert CharString to outline with subroutine support
  function ft_cff_charstring_to_outline_with_subr(charstring, length, global_subr, local_subr, outline, error) result(success)
    character(len=1), intent(in) :: charstring(:)
    integer, intent(in) :: length
    character(len=1), intent(in), optional :: global_subr(:)
    character(len=1), intent(in), optional :: local_subr(:)
    type(FT_Outline), intent(out) :: outline
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    type(FT_CFF_CharString_Context) :: context
    
    success = .false.
    error = FT_Err_Ok
    
    ! Initialize context
    if (.not. ft_cff_charstring_init(context, error)) then
      return
    end if
    
    ! Store subroutine data (for future implementation)
    ! For now, just use the regular parsing
    
    ! Count points first (simplified - assume max points)
    if (.not. ft_outline_new(1000, 100, outline, error)) then
      call ft_cff_charstring_done(context)
      return
    end if
    
    ! Initialize outline counters
    outline%n_points = 0
    outline%n_contours = 0
    
    ! Parse CharString
    if (.not. ft_cff_charstring_parse(context, charstring, length, outline, error)) then
      call ft_cff_charstring_done(context)
      call ft_outline_done(outline)
      return
    end if
    
    ! Clean up
    call ft_cff_charstring_done(context)
    
    success = .true.
    
  end function ft_cff_charstring_to_outline_with_subr

end module ft_cff_charstring