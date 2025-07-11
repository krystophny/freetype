module ft_outline_flatten
  use ft_types
  use ft_geometry, only: FT_Vector
  use ft_outline_mod
  use ft_bezier
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  private
  
  ! Public functions
  public :: ft_outline_flatten_to_lines
  
  ! Line callback context
  type :: line_context
    procedure(line_callback), pointer, nopass :: callback => null()
    type(c_ptr) :: user_data = c_null_ptr
  end type line_context
  
  ! Line callback interface
  abstract interface
    subroutine line_callback(from, to, user_data)
      import :: FT_Vector, c_ptr
      type(FT_Vector), intent(in) :: from, to
      type(c_ptr), intent(in) :: user_data
    end subroutine line_callback
  end interface
  
contains

  ! Flatten outline to line segments only
  function ft_outline_flatten_to_lines(outline, callback, user_data, error) result(success)
    type(FT_Outline), intent(in) :: outline
    procedure(line_callback) :: callback
    type(c_ptr), intent(in) :: user_data
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: n, first, last, tag, i
    type(FT_Vector) :: v_start, v_current, v_control
    type(line_context), target :: ctx
    
    success = .false.
    error = FT_Err_Ok
    
    ! Set up context
    ctx%callback => callback
    ctx%user_data = user_data
    
    ! Process each contour
    first = 1
    do n = 1, outline%n_contours
      last = outline%contours(n) + 1  ! Convert to 1-based
      
      if (first > last) cycle
      
      ! Get start point
      v_start = outline%points(first)
      v_current = v_start
      
      ! Process points in contour
      i = first
      do while (i <= last)
        tag = iand(int(outline%tags(i)), 3)
        
        select case (tag)
        case (FT_CURVE_TAG_ON)
          ! Line to on-curve point
          if (i > first) then
            call callback(v_current, outline%points(i), user_data)
          end if
          v_current = outline%points(i)
          i = i + 1
          
        case (FT_CURVE_TAG_CONIC)
          ! Conic bezier
          v_control = outline%points(i)
          i = i + 1
          
          if (i <= last) then
            tag = iand(int(outline%tags(i)), 3)
            
            if (tag == FT_CURVE_TAG_ON) then
              ! Simple conic with explicit end point
              call flatten_conic(v_current, v_control, outline%points(i), ctx)
              v_current = outline%points(i)
              i = i + 1
            else if (tag == FT_CURVE_TAG_CONIC) then
              ! Two consecutive conics - create implicit on-point
              do while (i <= last .and. iand(int(outline%tags(i)), 3) == FT_CURVE_TAG_CONIC)
                v_control%x = (v_control%x + outline%points(i)%x) / 2
                v_control%y = (v_control%y + outline%points(i)%y) / 2
                
                call flatten_conic(v_current, v_control, v_control, ctx)
                v_current = v_control
                v_control = outline%points(i)
                i = i + 1
              end do
              
              ! Final segment
              if (i <= last) then
                call flatten_conic(v_current, v_control, outline%points(i), ctx)
                v_current = outline%points(i)
                i = i + 1
              else
                ! Close to start
                call flatten_conic(v_current, v_control, v_start, ctx)
                v_current = v_start
              end if
            end if
          else
            ! Close contour with conic
            call flatten_conic(v_current, v_control, v_start, ctx)
            v_current = v_start
          end if
          
        case (FT_CURVE_TAG_CUBIC)
          ! Cubic bezier - need two control points
          if (i + 2 <= last) then
            call flatten_cubic(v_current, outline%points(i), &
                             outline%points(i+1), outline%points(i+2), ctx)
            v_current = outline%points(i+2)
            i = i + 3
          else
            ! Invalid cubic, skip
            i = i + 1
          end if
          
        case default
          i = i + 1
          
        end select
      end do
      
      ! Close contour if needed
      if (v_current%x /= v_start%x .or. v_current%y /= v_start%y) then
        call callback(v_current, v_start, user_data)
      end if
      
      first = last + 1
    end do
    
    success = .true.
    
  end function ft_outline_flatten_to_lines
  
  ! Wrapper for conic flattening
  subroutine flatten_conic(p0, p1, p2, ctx)
    type(FT_Vector), intent(in) :: p0, p1, p2
    type(line_context), target, intent(in) :: ctx
    
    call ft_bezier_flatten_conic(p0, p1, p2, conic_callback, c_loc(ctx))
    
  end subroutine flatten_conic
  
  ! Wrapper for cubic flattening
  subroutine flatten_cubic(p0, p1, p2, p3, ctx)
    type(FT_Vector), intent(in) :: p0, p1, p2, p3
    type(line_context), target, intent(in) :: ctx
    
    call ft_bezier_flatten_cubic(p0, p1, p2, p3, cubic_callback, c_loc(ctx))
    
  end subroutine flatten_cubic
  
  ! Callback wrapper for bezier flattening
  subroutine conic_callback(from, to, user_data)
    type(FT_Vector), intent(in) :: from, to
    type(c_ptr), intent(in) :: user_data
    
    type(line_context), pointer :: ctx
    
    call c_f_pointer(user_data, ctx)
    if (associated(ctx%callback)) then
      call ctx%callback(from, to, ctx%user_data)
    end if
    
  end subroutine conic_callback
  
  ! Same callback for cubic (same signature)
  subroutine cubic_callback(from, to, user_data)
    type(FT_Vector), intent(in) :: from, to
    type(c_ptr), intent(in) :: user_data
    
    type(line_context), pointer :: ctx
    
    call c_f_pointer(user_data, ctx)
    if (associated(ctx%callback)) then
      call ctx%callback(from, to, ctx%user_data)
    end if
    
  end subroutine cubic_callback

end module ft_outline_flatten