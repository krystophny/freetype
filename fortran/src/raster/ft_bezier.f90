module ft_bezier
  use ft_types
  use ft_geometry, only: FT_Vector
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, real32
  implicit none
  private
  
  ! Public functions
  public :: ft_bezier_flatten_conic
  public :: ft_bezier_flatten_cubic
  public :: ft_bezier_split_conic
  public :: ft_bezier_split_cubic
  
  ! Maximum recursion depth for subdivision
  integer, parameter :: MAX_DEPTH = 16
  
  ! Flatness tolerance (in 26.6 fixed point units)
  integer, parameter :: FLATNESS_TOLERANCE = 16  ! ~0.25 pixels
  
contains

  ! Flatten a conic Bezier curve into line segments
  subroutine ft_bezier_flatten_conic(p0, p1, p2, callback, user_data)
    type(FT_Vector), intent(in) :: p0, p1, p2  ! Start, control, end points
    interface
      subroutine callback(from, to, user_data)
        import :: FT_Vector, c_ptr
        type(FT_Vector), intent(in) :: from, to
        type(c_ptr), intent(in) :: user_data
      end subroutine callback
    end interface
    type(c_ptr), intent(in) :: user_data
    
    call subdivide_conic(p0, p1, p2, callback, user_data, 0)
    
  end subroutine ft_bezier_flatten_conic
  
  ! Recursive subdivision of conic Bezier
  recursive subroutine subdivide_conic(p0, p1, p2, callback, user_data, depth)
    type(FT_Vector), intent(in) :: p0, p1, p2
    interface
      subroutine callback(from, to, user_data)
        import :: FT_Vector, c_ptr
        type(FT_Vector), intent(in) :: from, to
        type(c_ptr), intent(in) :: user_data
      end subroutine callback
    end interface
    type(c_ptr), intent(in) :: user_data
    integer, intent(in) :: depth
    
    type(FT_Vector) :: mid01, mid12, midpoint
    integer(FT_Fixed) :: dx, dy, deviation
    
    ! Check recursion depth
    if (depth >= MAX_DEPTH) then
      call callback(p0, p2, user_data)
      return
    end if
    
    ! Check if curve is flat enough
    ! Compute deviation as distance from control point to line p0-p2
    dx = p2%x - p0%x
    dy = p2%y - p0%y
    
    ! Simple flatness test: check if control point is close to midpoint of p0-p2
    mid01%x = (p0%x + p2%x) / 2
    mid01%y = (p0%y + p2%y) / 2
    
    deviation = abs(p1%x - mid01%x) + abs(p1%y - mid01%y)
    
    if (deviation < FLATNESS_TOLERANCE) then
      ! Curve is flat enough, output line segment
      call callback(p0, p2, user_data)
    else
      ! Split curve at midpoint
      call ft_bezier_split_conic(p0, p1, p2, mid01, mid12, midpoint)
      
      ! Recursively flatten both halves
      call subdivide_conic(p0, mid01, midpoint, callback, user_data, depth + 1)
      call subdivide_conic(midpoint, mid12, p2, callback, user_data, depth + 1)
    end if
    
  end subroutine subdivide_conic
  
  ! Split a conic Bezier curve at t=0.5
  subroutine ft_bezier_split_conic(p0, p1, p2, q0, q1, r)
    type(FT_Vector), intent(in) :: p0, p1, p2
    type(FT_Vector), intent(out) :: q0, q1, r
    
    ! De Casteljau's algorithm for t=0.5
    ! First subdivision
    q0%x = (p0%x + p1%x) / 2
    q0%y = (p0%y + p1%y) / 2
    
    q1%x = (p1%x + p2%x) / 2
    q1%y = (p1%y + p2%y) / 2
    
    ! Midpoint
    r%x = (q0%x + q1%x) / 2
    r%y = (q0%y + q1%y) / 2
    
  end subroutine ft_bezier_split_conic
  
  ! Flatten a cubic Bezier curve into line segments
  subroutine ft_bezier_flatten_cubic(p0, p1, p2, p3, callback, user_data)
    type(FT_Vector), intent(in) :: p0, p1, p2, p3  ! Start, control1, control2, end
    interface
      subroutine callback(from, to, user_data)
        import :: FT_Vector, c_ptr
        type(FT_Vector), intent(in) :: from, to
        type(c_ptr), intent(in) :: user_data
      end subroutine callback
    end interface
    type(c_ptr), intent(in) :: user_data
    
    call subdivide_cubic(p0, p1, p2, p3, callback, user_data, 0)
    
  end subroutine ft_bezier_flatten_cubic
  
  ! Recursive subdivision of cubic Bezier
  recursive subroutine subdivide_cubic(p0, p1, p2, p3, callback, user_data, depth)
    type(FT_Vector), intent(in) :: p0, p1, p2, p3
    interface
      subroutine callback(from, to, user_data)
        import :: FT_Vector, c_ptr
        type(FT_Vector), intent(in) :: from, to
        type(c_ptr), intent(in) :: user_data
      end subroutine callback
    end interface
    type(c_ptr), intent(in) :: user_data
    integer, intent(in) :: depth
    
    type(FT_Vector) :: q0, q1, q2, r0, r1, s
    integer(FT_Fixed) :: deviation
    
    ! Check recursion depth
    if (depth >= MAX_DEPTH) then
      call callback(p0, p3, user_data)
      return
    end if
    
    ! Flatness test for cubic
    ! Check if control points are close to the line p0-p3
    deviation = 0
    deviation = deviation + point_line_distance(p1, p0, p3)
    deviation = deviation + point_line_distance(p2, p0, p3)
    
    if (deviation < FLATNESS_TOLERANCE) then
      ! Curve is flat enough
      call callback(p0, p3, user_data)
    else
      ! Split curve at midpoint
      call ft_bezier_split_cubic(p0, p1, p2, p3, q0, q1, q2, r0, r1, s)
      
      ! Recursively flatten both halves
      call subdivide_cubic(p0, q0, r0, s, callback, user_data, depth + 1)
      call subdivide_cubic(s, r1, q2, p3, callback, user_data, depth + 1)
    end if
    
  end subroutine subdivide_cubic
  
  ! Split a cubic Bezier curve at t=0.5
  subroutine ft_bezier_split_cubic(p0, p1, p2, p3, q0, q1, q2, r0, r1, s)
    type(FT_Vector), intent(in) :: p0, p1, p2, p3
    type(FT_Vector), intent(out) :: q0, q1, q2, r0, r1, s
    
    ! De Casteljau's algorithm for t=0.5
    ! First level
    q0%x = (p0%x + p1%x) / 2
    q0%y = (p0%y + p1%y) / 2
    
    q1%x = (p1%x + p2%x) / 2
    q1%y = (p1%y + p2%y) / 2
    
    q2%x = (p2%x + p3%x) / 2
    q2%y = (p2%y + p3%y) / 2
    
    ! Second level
    r0%x = (q0%x + q1%x) / 2
    r0%y = (q0%y + q1%y) / 2
    
    r1%x = (q1%x + q2%x) / 2
    r1%y = (q1%y + q2%y) / 2
    
    ! Third level (split point)
    s%x = (r0%x + r1%x) / 2
    s%y = (r0%y + r1%y) / 2
    
  end subroutine ft_bezier_split_cubic
  
  ! Compute approximate distance from point to line
  function point_line_distance(point, line_start, line_end) result(distance)
    type(FT_Vector), intent(in) :: point, line_start, line_end
    integer(FT_Fixed) :: distance
    
    integer(FT_Fixed) :: dx, dy, line_dx, line_dy
    integer(FT_Fixed) :: t, px, py
    
    ! Vector from line_start to line_end
    line_dx = line_end%x - line_start%x
    line_dy = line_end%y - line_start%y
    
    ! Vector from line_start to point
    dx = point%x - line_start%x
    dy = point%y - line_start%y
    
    ! Project point onto line (simplified)
    if (line_dx == 0 .and. line_dy == 0) then
      ! Degenerate line
      distance = abs(dx) + abs(dy)
    else
      ! Use Manhattan distance for simplicity
      ! This is an approximation but good enough for flatness testing
      t = (dx * line_dx + dy * line_dy) / (line_dx * line_dx + line_dy * line_dy)
      
      ! Clamp t to [0, 1]
      if (t < 0) t = 0
      if (t > 65536) t = 65536  ! 1.0 in 16.16 fixed
      
      ! Point on line closest to input point
      px = line_start%x + (t * line_dx) / 65536
      py = line_start%y + (t * line_dy) / 65536
      
      ! Manhattan distance
      distance = abs(point%x - px) + abs(point%y - py)
    end if
    
  end function point_line_distance

end module ft_bezier