module ft_geometry
  use ft_types, only: FT_Fixed, FT_Error, FT_FIXED_ONE
  use, intrinsic :: iso_c_binding
  implicit none
  private
  
  ! Public types
  public :: FT_Vector
  public :: FT_Matrix
  public :: FT_BBox
  
  ! Vector operations
  public :: ft_vector_add
  public :: ft_vector_sub
  public :: ft_vector_transform
  public :: ft_vector_length
  public :: ft_vector_dot
  
  ! Matrix operations
  public :: ft_matrix_multiply
  public :: ft_matrix_invert
  
  ! BBox operations
  public :: ft_bbox_union
  public :: ft_bbox_intersect
  
  ! Type definitions
  
  ! FT_Pos is used for vector coordinates (same as FT_Fixed)
  integer, parameter :: FT_Pos = FT_Fixed
  
  ! FT_Vector: 2D vector with x,y coordinates
  type, bind(C) :: FT_Vector
    integer(FT_Pos) :: x
    integer(FT_Pos) :: y
  end type FT_Vector
  
  ! FT_Matrix: 2x2 transformation matrix
  type, bind(C) :: FT_Matrix
    integer(FT_Fixed) :: xx, xy
    integer(FT_Fixed) :: yx, yy
  end type FT_Matrix
  
  ! FT_BBox: Bounding box with min/max coordinates
  type, bind(C) :: FT_BBox
    integer(FT_Pos) :: xMin, yMin
    integer(FT_Pos) :: xMax, yMax
  end type FT_BBox

contains

  ! Vector operations
  
  pure function ft_vector_add(a, b) result(c)
    type(FT_Vector), intent(in) :: a, b
    type(FT_Vector) :: c
    
    c%x = a%x + b%x
    c%y = a%y + b%y
  end function ft_vector_add
  
  pure function ft_vector_sub(a, b) result(c)
    type(FT_Vector), intent(in) :: a, b
    type(FT_Vector) :: c
    
    c%x = a%x - b%x
    c%y = a%y - b%y
  end function ft_vector_sub
  
  pure function ft_vector_transform(vec, matrix) result(result_vec)
    type(FT_Vector), intent(in) :: vec
    type(FT_Matrix), intent(in) :: matrix
    type(FT_Vector) :: result_vec
    integer(8) :: x, y
    
    ! Perform matrix multiplication with 64-bit intermediate
    x = int(matrix%xx, 8) * int(vec%x, 8) + int(matrix%xy, 8) * int(vec%y, 8)
    y = int(matrix%yx, 8) * int(vec%x, 8) + int(matrix%yy, 8) * int(vec%y, 8)
    
    ! Convert back to fixed-point
    result_vec%x = int(x / FT_FIXED_ONE, FT_Pos)
    result_vec%y = int(y / FT_FIXED_ONE, FT_Pos)
  end function ft_vector_transform
  
  pure function ft_vector_length(vec) result(length)
    type(FT_Vector), intent(in) :: vec
    integer(FT_Pos) :: length
    integer(8) :: x2, y2, sum
    
    ! Calculate squared length
    x2 = int(vec%x, 8) * int(vec%x, 8)
    y2 = int(vec%y, 8) * int(vec%y, 8)
    sum = x2 + y2
    
    ! Simple integer square root approximation
    ! For now, return the sum (squared length)
    ! TODO: Implement proper fixed-point square root
    length = int(sum / FT_FIXED_ONE, FT_Pos)
  end function ft_vector_length
  
  pure function ft_vector_dot(a, b) result(dot)
    type(FT_Vector), intent(in) :: a, b
    integer(FT_Pos) :: dot
    integer(8) :: temp
    
    temp = int(a%x, 8) * int(b%x, 8) + int(a%y, 8) * int(b%y, 8)
    dot = int(temp / FT_FIXED_ONE, FT_Pos)
  end function ft_vector_dot
  
  ! Matrix operations
  
  pure function ft_matrix_multiply(a, b) result(c)
    type(FT_Matrix), intent(in) :: a, b
    type(FT_Matrix) :: c
    integer(8) :: temp
    
    ! c = a * b
    temp = int(a%xx, 8) * int(b%xx, 8) + int(a%xy, 8) * int(b%yx, 8)
    c%xx = int(temp / FT_FIXED_ONE, FT_Fixed)
    
    temp = int(a%xx, 8) * int(b%xy, 8) + int(a%xy, 8) * int(b%yy, 8)
    c%xy = int(temp / FT_FIXED_ONE, FT_Fixed)
    
    temp = int(a%yx, 8) * int(b%xx, 8) + int(a%yy, 8) * int(b%yx, 8)
    c%yx = int(temp / FT_FIXED_ONE, FT_Fixed)
    
    temp = int(a%yx, 8) * int(b%xy, 8) + int(a%yy, 8) * int(b%yy, 8)
    c%yy = int(temp / FT_FIXED_ONE, FT_Fixed)
  end function ft_matrix_multiply
  
  subroutine ft_matrix_invert(matrix, inv, success)
    type(FT_Matrix), intent(in) :: matrix
    type(FT_Matrix), intent(out) :: inv
    logical, intent(out) :: success
    integer(8) :: det, temp
    
    ! Calculate determinant
    det = int(matrix%xx, 8) * int(matrix%yy, 8) - &
          int(matrix%xy, 8) * int(matrix%yx, 8)
    
    if (det == 0) then
      success = .false.
      inv = matrix  ! Return unchanged
      return
    end if
    
    success = .true.
    
    ! Calculate inverse
    inv%xx = int(int(matrix%yy, 8) * FT_FIXED_ONE * FT_FIXED_ONE / det, FT_Fixed)
    inv%xy = int(-int(matrix%xy, 8) * FT_FIXED_ONE * FT_FIXED_ONE / det, FT_Fixed)
    inv%yx = int(-int(matrix%yx, 8) * FT_FIXED_ONE * FT_FIXED_ONE / det, FT_Fixed)
    inv%yy = int(int(matrix%xx, 8) * FT_FIXED_ONE * FT_FIXED_ONE / det, FT_Fixed)
  end subroutine ft_matrix_invert
  
  ! BBox operations
  
  pure function ft_bbox_union(a, b) result(c)
    type(FT_BBox), intent(in) :: a, b
    type(FT_BBox) :: c
    
    c%xMin = min(a%xMin, b%xMin)
    c%yMin = min(a%yMin, b%yMin)
    c%xMax = max(a%xMax, b%xMax)
    c%yMax = max(a%yMax, b%yMax)
  end function ft_bbox_union
  
  pure function ft_bbox_intersect(a, b) result(c)
    type(FT_BBox), intent(in) :: a, b
    type(FT_BBox) :: c
    
    c%xMin = max(a%xMin, b%xMin)
    c%yMin = max(a%yMin, b%yMin)
    c%xMax = min(a%xMax, b%xMax)
    c%yMax = min(a%yMax, b%yMax)
    
    ! If no intersection, return empty bbox
    if (c%xMin > c%xMax .or. c%yMin > c%yMax) then
      c%xMin = 0
      c%yMin = 0
      c%xMax = 0
      c%yMax = 0
    end if
  end function ft_bbox_intersect

end module ft_geometry