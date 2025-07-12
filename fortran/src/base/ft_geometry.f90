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
  public :: ft_vector_scale
  public :: ft_vector_rotate
  
  ! Matrix operations
  public :: ft_matrix_multiply
  public :: ft_matrix_invert
  public :: ft_matrix_scale
  public :: ft_matrix_rotate
  public :: ft_matrix_translate
  
  ! BBox operations
  public :: ft_bbox_union
  public :: ft_bbox_intersect
  public :: ft_bbox_transform
  
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
  
  ! Scale vector by factor
  function ft_vector_scale(vector, scale_factor) result(result_vector)
    type(FT_Vector), intent(in) :: vector
    real, intent(in) :: scale_factor
    type(FT_Vector) :: result_vector
    
    result_vector%x = int(real(vector%x) * scale_factor)
    result_vector%y = int(real(vector%y) * scale_factor)
    
  end function ft_vector_scale
  
  ! Rotate vector by angle (in radians)
  function ft_vector_rotate(vector, angle) result(result_vector)
    type(FT_Vector), intent(in) :: vector
    real, intent(in) :: angle
    type(FT_Vector) :: result_vector
    
    real :: cos_a, sin_a, x_real, y_real
    
    cos_a = cos(angle)
    sin_a = sin(angle)
    x_real = real(vector%x)
    y_real = real(vector%y)
    
    result_vector%x = int(x_real * cos_a - y_real * sin_a)
    result_vector%y = int(x_real * sin_a + y_real * cos_a)
    
  end function ft_vector_rotate
  
  ! Create scaling matrix
  function ft_matrix_scale(scale_x, scale_y) result(matrix)
    real, intent(in) :: scale_x, scale_y
    type(FT_Matrix) :: matrix
    
    matrix%xx = int(scale_x * real(FT_FIXED_ONE))
    matrix%xy = 0
    matrix%yx = 0
    matrix%yy = int(scale_y * real(FT_FIXED_ONE))
    
  end function ft_matrix_scale
  
  ! Create rotation matrix
  function ft_matrix_rotate(angle) result(matrix)
    real, intent(in) :: angle
    type(FT_Matrix) :: matrix
    
    real :: cos_a, sin_a
    
    cos_a = cos(angle)
    sin_a = sin(angle)
    
    matrix%xx = int(cos_a * real(FT_FIXED_ONE))
    matrix%xy = int(-sin_a * real(FT_FIXED_ONE))
    matrix%yx = int(sin_a * real(FT_FIXED_ONE))
    matrix%yy = int(cos_a * real(FT_FIXED_ONE))
    
  end function ft_matrix_rotate
  
  ! Create translation matrix (for homogeneous coordinates)
  function ft_matrix_translate(offset_x, offset_y) result(matrix)
    integer(FT_Pos), intent(in) :: offset_x, offset_y
    type(FT_Matrix) :: matrix
    
    ! Standard 2x2 matrix doesn't support translation
    ! This creates an identity matrix - translation must be applied separately
    matrix%xx = FT_FIXED_ONE
    matrix%xy = 0
    matrix%yx = 0
    matrix%yy = FT_FIXED_ONE
    
  end function ft_matrix_translate
  
  ! Transform bounding box
  function ft_bbox_transform(bbox, matrix) result(result_bbox)
    type(FT_BBox), intent(in) :: bbox
    type(FT_Matrix), intent(in) :: matrix
    type(FT_BBox) :: result_bbox
    
    type(FT_Vector) :: corners(4)
    type(FT_Vector) :: transformed_corners(4)
    integer :: i
    
    ! Define the four corners of the bounding box
    corners(1)%x = bbox%xMin
    corners(1)%y = bbox%yMin
    corners(2)%x = bbox%xMax
    corners(2)%y = bbox%yMin
    corners(3)%x = bbox%xMax
    corners(3)%y = bbox%yMax
    corners(4)%x = bbox%xMin
    corners(4)%y = bbox%yMax
    
    ! Transform all corners
    do i = 1, 4
      transformed_corners(i) = ft_vector_transform(corners(i), matrix)
    end do
    
    ! Find new bounding box
    result_bbox%xMin = transformed_corners(1)%x
    result_bbox%xMax = transformed_corners(1)%x
    result_bbox%yMin = transformed_corners(1)%y
    result_bbox%yMax = transformed_corners(1)%y
    
    do i = 2, 4
      result_bbox%xMin = min(result_bbox%xMin, transformed_corners(i)%x)
      result_bbox%xMax = max(result_bbox%xMax, transformed_corners(i)%x)
      result_bbox%yMin = min(result_bbox%yMin, transformed_corners(i)%y)
      result_bbox%yMax = max(result_bbox%yMax, transformed_corners(i)%y)
    end do
    
  end function ft_bbox_transform

end module ft_geometry