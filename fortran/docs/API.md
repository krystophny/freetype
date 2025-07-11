# FreeType Fortran API Documentation

## Overview

The FreeType Fortran port provides a modern Fortran interface to font rendering functionality. This document describes the public APIs organized by module.

## Module: ft_types

Basic type definitions and fixed-point arithmetic.

### Types

- `FT_Error` - Error code type (integer)
- `FT_Fixed` - 16.16 fixed-point type
- `FT_F26Dot6` - 26.6 fixed-point type
- `FT_F2Dot14` - 2.14 fixed-point type

### Fixed-Point Functions

```fortran
! Convert integer to 16.16 fixed-point
pure function ft_fixed_from_int(value) result(fixed)
  integer, intent(in) :: value
  integer(FT_Fixed) :: fixed

! Convert 16.16 fixed-point to integer
pure function ft_fixed_to_int(fixed) result(value)
  integer(FT_Fixed), intent(in) :: fixed
  integer :: value

! Convert real to 16.16 fixed-point
pure function ft_fixed_from_float(value) result(fixed)
  real, intent(in) :: value
  integer(FT_Fixed) :: fixed

! Fixed-point arithmetic
pure function ft_fixed_add(a, b) result(c)
pure function ft_fixed_sub(a, b) result(c)
pure function ft_fixed_mul(a, b) result(c)
pure function ft_fixed_div(a, b) result(c)
```

### Error Constants

- `FT_Err_Ok` - Success (0)
- `FT_Err_Out_Of_Memory` - Memory allocation failed
- `FT_Err_Invalid_Argument` - Invalid parameter
- (and many more...)

## Module: ft_geometry

Geometric types and operations.

### Types

```fortran
type, bind(C) :: FT_Vector
  integer(FT_Pos) :: x
  integer(FT_Pos) :: y
end type

type, bind(C) :: FT_Matrix
  integer(FT_Fixed) :: xx, xy
  integer(FT_Fixed) :: yx, yy
end type

type, bind(C) :: FT_BBox
  integer(FT_Pos) :: xMin, yMin
  integer(FT_Pos) :: xMax, yMax
end type
```

### Vector Functions

```fortran
! Vector arithmetic
pure function ft_vector_add(a, b) result(c)
pure function ft_vector_sub(a, b) result(c)
pure function ft_vector_dot(a, b) result(dot)

! Transform vector by matrix
pure function ft_vector_transform(vec, matrix) result(result_vec)
```

### Matrix Functions

```fortran
! Matrix multiplication
pure function ft_matrix_multiply(a, b) result(c)

! Matrix inversion
subroutine ft_matrix_invert(matrix, inv, success)
```

### BBox Functions

```fortran
! Bounding box operations
pure function ft_bbox_union(a, b) result(c)
pure function ft_bbox_intersect(a, b) result(c)
```

## Module: ft_memory

Memory management with optional tracking.

### Functions

```fortran
! Initialize memory system
subroutine ft_mem_init(enable_tracking)
  logical, intent(in), optional :: enable_tracking

! Allocate memory
function ft_mem_alloc(size, ptr, error) result(success)
  integer(c_size_t), intent(in) :: size
  type(c_ptr), intent(out) :: ptr
  integer(FT_Error), intent(out) :: error
  logical :: success

! Free memory
subroutine ft_mem_free(ptr, size)
  type(c_ptr), intent(inout) :: ptr
  integer(c_size_t), intent(in), optional :: size

! Memory operations
subroutine ft_mem_copy(dest, src, size)
subroutine ft_mem_set(ptr, value, size)
subroutine ft_mem_move(dest, src, size)

! Get memory statistics
function ft_mem_get_stats() result(stats)
  type(FT_Memory_Stats) :: stats
```

## Module: ft_object

Object system with reference counting.

### Types

```fortran
type :: FT_Object_Type
  type(FT_Object_Class), pointer :: clazz
  integer :: ref_count
  type(c_ptr) :: internal
end type

type :: FT_Object_Class
  integer :: class_id
  integer :: object_size
  procedure(object_init_proc), pointer :: init
  procedure(object_done_proc), pointer :: done
end type
```

### Object Functions

```fortran
! Create object
function ft_object_create(clazz, object, error) result(success)

! Initialize object
subroutine ft_object_init(object, clazz)

! Reference counting
subroutine ft_object_ref(object)
function ft_object_unref(object) result(destroyed)

! Get class ID
function ft_object_get_class_id(object) result(class_id)
```

### List Operations

```fortran
! List management
subroutine ft_list_init(list)
subroutine ft_list_done(list)

! List operations
function ft_list_add(list, data, error) result(node)
subroutine ft_list_remove(list, node)
function ft_list_find(list, data) result(node)

! List iteration
subroutine ft_list_iterate(list, iterator, user_data)
function ft_list_count(list) result(count)
```

## Module: ft_glyph_types

Glyph and font metrics types.

### Types

```fortran
type, bind(C) :: FT_Glyph_Metrics
  integer(FT_Pos) :: width, height
  integer(FT_Pos) :: horiBearingX, horiBearingY
  integer(FT_Pos) :: horiAdvance
  integer(FT_Pos) :: vertBearingX, vertBearingY
  integer(FT_Pos) :: vertAdvance
end type

type, bind(C) :: FT_Size_Metrics
  integer(FT_UShort) :: x_ppem, y_ppem
  integer(FT_Fixed) :: x_scale, y_scale
  integer(FT_Pos) :: ascender, descender
  integer(FT_Pos) :: height, max_advance
end type
```

### Glyph Format Constants

- `FT_GLYPH_FORMAT_NONE` - No glyph format
- `FT_GLYPH_FORMAT_BITMAP` - Bitmap glyph
- `FT_GLYPH_FORMAT_OUTLINE` - Outline glyph
- `FT_GLYPH_FORMAT_COMPOSITE` - Composite glyph

### Functions

```fortran
! Initialize metrics
pure subroutine ft_glyph_metrics_init(metrics)
pure subroutine ft_size_metrics_init(metrics)

! Scale glyph metrics
pure subroutine ft_glyph_metrics_scale(metrics, scale_x, scale_y)
```

## Usage Example

```fortran
program example
  use ft_types
  use ft_geometry
  use ft_memory
  
  type(FT_Vector) :: v1, v2, result
  integer(FT_Fixed) :: scale
  
  ! Initialize memory system with tracking
  call ft_mem_init(enable_tracking=.true.)
  
  ! Create vectors
  v1%x = ft_fixed_from_int(10)
  v1%y = ft_fixed_from_int(20)
  
  v2%x = ft_fixed_from_float(5.5)
  v2%y = ft_fixed_from_float(7.5)
  
  ! Add vectors
  result = ft_vector_add(v1, v2)
  
  ! Scale result
  scale = ft_fixed_from_float(2.0)
  result%x = ft_fixed_mul(result%x, scale)
  result%y = ft_fixed_mul(result%y, scale)
  
  ! Clean up
  call ft_mem_done()
  
end program example
```

## Error Handling

All functions that can fail return an error code of type `FT_Error`. Always check for `FT_Err_Ok` to ensure success:

```fortran
integer(FT_Error) :: error
type(c_ptr) :: ptr

if (ft_mem_alloc(1024_c_size_t, ptr, error)) then
  if (error == FT_Err_Ok) then
    ! Success
  else
    ! Handle error
  end if
end if
```

## Thread Safety

The current implementation is not thread-safe. Memory tracking and object reference counting require external synchronization in multi-threaded environments.

## Performance Considerations

- Fixed-point arithmetic is optimized for 64-bit intermediate calculations
- Memory allocation uses C malloc/free for compatibility
- Vector and matrix operations are implemented as pure functions for optimization
- Lists are implemented as doubly-linked for O(1) insertion/removal

## Version Compatibility

This API corresponds to Phase 1 of the FreeType Fortran port, providing foundation types and basic functionality. Font loading and rendering capabilities will be added in subsequent phases.