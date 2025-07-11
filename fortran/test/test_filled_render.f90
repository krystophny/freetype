program test_filled_render
  use ft_types
  use ft_geometry
  use ft_bitmap_mod
  use ft_outline_mod
  use ft_scanline_simple
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  
  ! Test filled shape rendering
  call test_filled_triangle()
  call test_filled_square()
  call test_filled_diamond()
  
  print '(/, "All filled rendering tests completed!")'

contains

  subroutine test_filled_triangle()
    type(FT_Outline), target :: outline
    type(FT_Bitmap), target :: bitmap
    integer(FT_Error) :: error
    logical :: success
    integer :: x, y
    character(len=20) :: row_str
    
    print '(/, "Testing filled triangle rendering...")'
    
    ! Create triangle outline
    success = ft_outline_new(3, 1, outline, error)
    if (.not. success) return
    
    ! Triangle vertices
    outline%points(1) = FT_Vector(512, 512)    ! (2,2)
    outline%points(2) = FT_Vector(1536, 512)   ! (6,2)  
    outline%points(3) = FT_Vector(1024, 1536)  ! (4,6)
    outline%contours(1) = 2
    outline%tags = FT_CURVE_TAG_ON
    
    ! Create bitmap
    success = ft_bitmap_new(10, 10, FT_PIXEL_MODE_MONO, bitmap, error)
    if (.not. success) then
      call ft_outline_done(outline)
      return
    end if
    
    ! Render filled triangle
    success = ft_render_outline_filled(outline, bitmap, error)
    
    if (success) then
      print '("Filled triangle rendered successfully")'
      print '("Bitmap output (10x10):")'
      
      ! Display bitmap
      do y = 0, bitmap%rows - 1
        row_str = ""
        do x = 0, bitmap%width - 1
          if (ft_bitmap_get_pixel(bitmap, x, y)) then
            row_str(x+1:x+1) = "*"
          else
            row_str(x+1:x+1) = "."
          end if
        end do
        print '(A)', trim(row_str)
      end do
    else
      print '("Failed to render filled triangle")'
    end if
    
    ! Cleanup
    call ft_outline_done(outline)
    call ft_bitmap_done(bitmap)
    
  end subroutine test_filled_triangle
  
  subroutine test_filled_square()
    type(FT_Outline), target :: outline
    type(FT_Bitmap), target :: bitmap
    integer(FT_Error) :: error
    logical :: success
    integer :: x, y
    character(len=20) :: row_str
    
    print '(/, "Testing filled square rendering...")'
    
    ! Create square outline
    success = ft_outline_new(4, 1, outline, error)
    if (.not. success) return
    
    ! Square vertices (3x3 square)
    outline%points(1) = FT_Vector(512, 512)    ! (2,2)
    outline%points(2) = FT_Vector(1280, 512)   ! (5,2)
    outline%points(3) = FT_Vector(1280, 1280)  ! (5,5)
    outline%points(4) = FT_Vector(512, 1280)   ! (2,5)
    outline%contours(1) = 3
    outline%tags = FT_CURVE_TAG_ON
    
    ! Create bitmap
    success = ft_bitmap_new(8, 8, FT_PIXEL_MODE_MONO, bitmap, error)
    if (.not. success) then
      call ft_outline_done(outline)
      return
    end if
    
    ! Render filled square
    success = ft_render_outline_filled(outline, bitmap, error)
    
    if (success) then
      print '("Filled square rendered successfully")'
      print '("Bitmap output (8x8):")'
      
      ! Display bitmap
      do y = 0, bitmap%rows - 1
        row_str = ""
        do x = 0, bitmap%width - 1
          if (ft_bitmap_get_pixel(bitmap, x, y)) then
            row_str(x+1:x+1) = "*"
          else
            row_str(x+1:x+1) = "."
          end if
        end do
        print '(A)', trim(row_str)
      end do
    end if
    
    ! Cleanup
    call ft_outline_done(outline)
    call ft_bitmap_done(bitmap)
    
  end subroutine test_filled_square
  
  subroutine test_filled_diamond()
    type(FT_Outline), target :: outline
    type(FT_Bitmap), target :: bitmap
    integer(FT_Error) :: error
    logical :: success
    integer :: x, y
    character(len=20) :: row_str
    
    print '(/, "Testing filled diamond rendering...")'
    
    ! Create diamond outline
    success = ft_outline_new(4, 1, outline, error)
    if (.not. success) return
    
    ! Diamond vertices
    outline%points(1) = FT_Vector(768, 256)    ! (3, 1) - top
    outline%points(2) = FT_Vector(1280, 768)   ! (5, 3) - right
    outline%points(3) = FT_Vector(768, 1280)   ! (3, 5) - bottom
    outline%points(4) = FT_Vector(256, 768)    ! (1, 3) - left
    outline%contours(1) = 3
    outline%tags = FT_CURVE_TAG_ON
    
    ! Create bitmap
    success = ft_bitmap_new(7, 7, FT_PIXEL_MODE_MONO, bitmap, error)
    if (.not. success) then
      call ft_outline_done(outline)
      return
    end if
    
    ! Render filled diamond
    success = ft_render_outline_filled(outline, bitmap, error)
    
    if (success) then
      print '("Filled diamond rendered successfully")'
      print '("Bitmap output (7x7):")'
      
      ! Display bitmap
      do y = 0, bitmap%rows - 1
        row_str = ""
        do x = 0, bitmap%width - 1
          if (ft_bitmap_get_pixel(bitmap, x, y)) then
            row_str(x+1:x+1) = "*"
          else
            row_str(x+1:x+1) = "."
          end if
        end do
        print '(A)', trim(row_str)
      end do
    end if
    
    ! Cleanup
    call ft_outline_done(outline)
    call ft_bitmap_done(bitmap)
    
  end subroutine test_filled_diamond

end program test_filled_render