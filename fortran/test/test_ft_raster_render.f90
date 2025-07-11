program test_ft_raster_render
  use ft_types
  use ft_geometry
  use ft_bitmap_mod
  use ft_outline_mod
  use ft_raster_types
  use ft_raster
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  
  ! Test rendering a simple shape
  call test_render_triangle()
  call test_render_square()
  call test_render_diamond()
  
  print '(/, "All rendering tests completed!")'

contains

  subroutine test_render_triangle()
    type(FT_Raster_State) :: raster
    type(FT_Outline), target :: outline
    type(FT_Bitmap), target :: bitmap
    type(FT_Raster_Params) :: params
    integer(FT_Error) :: error
    logical :: success
    integer :: x, y
    character(len=20) :: row_str
    
    print '(/, "Testing triangle rendering...")'
    
    ! Create triangle outline (scaled up to 8.8 fixed point)
    success = ft_outline_new(3, 1, outline, error)
    if (.not. success) then
      print '("Failed to create outline")'
      return
    end if
    
    ! Triangle vertices (in 8.8 fixed point, 256 units = 1 pixel)
    outline%points(1) = FT_Vector(256, 256)    ! (1,1)
    outline%points(2) = FT_Vector(1280, 256)   ! (5,1)
    outline%points(3) = FT_Vector(768, 1024)   ! (3,4)
    outline%contours(1) = 2
    outline%tags = FT_CURVE_TAG_ON
    
    ! Create bitmap
    success = ft_bitmap_new(8, 6, FT_PIXEL_MODE_MONO, bitmap, error)
    if (.not. success) then
      call ft_outline_done(outline)
      print '("Failed to create bitmap")'
      return
    end if
    
    ! Create rasterizer
    success = ft_raster_new(raster, error)
    if (.not. success) then
      call ft_outline_done(outline)
      call ft_bitmap_done(bitmap)
      print '("Failed to create rasterizer")'
      return
    end if
    
    ! Set up rasterizer
    raster%outline => outline
    
    ! Set up render params
    params%target => bitmap
    params%flags = 0
    
    ! Render outline
    success = ft_raster_render_outline(raster, params, error)
    
    if (success) then
      print '("Triangle rendered successfully")'
      print '("Bitmap output (8x6):")'
      
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
      print '("Failed to render triangle")'
    end if
    
    ! Cleanup
    call ft_raster_done(raster)
    call ft_outline_done(outline)
    call ft_bitmap_done(bitmap)
    
  end subroutine test_render_triangle
  
  subroutine test_render_square()
    type(FT_Raster_State) :: raster
    type(FT_Outline), target :: outline
    type(FT_Bitmap), target :: bitmap
    type(FT_Raster_Params) :: params
    integer(FT_Error) :: error
    logical :: success
    integer :: x, y
    character(len=20) :: row_str
    
    print '(/, "Testing square rendering...")'
    
    ! Create square outline
    success = ft_outline_new(4, 1, outline, error)
    if (.not. success) return
    
    ! Square vertices (2x2 pixels)
    outline%points(1) = FT_Vector(256, 256)    ! (1,1)
    outline%points(2) = FT_Vector(768, 256)    ! (3,1)
    outline%points(3) = FT_Vector(768, 768)    ! (3,3)
    outline%points(4) = FT_Vector(256, 768)    ! (1,3)
    outline%contours(1) = 3
    outline%tags = FT_CURVE_TAG_ON
    
    ! Create bitmap
    success = ft_bitmap_new(5, 5, FT_PIXEL_MODE_MONO, bitmap, error)
    if (.not. success) then
      call ft_outline_done(outline)
      return
    end if
    
    ! Create rasterizer
    success = ft_raster_new(raster, error)
    if (.not. success) then
      call ft_outline_done(outline)
      call ft_bitmap_done(bitmap)
      return
    end if
    
    raster%outline => outline
    params%target => bitmap
    params%flags = 0
    
    ! Render
    success = ft_raster_render_outline(raster, params, error)
    
    if (success) then
      print '("Square rendered successfully")'
      print '("Bitmap output (5x5):")'
      
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
    call ft_raster_done(raster)
    call ft_outline_done(outline)
    call ft_bitmap_done(bitmap)
    
  end subroutine test_render_square
  
  subroutine test_render_diamond()
    type(FT_Raster_State) :: raster
    type(FT_Outline), target :: outline
    type(FT_Bitmap), target :: bitmap
    type(FT_Raster_Params) :: params
    integer(FT_Error) :: error
    logical :: success
    integer :: x, y
    character(len=20) :: row_str
    
    print '(/, "Testing diamond rendering...")'
    
    ! Create diamond outline
    success = ft_outline_new(4, 1, outline, error)
    if (.not. success) return
    
    ! Diamond vertices
    outline%points(1) = FT_Vector(640, 256)    ! (2.5, 1) - top
    outline%points(2) = FT_Vector(1024, 640)   ! (4, 2.5) - right
    outline%points(3) = FT_Vector(640, 1024)   ! (2.5, 4) - bottom
    outline%points(4) = FT_Vector(256, 640)    ! (1, 2.5) - left
    outline%contours(1) = 3
    outline%tags = FT_CURVE_TAG_ON
    
    ! Create bitmap
    success = ft_bitmap_new(6, 6, FT_PIXEL_MODE_MONO, bitmap, error)
    if (.not. success) then
      call ft_outline_done(outline)
      return
    end if
    
    ! Create rasterizer
    success = ft_raster_new(raster, error)
    if (.not. success) then
      call ft_outline_done(outline)
      call ft_bitmap_done(bitmap)
      return
    end if
    
    raster%outline => outline
    params%target => bitmap
    params%flags = 0
    
    ! Render
    success = ft_raster_render_outline(raster, params, error)
    
    if (success) then
      print '("Diamond rendered successfully")'
      print '("Bitmap output (6x6):")'
      
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
    call ft_raster_done(raster)
    call ft_outline_done(outline)
    call ft_bitmap_done(bitmap)
    
  end subroutine test_render_diamond

end program test_ft_raster_render