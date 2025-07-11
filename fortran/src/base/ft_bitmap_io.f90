module ft_bitmap_io
  use ft_types
  use ft_bitmap_mod
  use fortplot_png, only: write_png_file
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  private
  
  ! Public functions
  public :: ft_bitmap_write_pbm
  public :: ft_bitmap_write_pgm
  public :: ft_bitmap_write_ppm
  public :: ft_bitmap_write_png
  
contains

  ! Write bitmap as PBM (Portable Bitmap) file - monochrome
  function ft_bitmap_write_pbm(bitmap, filename, error) result(success)
    type(FT_Bitmap), intent(in) :: bitmap
    character(len=*), intent(in) :: filename
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: unit, iostat
    integer :: x, y
    character(len=1) :: pixel
    
    success = .false.
    error = FT_Err_Ok
    
    ! Check bitmap format
    if (bitmap%pixel_mode /= FT_PIXEL_MODE_MONO) then
      error = FT_Err_Invalid_Argument
      return
    end if
    
    ! Open file
    open(newunit=unit, file=filename, status='replace', &
         action='write', iostat=iostat)
    if (iostat /= 0) then
      error = FT_Err_Cannot_Open_Resource
      return
    end if
    
    ! Write PBM header
    write(unit, '(A)') 'P1'
    write(unit, '(A)') '# Created by FreeType Fortran port'
    write(unit, '(I0, 1X, I0)') bitmap%width, bitmap%rows
    
    ! Write pixel data (1 = black, 0 = white in PBM)
    do y = 0, bitmap%rows - 1
      do x = 0, bitmap%width - 1
        if (ft_bitmap_get_pixel(bitmap, x, y)) then
          pixel = '1'
        else
          pixel = '0'
        end if
        write(unit, '(A1, 1X)', advance='no') pixel
      end do
      write(unit, *)  ! New line
    end do
    
    close(unit)
    
    error = FT_Err_Ok
    success = .true.
    
  end function ft_bitmap_write_pbm
  
  ! Write bitmap as PGM (Portable Graymap) file - grayscale
  function ft_bitmap_write_pgm(bitmap, filename, error) result(success)
    type(FT_Bitmap), intent(in) :: bitmap
    character(len=*), intent(in) :: filename
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: unit, iostat
    integer :: x, y, byte_offset
    integer :: gray_value
    
    success = .false.
    error = FT_Err_Ok
    
    ! Check bitmap format
    if (bitmap%pixel_mode /= FT_PIXEL_MODE_GRAY) then
      error = FT_Err_Invalid_Argument
      return
    end if
    
    ! Open file
    open(newunit=unit, file=filename, status='replace', &
         action='write', iostat=iostat)
    if (iostat /= 0) then
      error = FT_Err_Cannot_Open_Resource
      return
    end if
    
    ! Write PGM header
    write(unit, '(A)') 'P2'
    write(unit, '(A)') '# Created by FreeType Fortran port'
    write(unit, '(I0, 1X, I0)') bitmap%width, bitmap%rows
    write(unit, '(I0)') bitmap%num_grays - 1
    
    ! Write pixel data
    do y = 0, bitmap%rows - 1
      do x = 0, bitmap%width - 1
        byte_offset = y * abs(bitmap%pitch) + x + 1
        if (byte_offset > 0 .and. byte_offset <= size(bitmap%buffer)) then
          ! Convert signed byte to unsigned gray value
          gray_value = iand(int(bitmap%buffer(byte_offset)), 255)
        else
          gray_value = 0
        end if
        write(unit, '(I0, 1X)', advance='no') gray_value
      end do
      write(unit, *)  ! New line
    end do
    
    close(unit)
    
    error = FT_Err_Ok
    success = .true.
    
  end function ft_bitmap_write_pgm
  
  ! Write colored bitmap as PPM (Portable Pixmap) file
  function ft_bitmap_write_ppm(bitmap, filename, error) result(success)
    type(FT_Bitmap), intent(in) :: bitmap
    character(len=*), intent(in) :: filename
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: unit, iostat
    integer :: x, y, byte_offset
    integer :: r, g, b
    
    success = .false.
    error = FT_Err_Ok
    
    ! For now, convert monochrome/gray to RGB
    open(newunit=unit, file=filename, status='replace', &
         action='write', iostat=iostat)
    if (iostat /= 0) then
      error = FT_Err_Cannot_Open_Resource
      return
    end if
    
    ! Write PPM header
    write(unit, '(A)') 'P3'
    write(unit, '(A)') '# Created by FreeType Fortran port'
    write(unit, '(I0, 1X, I0)') bitmap%width, bitmap%rows
    write(unit, '(A)') '255'
    
    ! Write pixel data based on mode
    select case (bitmap%pixel_mode)
    case (FT_PIXEL_MODE_MONO)
      ! Monochrome to RGB
      do y = 0, bitmap%rows - 1
        do x = 0, bitmap%width - 1
          if (ft_bitmap_get_pixel(bitmap, x, y)) then
            r = 0; g = 0; b = 0  ! Black
          else
            r = 255; g = 255; b = 255  ! White
          end if
          write(unit, '(I0, 1X, I0, 1X, I0, 2X)', advance='no') r, g, b
        end do
        write(unit, *)
      end do
      
    case (FT_PIXEL_MODE_GRAY)
      ! Grayscale to RGB
      do y = 0, bitmap%rows - 1
        do x = 0, bitmap%width - 1
          byte_offset = y * abs(bitmap%pitch) + x + 1
          if (byte_offset > 0 .and. byte_offset <= size(bitmap%buffer)) then
            r = 255 - iand(int(bitmap%buffer(byte_offset)), 255)
            g = r
            b = r
          else
            r = 255; g = 255; b = 255
          end if
          write(unit, '(I0, 1X, I0, 1X, I0, 2X)', advance='no') r, g, b
        end do
        write(unit, *)
      end do
      
    case default
      ! Unsupported format
      close(unit)
      error = FT_Err_Invalid_Argument
      return
    end select
    
    close(unit)
    
    error = FT_Err_Ok
    success = .true.
    
  end function ft_bitmap_write_ppm
  
  ! Write bitmap as PNG file using fortplotlib
  function ft_bitmap_write_png(bitmap, filename, error) result(success)
    type(FT_Bitmap), intent(in) :: bitmap
    character(len=*), intent(in) :: filename
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer(int8), allocatable :: img_data(:)  ! RGBA image data for fortplotlib
    integer :: x, y, byte_offset, idx
    integer :: gray_val
    
    success = .false.
    error = FT_Err_Ok
    
    ! Allocate RGBA image data (width * height * 4)
    allocate(img_data(bitmap%width * bitmap%rows * 4))
    
    ! Convert bitmap to RGBA array based on pixel mode
    select case (bitmap%pixel_mode)
    case (FT_PIXEL_MODE_MONO)
      ! Monochrome to RGBA
      do y = 0, bitmap%rows - 1
        do x = 0, bitmap%width - 1
          idx = (y * bitmap%width + x) * 4 + 1
          if (ft_bitmap_get_pixel(bitmap, x, y)) then
            ! Black pixel
            img_data(idx) = 0_int8      ! R
            img_data(idx+1) = 0_int8    ! G
            img_data(idx+2) = 0_int8    ! B
            img_data(idx+3) = -1_int8   ! A (255)
          else
            ! White pixel
            img_data(idx) = -1_int8     ! R (255)
            img_data(idx+1) = -1_int8   ! G (255)
            img_data(idx+2) = -1_int8   ! B (255)
            img_data(idx+3) = -1_int8   ! A (255)
          end if
        end do
      end do
      
    case (FT_PIXEL_MODE_GRAY)
      ! Grayscale to RGBA
      do y = 0, bitmap%rows - 1
        do x = 0, bitmap%width - 1
          idx = (y * bitmap%width + x) * 4 + 1
          byte_offset = y * abs(bitmap%pitch) + x + 1
          if (byte_offset > 0 .and. byte_offset <= size(bitmap%buffer)) then
            ! Invert for text: 0=white, 255=black
            gray_val = 255 - iand(int(bitmap%buffer(byte_offset)), 255)
          else
            gray_val = 255
          end if
          img_data(idx) = int(gray_val, int8)      ! R
          img_data(idx+1) = int(gray_val, int8)    ! G
          img_data(idx+2) = int(gray_val, int8)    ! B
          img_data(idx+3) = -1_int8                 ! A (255)
        end do
      end do
      
    case default
      ! Unsupported format
      deallocate(img_data)
      error = FT_Err_Invalid_Argument
      return
    end select
    
    ! Write PNG file using fortplotlib
    call write_png_file(trim(filename), bitmap%width, bitmap%rows, img_data)
    
    ! Cleanup
    deallocate(img_data)
    
    error = FT_Err_Ok
    success = .true.
    
  end function ft_bitmap_write_png

end module ft_bitmap_io