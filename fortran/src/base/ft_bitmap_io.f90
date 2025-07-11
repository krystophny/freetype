module ft_bitmap_io
  use ft_types
  use ft_bitmap_mod
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  private
  
  ! Public functions
  public :: ft_bitmap_write_pbm
  public :: ft_bitmap_write_pgm
  public :: ft_bitmap_write_ppm
  
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

end module ft_bitmap_io