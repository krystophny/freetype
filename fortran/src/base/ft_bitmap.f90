module ft_bitmap_mod
  use ft_types
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  private
  
  ! Public types
  public :: FT_Bitmap
  public :: FT_Pixel_Mode
  
  ! Public functions
  public :: ft_bitmap_new
  public :: ft_bitmap_done
  public :: ft_bitmap_copy
  public :: ft_bitmap_clear
  public :: ft_bitmap_fill
  public :: ft_bitmap_set_pixel
  public :: ft_bitmap_set_pixel_gray
  public :: ft_bitmap_get_pixel
  public :: ft_bitmap_convert
  
  ! Pixel modes
  public :: FT_PIXEL_MODE_NONE
  public :: FT_PIXEL_MODE_MONO
  public :: FT_PIXEL_MODE_GRAY
  public :: FT_PIXEL_MODE_GRAY2
  public :: FT_PIXEL_MODE_GRAY4
  public :: FT_PIXEL_MODE_LCD
  public :: FT_PIXEL_MODE_LCD_V
  public :: FT_PIXEL_MODE_BGRA
  
  ! Bitmap structure
  type :: FT_Bitmap
    integer :: rows          ! Number of bitmap rows
    integer :: width         ! Number of pixels in bitmap row
    integer :: pitch         ! Number of bytes per bitmap row (can be negative)
    integer(int8), pointer :: buffer(:) => null()  ! Bitmap buffer
    integer :: num_grays     ! Number of gray levels (for anti-aliased modes)
    integer(int8) :: pixel_mode    ! Pixel mode
    integer(int8) :: palette_mode  ! Palette mode (unused for now)
    type(c_ptr) :: palette = c_null_ptr  ! Palette pointer (unused)
  end type FT_Bitmap
  
  ! Pixel mode constants
  integer(int8), parameter :: FT_PIXEL_MODE_NONE = 0
  integer(int8), parameter :: FT_PIXEL_MODE_MONO = 1    ! Monochrome (1 bit/pixel)
  integer(int8), parameter :: FT_PIXEL_MODE_GRAY = 2    ! Anti-aliased (8 bits/pixel)
  integer(int8), parameter :: FT_PIXEL_MODE_GRAY2 = 3   ! 2 bits/pixel
  integer(int8), parameter :: FT_PIXEL_MODE_GRAY4 = 4   ! 4 bits/pixel
  integer(int8), parameter :: FT_PIXEL_MODE_LCD = 5     ! LCD decimated
  integer(int8), parameter :: FT_PIXEL_MODE_LCD_V = 6   ! LCD decimated vertical
  integer(int8), parameter :: FT_PIXEL_MODE_BGRA = 7    ! BGRA color
  
  type :: FT_Pixel_Mode
    integer(int8) :: value
  end type FT_Pixel_Mode

contains

  ! Create a new bitmap
  function ft_bitmap_new(width, rows, pixel_mode, bitmap, error) result(success)
    integer, intent(in) :: width, rows
    integer(int8), intent(in) :: pixel_mode
    type(FT_Bitmap), intent(out) :: bitmap
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: pitch, buffer_size
    
    success = .false.
    error = FT_Err_Ok
    
    ! Initialize bitmap structure
    bitmap%width = width
    bitmap%rows = rows
    bitmap%pixel_mode = pixel_mode
    bitmap%palette_mode = 0
    bitmap%palette = c_null_ptr
    
    ! Calculate pitch and buffer size based on pixel mode
    select case (pixel_mode)
    case (FT_PIXEL_MODE_MONO)
      ! Monochrome: 1 bit per pixel, aligned to byte boundary
      bitmap%pitch = (width + 7) / 8
      bitmap%num_grays = 2
      
    case (FT_PIXEL_MODE_GRAY)
      ! 8-bit grayscale
      bitmap%pitch = width
      bitmap%num_grays = 256
      
    case (FT_PIXEL_MODE_GRAY2)
      ! 2 bits per pixel
      bitmap%pitch = (width + 3) / 4
      bitmap%num_grays = 4
      
    case (FT_PIXEL_MODE_GRAY4)
      ! 4 bits per pixel
      bitmap%pitch = (width + 1) / 2
      bitmap%num_grays = 16
      
    case (FT_PIXEL_MODE_LCD)
      ! LCD mode: 3x horizontal oversampling
      bitmap%pitch = width * 3
      bitmap%num_grays = 256
      
    case (FT_PIXEL_MODE_LCD_V)
      ! LCD vertical: normal width
      bitmap%pitch = width
      bitmap%num_grays = 256
      
    case (FT_PIXEL_MODE_BGRA)
      ! 32-bit BGRA
      bitmap%pitch = width * 4
      bitmap%num_grays = 256
      
    case default
      error = FT_Err_Invalid_Argument
      return
    end select
    
    ! Allocate buffer
    buffer_size = abs(bitmap%pitch) * rows
    if (buffer_size > 0) then
      allocate(bitmap%buffer(buffer_size), stat=error)
      if (error /= 0) then
        error = FT_Err_Out_Of_Memory
        return
      end if
      
      ! Clear buffer
      bitmap%buffer = 0
    end if
    
    error = FT_Err_Ok
    success = .true.
    
  end function ft_bitmap_new
  
  ! Free bitmap memory
  subroutine ft_bitmap_done(bitmap)
    type(FT_Bitmap), intent(inout) :: bitmap
    
    if (associated(bitmap%buffer)) then
      deallocate(bitmap%buffer)
      bitmap%buffer => null()
    end if
    
    bitmap%rows = 0
    bitmap%width = 0
    bitmap%pitch = 0
    bitmap%num_grays = 0
    bitmap%pixel_mode = FT_PIXEL_MODE_NONE
    
  end subroutine ft_bitmap_done
  
  ! Copy bitmap
  function ft_bitmap_copy(source, target, error) result(success)
    type(FT_Bitmap), intent(in) :: source
    type(FT_Bitmap), intent(out) :: target
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: buffer_size, i
    
    success = .false.
    
    ! Create target bitmap with same parameters
    if (.not. ft_bitmap_new(source%width, source%rows, source%pixel_mode, &
                            target, error)) then
      return
    end if
    
    ! Copy buffer data if it exists
    if (associated(source%buffer) .and. associated(target%buffer)) then
      buffer_size = abs(source%pitch) * source%rows
      do i = 1, min(size(source%buffer), size(target%buffer))
        target%buffer(i) = source%buffer(i)
      end do
    end if
    
    ! Copy other fields
    target%num_grays = source%num_grays
    target%palette_mode = source%palette_mode
    
    success = .true.
    
  end function ft_bitmap_copy
  
  ! Clear bitmap (set all pixels to 0)
  subroutine ft_bitmap_clear(bitmap)
    type(FT_Bitmap), intent(inout) :: bitmap
    
    if (associated(bitmap%buffer)) then
      bitmap%buffer = 0
    end if
    
  end subroutine ft_bitmap_clear
  
  ! Fill bitmap with value
  subroutine ft_bitmap_fill(bitmap, value)
    type(FT_Bitmap), intent(inout) :: bitmap
    integer(int8), intent(in) :: value
    
    if (associated(bitmap%buffer)) then
      bitmap%buffer = value
    end if
    
  end subroutine ft_bitmap_fill
  
  ! Set pixel value (monochrome version)
  subroutine ft_bitmap_set_pixel(bitmap, x, y, value)
    type(FT_Bitmap), intent(inout) :: bitmap
    integer, intent(in) :: x, y
    logical, intent(in) :: value
    
    integer :: byte_offset, bit_offset
    integer(int8) :: mask
    
    ! Bounds check
    if (x < 0 .or. x >= bitmap%width .or. y < 0 .or. y >= bitmap%rows) return
    if (.not. associated(bitmap%buffer)) return
    
    select case (bitmap%pixel_mode)
    case (FT_PIXEL_MODE_MONO)
      ! Calculate byte and bit position
      byte_offset = y * abs(bitmap%pitch) + x / 8 + 1
      bit_offset = 7 - mod(x, 8)  ! Most significant bit first
      
      if (byte_offset > 0 .and. byte_offset <= size(bitmap%buffer)) then
        mask = int(ishft(1, bit_offset), int8)
        
        if (value) then
          ! Set bit
          bitmap%buffer(byte_offset) = ior(bitmap%buffer(byte_offset), mask)
        else
          ! Clear bit
          bitmap%buffer(byte_offset) = iand(bitmap%buffer(byte_offset), not(mask))
        end if
      end if
      
    case (FT_PIXEL_MODE_GRAY)
      ! 8-bit grayscale
      byte_offset = y * abs(bitmap%pitch) + x + 1
      if (byte_offset > 0 .and. byte_offset <= size(bitmap%buffer)) then
        if (value) then
          bitmap%buffer(byte_offset) = -1_int8  ! 255
        else
          bitmap%buffer(byte_offset) = 0_int8
        end if
      end if
      
    end select
    
  end subroutine ft_bitmap_set_pixel
  
  ! Set pixel gray value (0-255)
  subroutine ft_bitmap_set_pixel_gray(bitmap, x, y, gray_value)
    type(FT_Bitmap), intent(inout) :: bitmap
    integer, intent(in) :: x, y, gray_value
    
    integer :: byte_offset
    integer(int8) :: clamped_value
    
    ! Bounds check
    if (x < 0 .or. x >= bitmap%width .or. y < 0 .or. y >= bitmap%rows) return
    if (.not. associated(bitmap%buffer)) return
    
    ! Only works for grayscale mode
    if (bitmap%pixel_mode /= FT_PIXEL_MODE_GRAY) return
    
    ! Clamp gray value to 0-255
    clamped_value = int(max(0, min(255, gray_value)), int8)
    
    ! 8-bit grayscale
    byte_offset = y * abs(bitmap%pitch) + x + 1
    if (byte_offset > 0 .and. byte_offset <= size(bitmap%buffer)) then
      bitmap%buffer(byte_offset) = clamped_value
    end if
    
  end subroutine ft_bitmap_set_pixel_gray
  
  ! Get pixel value (monochrome version)
  function ft_bitmap_get_pixel(bitmap, x, y) result(value)
    type(FT_Bitmap), intent(in) :: bitmap
    integer, intent(in) :: x, y
    logical :: value
    
    integer :: byte_offset, bit_offset
    integer(int8) :: mask
    
    value = .false.
    
    ! Bounds check
    if (x < 0 .or. x >= bitmap%width .or. y < 0 .or. y >= bitmap%rows) return
    if (.not. associated(bitmap%buffer)) return
    
    select case (bitmap%pixel_mode)
    case (FT_PIXEL_MODE_MONO)
      ! Calculate byte and bit position
      byte_offset = y * abs(bitmap%pitch) + x / 8 + 1
      bit_offset = 7 - mod(x, 8)
      
      if (byte_offset > 0 .and. byte_offset <= size(bitmap%buffer)) then
        mask = int(ishft(1, bit_offset), int8)
        value = (iand(bitmap%buffer(byte_offset), mask) /= 0)
      end if
      
    case (FT_PIXEL_MODE_GRAY)
      ! 8-bit grayscale
      byte_offset = y * abs(bitmap%pitch) + x + 1
      if (byte_offset > 0 .and. byte_offset <= size(bitmap%buffer)) then
        value = (bitmap%buffer(byte_offset) /= 0)
      end if
      
    end select
    
  end function ft_bitmap_get_pixel
  
  ! Convert bitmap between pixel modes (placeholder)
  function ft_bitmap_convert(source, target, pixel_mode, error) result(success)
    type(FT_Bitmap), intent(in) :: source
    type(FT_Bitmap), intent(out) :: target
    integer(int8), intent(in) :: pixel_mode
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: x, y
    logical :: pixel_value
    
    success = .false.
    error = FT_Err_Ok
    
    ! Create target bitmap
    if (.not. ft_bitmap_new(source%width, source%rows, pixel_mode, target, error)) then
      return
    end if
    
    ! Simple conversion for now (mono to target)
    if (source%pixel_mode == FT_PIXEL_MODE_MONO) then
      do y = 0, source%rows - 1
        do x = 0, source%width - 1
          pixel_value = ft_bitmap_get_pixel(source, x, y)
          call ft_bitmap_set_pixel(target, x, y, pixel_value)
        end do
      end do
    else
      ! Just copy for now
      if (.not. ft_bitmap_copy(source, target, error)) then
        return
      end if
    end if
    
    success = .true.
    
  end function ft_bitmap_convert

end module ft_bitmap_mod