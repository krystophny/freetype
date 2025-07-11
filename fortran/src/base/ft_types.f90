module ft_types
  use, intrinsic :: iso_c_binding
  implicit none
  private

  ! Make types public
  public :: FT_Error
  public :: FT_Fixed
  public :: FT_F26Dot6
  public :: FT_F2Dot14
  
  ! Fixed-point math functions
  public :: ft_fixed_from_int
  public :: ft_fixed_to_int
  public :: ft_fixed_from_float
  public :: ft_fixed_to_float
  public :: ft_fixed_mul
  public :: ft_fixed_div
  public :: ft_fixed_add
  public :: ft_fixed_sub
  
  ! F26Dot6 conversion functions
  public :: ft_f26dot6_from_int
  public :: ft_f26dot6_to_int
  public :: ft_f26dot6_from_float
  public :: ft_f26dot6_to_float
  public :: ft_f26dot6_from_pixels
  
  ! Constants
  public :: FT_FIXED_ONE
  public :: FT_F26DOT6_ONE
  
  ! Error code constants - matching FreeType's error codes
  public :: FT_Err_Ok
  public :: FT_Err_Cannot_Open_Resource
  public :: FT_Err_Unknown_File_Format
  public :: FT_Err_Invalid_File_Format
  public :: FT_Err_Invalid_Version
  public :: FT_Err_Lower_Module_Version
  public :: FT_Err_Invalid_Argument
  public :: FT_Err_Unimplemented_Feature
  public :: FT_Err_Invalid_Table
  public :: FT_Err_Invalid_Table_Format
  public :: FT_Err_Invalid_Offset
  public :: FT_Err_Array_Too_Large
  public :: FT_Err_Missing_Module
  public :: FT_Err_Missing_Property
  public :: FT_Err_Invalid_Glyph_Index
  public :: FT_Err_Invalid_Character_Code
  public :: FT_Err_Invalid_Glyph_Format
  public :: FT_Err_Cannot_Render_Glyph
  public :: FT_Err_Invalid_Outline
  public :: FT_Err_Invalid_Composite
  public :: FT_Err_Too_Many_Hints
  public :: FT_Err_Invalid_Pixel_Size
  public :: FT_Err_Invalid_SVG_Document
  public :: FT_Err_Invalid_Handle
  public :: FT_Err_Invalid_Library_Handle
  public :: FT_Err_Invalid_Driver_Handle
  public :: FT_Err_Invalid_Face_Handle
  public :: FT_Err_Invalid_Size_Handle
  public :: FT_Err_Invalid_Slot_Handle
  public :: FT_Err_Invalid_CharMap_Handle
  public :: FT_Err_Invalid_Cache_Handle
  public :: FT_Err_Invalid_Stream_Handle
  public :: FT_Err_Too_Many_Drivers
  public :: FT_Err_Too_Many_Extensions
  public :: FT_Err_Out_Of_Memory
  public :: FT_Err_Unlisted_Object
  public :: FT_Err_Cannot_Open_Stream
  public :: FT_Err_Invalid_Stream_Seek
  public :: FT_Err_Invalid_Stream_Skip
  public :: FT_Err_Invalid_Stream_Read

  ! Type definitions
  
  ! FT_Error type - compatible with C int
  integer, parameter :: FT_Error = c_int
  
  ! FT_Fixed: 16.16 fixed-point type (compatible with C signed long)
  integer, parameter :: FT_Fixed = c_long
  
  ! FT_F26Dot6: 26.6 fixed-point type (compatible with C signed long)
  integer, parameter :: FT_F26Dot6 = c_long
  
  ! FT_F2Dot14: 2.14 fixed-point type (compatible with C signed short)
  integer, parameter :: FT_F2Dot14 = c_short
  
  ! Fixed-point constants
  integer(FT_Fixed), parameter :: FT_FIXED_ONE = 65536_c_long  ! 1.0 in 16.16 format
  integer(FT_F26Dot6), parameter :: FT_F26DOT6_ONE = 64_c_long  ! 1.0 in 26.6 format

  ! Error code definitions
  integer(FT_Error), parameter :: FT_Err_Ok                       = 0
  integer(FT_Error), parameter :: FT_Err_Cannot_Open_Resource     = 1
  integer(FT_Error), parameter :: FT_Err_Unknown_File_Format      = 2
  integer(FT_Error), parameter :: FT_Err_Invalid_File_Format      = 3
  integer(FT_Error), parameter :: FT_Err_Invalid_Version          = 4
  integer(FT_Error), parameter :: FT_Err_Lower_Module_Version     = 5
  integer(FT_Error), parameter :: FT_Err_Invalid_Argument         = 6
  integer(FT_Error), parameter :: FT_Err_Unimplemented_Feature    = 7
  integer(FT_Error), parameter :: FT_Err_Invalid_Table            = 8
  integer(FT_Error), parameter :: FT_Err_Invalid_Table_Format     = 9
  integer(FT_Error), parameter :: FT_Err_Invalid_Offset           = 10
  integer(FT_Error), parameter :: FT_Err_Array_Too_Large          = 11
  integer(FT_Error), parameter :: FT_Err_Missing_Module           = 12
  integer(FT_Error), parameter :: FT_Err_Missing_Property         = 13
  
  ! Glyph/character errors
  integer(FT_Error), parameter :: FT_Err_Invalid_Glyph_Index      = 16
  integer(FT_Error), parameter :: FT_Err_Invalid_Character_Code   = 17
  integer(FT_Error), parameter :: FT_Err_Invalid_Glyph_Format     = 18
  integer(FT_Error), parameter :: FT_Err_Cannot_Render_Glyph      = 19
  integer(FT_Error), parameter :: FT_Err_Invalid_Outline          = 20
  integer(FT_Error), parameter :: FT_Err_Invalid_Composite        = 21
  integer(FT_Error), parameter :: FT_Err_Too_Many_Hints           = 22
  integer(FT_Error), parameter :: FT_Err_Invalid_Pixel_Size       = 23
  integer(FT_Error), parameter :: FT_Err_Invalid_SVG_Document     = 24
  
  
  ! Handle errors
  integer(FT_Error), parameter :: FT_Err_Invalid_Handle           = 32
  integer(FT_Error), parameter :: FT_Err_Invalid_Library_Handle   = 33
  integer(FT_Error), parameter :: FT_Err_Invalid_Driver_Handle    = 34
  integer(FT_Error), parameter :: FT_Err_Invalid_Face_Handle      = 35
  integer(FT_Error), parameter :: FT_Err_Invalid_Size_Handle      = 36
  integer(FT_Error), parameter :: FT_Err_Invalid_Slot_Handle      = 37
  integer(FT_Error), parameter :: FT_Err_Invalid_CharMap_Handle   = 38
  integer(FT_Error), parameter :: FT_Err_Invalid_Cache_Handle     = 39
  integer(FT_Error), parameter :: FT_Err_Invalid_Stream_Handle    = 40
  
  ! Driver errors  
  integer(FT_Error), parameter :: FT_Err_Too_Many_Drivers         = 48
  integer(FT_Error), parameter :: FT_Err_Too_Many_Extensions      = 49
  
  ! Memory errors
  integer(FT_Error), parameter :: FT_Err_Out_Of_Memory            = 64
  integer(FT_Error), parameter :: FT_Err_Unlisted_Object          = 65
  
  ! Stream errors
  integer(FT_Error), parameter :: FT_Err_Cannot_Open_Stream       = 81
  integer(FT_Error), parameter :: FT_Err_Invalid_Stream_Seek      = 82
  integer(FT_Error), parameter :: FT_Err_Invalid_Stream_Skip      = 83
  integer(FT_Error), parameter :: FT_Err_Invalid_Stream_Read      = 84

contains

  ! Fixed-point conversion functions
  
  pure function ft_fixed_from_int(value) result(fixed)
    integer, intent(in) :: value
    integer(FT_Fixed) :: fixed
    
    fixed = int(value, FT_Fixed) * FT_FIXED_ONE
  end function ft_fixed_from_int
  
  pure function ft_fixed_to_int(fixed) result(value)
    integer(FT_Fixed), intent(in) :: fixed
    integer :: value
    
    value = int(fixed / FT_FIXED_ONE)
  end function ft_fixed_to_int
  
  pure function ft_fixed_from_float(value) result(fixed)
    real, intent(in) :: value
    integer(FT_Fixed) :: fixed
    
    fixed = int(value * real(FT_FIXED_ONE), FT_Fixed)
  end function ft_fixed_from_float
  
  pure function ft_fixed_to_float(fixed) result(value)
    integer(FT_Fixed), intent(in) :: fixed
    real :: value
    
    value = real(fixed) / real(FT_FIXED_ONE)
  end function ft_fixed_to_float
  
  ! Fixed-point arithmetic functions
  
  pure function ft_fixed_add(a, b) result(c)
    integer(FT_Fixed), intent(in) :: a, b
    integer(FT_Fixed) :: c
    
    c = a + b
  end function ft_fixed_add
  
  pure function ft_fixed_sub(a, b) result(c)
    integer(FT_Fixed), intent(in) :: a, b
    integer(FT_Fixed) :: c
    
    c = a - b
  end function ft_fixed_sub
  
  pure function ft_fixed_mul(a, b) result(c)
    integer(FT_Fixed), intent(in) :: a, b
    integer(FT_Fixed) :: c
    integer(8) :: temp
    
    ! Use 64-bit intermediate to avoid overflow
    temp = int(a, 8) * int(b, 8)
    c = int(temp / FT_FIXED_ONE, FT_Fixed)
  end function ft_fixed_mul
  
  pure function ft_fixed_div(a, b) result(c)
    integer(FT_Fixed), intent(in) :: a, b
    integer(FT_Fixed) :: c
    integer(8) :: temp
    
    ! Use 64-bit intermediate to maintain precision
    temp = int(a, 8) * FT_FIXED_ONE
    c = int(temp / int(b, 8), FT_Fixed)
  end function ft_fixed_div
  
  ! F26Dot6 conversion functions
  
  pure function ft_f26dot6_from_int(value) result(f26dot6)
    integer, intent(in) :: value
    integer(FT_F26Dot6) :: f26dot6
    
    f26dot6 = int(value, FT_F26Dot6) * FT_F26DOT6_ONE
  end function ft_f26dot6_from_int
  
  pure function ft_f26dot6_to_int(f26dot6) result(value)
    integer(FT_F26Dot6), intent(in) :: f26dot6
    integer :: value
    
    value = int(f26dot6 / FT_F26DOT6_ONE)
  end function ft_f26dot6_to_int
  
  pure function ft_f26dot6_from_float(value) result(f26dot6)
    real, intent(in) :: value
    integer(FT_F26Dot6) :: f26dot6
    
    f26dot6 = int(value * real(FT_F26DOT6_ONE), FT_F26Dot6)
  end function ft_f26dot6_from_float
  
  pure function ft_f26dot6_to_float(f26dot6) result(value)
    integer(FT_F26Dot6), intent(in) :: f26dot6
    real :: value
    
    value = real(f26dot6) / real(FT_F26DOT6_ONE)
  end function ft_f26dot6_to_float
  
  pure function ft_f26dot6_from_pixels(pixels) result(f26dot6)
    integer, intent(in) :: pixels
    integer(FT_F26Dot6) :: f26dot6
    
    ! In 26.6 format, pixel coordinates are already in the correct format
    ! Just shift left by 6 bits (multiply by 64)
    f26dot6 = int(pixels, FT_F26Dot6) * FT_F26DOT6_ONE
  end function ft_f26dot6_from_pixels

end module ft_types