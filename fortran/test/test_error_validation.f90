program test_error_validation
  use ft_types
  use, intrinsic :: iso_c_binding
  implicit none
  
  ! For now, we validate that our error codes follow the pattern from fterrdef.h
  ! We can add C validation later when we have proper FreeType integration
  
  print '("Testing FT_Error code consistency...")'
  
  ! Test that codes are in expected ranges
  call test_error_ranges()
  
  print '("All error validation tests passed!")'

contains

  subroutine test_error_ranges()
    ! Generic errors should be in 0x00-0x0F range
    if (FT_Err_Ok /= 0) error stop "FT_Err_Ok must be 0"
    if (FT_Err_Cannot_Open_Resource < 0 .or. FT_Err_Cannot_Open_Resource > 15) then
      error stop "Generic errors should be in 0x00-0x0F range"
    end if
    
    ! Glyph/character errors should be in 0x10-0x1F range  
    if (FT_Err_Invalid_Glyph_Index < 16 .or. FT_Err_Invalid_Glyph_Index > 31) then
      error stop "Glyph errors should be in 0x10-0x1F range"
    end if
    
    ! Handle errors should be in 0x20-0x2F range
    if (FT_Err_Invalid_Handle < 32 .or. FT_Err_Invalid_Handle > 47) then
      error stop "Handle errors should be in 0x20-0x2F range"
    end if
    
    ! Driver errors should be in 0x30-0x3F range
    if (FT_Err_Too_Many_Drivers < 48 .or. FT_Err_Too_Many_Drivers > 63) then
      error stop "Driver errors should be in 0x30-0x3F range"
    end if
    
    ! Memory errors should be in 0x40-0x4F range
    if (FT_Err_Out_Of_Memory < 64 .or. FT_Err_Out_Of_Memory > 79) then
      error stop "Memory errors should be in 0x40-0x4F range"
    end if
    
    ! Stream errors should be in 0x51+ range
    if (FT_Err_Cannot_Open_Stream < 81) then
      error stop "Stream errors should be in 0x51+ range"
    end if
    
    print '("PASS: Error code ranges are correct")'
  end subroutine test_error_ranges

end program test_error_validation