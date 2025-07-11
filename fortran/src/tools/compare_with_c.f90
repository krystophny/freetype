program compare_with_c
  use ft_types
  use ft_bitmap_mod
  use ft_bitmap_io
  use ft_compare
  use ft_face
  use ft_outline_mod
  use ft_scanline_simple
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  
  ! ISO C interface to original FreeType
  interface
    function c_ft_init_freetype(library) bind(C, name="FT_Init_FreeType")
      import :: c_ptr, c_int
      type(c_ptr), intent(out) :: library
      integer(c_int) :: c_ft_init_freetype
    end function
    
    function c_ft_new_face(library, filename, face_index, face) bind(C, name="FT_New_Face")
      import :: c_ptr, c_char, c_long, c_int
      type(c_ptr), value :: library
      character(c_char), intent(in) :: filename(*)
      integer(c_long), value :: face_index
      type(c_ptr), intent(out) :: face
      integer(c_int) :: c_ft_new_face
    end function
    
    function c_ft_set_char_size(face, char_width, char_height, horz_res, vert_res) bind(C, name="FT_Set_Char_Size")
      import :: c_ptr, c_long, c_int
      type(c_ptr), value :: face
      integer(c_long), value :: char_width, char_height
      integer(c_int), value :: horz_res, vert_res
      integer(c_int) :: c_ft_set_char_size
    end function
    
    function c_ft_load_char(face, char_code, load_flags) bind(C, name="FT_Load_Char")
      import :: c_ptr, c_long, c_int32_t, c_int
      type(c_ptr), value :: face
      integer(c_long), value :: char_code
      integer(c_int32_t), value :: load_flags
      integer(c_int) :: c_ft_load_char
    end function
    
    function c_ft_render_glyph(slot, render_mode) bind(C, name="FT_Render_Glyph")
      import :: c_ptr, c_int
      type(c_ptr), value :: slot
      integer(c_int), value :: render_mode
      integer(c_int) :: c_ft_render_glyph
    end function
    
    subroutine c_ft_done_face(face) bind(C, name="FT_Done_Face")
      import :: c_ptr
      type(c_ptr), value :: face
    end subroutine
    
    subroutine c_ft_done_freetype(library) bind(C, name="FT_Done_FreeType")
      import :: c_ptr
      type(c_ptr), value :: library
    end subroutine
  end interface
  
  ! C structure accessors (simplified)
  type, bind(C) :: c_ft_bitmap
    integer(c_int) :: rows
    integer(c_int) :: width
    integer(c_int) :: pitch
    type(c_ptr) :: buffer
    integer(c_short) :: num_grays
    integer(c_char) :: pixel_mode
    integer(c_char) :: palette_mode
    type(c_ptr) :: palette
  end type
  
  ! Local variables
  character(len=256) :: font_file
  character(len=1) :: test_char
  integer :: char_code
  type(c_ptr) :: c_library, c_face, c_slot
  type(c_ptr) :: c_bitmap_ptr
  type(c_ft_bitmap), pointer :: c_bitmap
  type(FT_Bitmap) :: fortran_bitmap, c_bitmap_copy
  type(FT_Face), pointer :: fortran_face
  integer(FT_Error) :: error
  integer(c_int) :: c_error
  logical :: success
  integer :: total_pixels, different_pixels
  real :: diff_percentage
  integer :: i
  integer(c_int8), pointer :: c_buffer(:)
  
  ! Get font file from command line or use default
  if (command_argument_count() >= 1) then
    call get_command_argument(1, font_file)
  else
    font_file = "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"
  end if
  
  ! Get test character
  if (command_argument_count() >= 2) then
    call get_command_argument(2, test_char)
    char_code = iachar(test_char)
  else
    char_code = iachar('A')
  end if
  
  print '("Comparing Fortran vs C FreeType rendering")'
  print '("Font file: ", A)', trim(font_file)
  print '("Test character: ", A, " (code ", I0, ")")', char(char_code), char_code
  print '("")'
  
  ! Initialize C FreeType
  c_error = c_ft_init_freetype(c_library)
  if (c_error /= 0) then
    print '("ERROR: Failed to initialize C FreeType")'
    stop 1
  end if
  
  ! Load font with C FreeType
  c_error = c_ft_new_face(c_library, trim(font_file)//c_null_char, 0_c_long, c_face)
  if (c_error /= 0) then
    print '("ERROR: Failed to load font with C FreeType")'
    call c_ft_done_freetype(c_library)
    stop 1
  end if
  
  ! Set character size (16pt at 72 DPI)
  c_error = c_ft_set_char_size(c_face, 0_c_long, 16*64_c_long, 72, 72)
  if (c_error /= 0) then
    print '("ERROR: Failed to set char size in C FreeType")'
  end if
  
  ! Load and render character with C FreeType
  c_error = c_ft_load_char(c_face, int(char_code, c_long), 4)  ! FT_LOAD_RENDER
  if (c_error /= 0) then
    print '("ERROR: Failed to load character with C FreeType")'
  else
    print '("C FreeType: Character loaded successfully")'
    
    ! Get glyph slot and bitmap
    ! Note: This is simplified - in real code we'd properly access the face structure
    print '("WARNING: C bitmap extraction not fully implemented")'
    print '("         Creating placeholder C bitmap for demonstration")'
  end if
  
  ! Initialize Fortran FreeType
  success = ft_new_face(font_file, 0_FT_Long, fortran_face, error)
  if (.not. success) then
    print '("ERROR: Failed to load font with Fortran FreeType: ", I0)', error
    call c_ft_done_face(c_face)
    call c_ft_done_freetype(c_library)
    stop 1
  end if
  
  ! Get glyph for character
  print '("Fortran FreeType: Font loaded successfully")'
  print '("  Family: ", A)', trim(fortran_face%family_name)
  print '("  Glyphs: ", I0)', fortran_face%num_glyphs
  
  ! For now, create test bitmaps to demonstrate comparison functionality
  print '("")'
  print '("Creating test bitmaps for comparison demonstration...")'
  
  ! Create Fortran test bitmap
  success = ft_bitmap_new(24, 24, FT_PIXEL_MODE_GRAY, fortran_bitmap, error)
  if (success) then
    ! Create simple pattern
    call create_test_pattern(fortran_bitmap, 1)
    success = ft_bitmap_write_png(fortran_bitmap, "fortran_output.png", error)
    if (success) then
      print '("Fortran bitmap saved to: fortran_output.png")'
    end if
  end if
  
  ! Create C test bitmap (simulated)
  success = ft_bitmap_new(24, 24, FT_PIXEL_MODE_GRAY, c_bitmap_copy, error)
  if (success) then
    ! Create slightly different pattern
    call create_test_pattern(c_bitmap_copy, 2)
    success = ft_bitmap_write_png(c_bitmap_copy, "c_output.png", error)
    if (success) then
      print '("C bitmap saved to: c_output.png")'
    end if
  end if
  
  ! Create side-by-side comparison
  if (success) then
    success = ft_compare_create_side_by_side(fortran_bitmap, c_bitmap_copy, &
                                           "comparison_side_by_side.png", error)
    if (success) then
      print '("")'
      print '("Side-by-side comparison saved to: comparison_side_by_side.png")'
    end if
    
    ! Create difference image
    success = ft_compare_create_diff_image(fortran_bitmap, c_bitmap_copy, &
                                         "comparison_diff.png", error)
    if (success) then
      print '("Difference image saved to: comparison_diff.png")'
    end if
    
    ! Calculate statistics
    success = ft_compare_pixel_difference(fortran_bitmap, c_bitmap_copy, &
                                        total_pixels, different_pixels, error)
    if (success) then
      diff_percentage = (real(different_pixels) / real(total_pixels)) * 100.0
      print '("")'
      print '("Pixel comparison statistics:")'
      print '("  Total pixels: ", I0)', total_pixels
      print '("  Different pixels: ", I0)', different_pixels
      print '("  Difference: ", F5.1, "%")', diff_percentage
      
      if (different_pixels == 0) then
        print '("")'
        print '("SUCCESS: Pixel-perfect match!")'
      else if (diff_percentage < 1.0) then
        print '("")'
        print '("GOOD: Less than 1% difference")'
      else if (diff_percentage < 5.0) then
        print '("")'
        print '("ACCEPTABLE: Less than 5% difference")'
      else
        print '("")'
        print '("WARNING: Significant differences detected")'
      end if
    end if
  end if
  
  ! Cleanup
  call ft_bitmap_done(fortran_bitmap)
  call ft_bitmap_done(c_bitmap_copy)
  call ft_done_face(fortran_face)
  call c_ft_done_face(c_face)
  call c_ft_done_freetype(c_library)
  
  print '("")'
  print '("Comparison complete.")'

contains

  subroutine create_test_pattern(bitmap, pattern_type)
    type(FT_Bitmap), intent(inout) :: bitmap
    integer, intent(in) :: pattern_type
    integer :: x, y, gray
    
    select case (pattern_type)
    case (1)
      ! Circular gradient
      do y = 0, bitmap%rows - 1
        do x = 0, bitmap%width - 1
          gray = int(sqrt(real((x - 12)**2 + (y - 12)**2)) * 10)
          gray = min(255, gray)
          if (associated(bitmap%buffer)) then
            bitmap%buffer(y * bitmap%pitch + x + 1) = int(gray, int8)
          end if
        end do
      end do
      
    case (2)
      ! Similar but slightly offset circular gradient
      do y = 0, bitmap%rows - 1
        do x = 0, bitmap%width - 1
          gray = int(sqrt(real((x - 11)**2 + (y - 11)**2)) * 10)
          gray = min(255, gray)
          if (associated(bitmap%buffer)) then
            bitmap%buffer(y * bitmap%pitch + x + 1) = int(gray, int8)
          end if
        end do
      end do
      
    case default
      ! Simple gradient
      do y = 0, bitmap%rows - 1
        do x = 0, bitmap%width - 1
          gray = (x * 255) / (bitmap%width - 1)
          if (associated(bitmap%buffer)) then
            bitmap%buffer(y * bitmap%pitch + x + 1) = int(gray, int8)
          end if
        end do
      end do
    end select
    
  end subroutine create_test_pattern

end program compare_with_c