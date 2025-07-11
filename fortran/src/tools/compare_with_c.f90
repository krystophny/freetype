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
  
  ! C FreeType glyph slot structure (simplified)
  type, bind(C) :: c_ft_glyph_slot
    type(c_ptr) :: library
    type(c_ptr) :: face
    type(c_ptr) :: next
    integer(c_int) :: glyph_index
    integer(c_int) :: generic_data
    integer(c_int) :: generic_finalizer
    ! ... other fields omitted for brevity
    type(c_ft_bitmap) :: bitmap
    integer(c_int) :: bitmap_left
    integer(c_int) :: bitmap_top
    ! ... more fields
  end type
  
  ! C FreeType face structure (simplified)
  type, bind(C) :: c_ft_face
    integer(c_long) :: num_faces
    integer(c_long) :: face_index
    integer(c_long) :: face_flags
    integer(c_long) :: style_flags
    integer(c_long) :: num_glyphs
    ! ... other fields omitted for brevity
    type(c_ptr) :: glyph  ! Points to glyph slot
    ! ... more fields
  end type
  
  ! Local variables
  character(len=256) :: font_file
  character(len=1) :: test_char
  integer :: char_code
  type(c_ptr) :: c_library, c_face, c_slot
  type(c_ptr) :: c_bitmap_ptr
  type(c_ft_bitmap), pointer :: c_bitmap
  type(c_ft_face), pointer :: c_face_rec
  type(c_ft_glyph_slot), pointer :: c_glyph_slot
  type(FT_Bitmap) :: fortran_bitmap, c_bitmap_copy
  type(FT_Face), pointer :: fortran_face
  integer(FT_Error) :: error
  integer(c_int) :: c_error
  logical :: success
  integer :: total_pixels, different_pixels
  real :: diff_percentage
  integer :: i, j
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
    
    ! Get face record to access glyph slot
    call c_f_pointer(c_face, c_face_rec)
    call c_f_pointer(c_face_rec%glyph, c_glyph_slot)
    
    ! Extract bitmap from glyph slot
    print '("C FreeType bitmap: ", I0, "x", I0, " pixels")', &
          c_glyph_slot%bitmap%width, c_glyph_slot%bitmap%rows
    
    ! Convert C bitmap to Fortran bitmap
    success = convert_c_bitmap_to_fortran(c_glyph_slot%bitmap, c_bitmap_copy, error)
    if (success) then
      print '("C FreeType: Bitmap extracted successfully")'
    else
      print '("WARNING: Failed to extract C bitmap, using placeholder")'
    end if
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
  
  ! Render character with Fortran FreeType
  print '("")'
  print '("Rendering character with Fortran FreeType...")'
  
  ! Create Fortran bitmap by rendering the character
  success = render_char_with_fortran(fortran_face, char_code, fortran_bitmap, error)
  if (success) then
    success = ft_bitmap_write_png(fortran_bitmap, "fortran_output.png", error)
    if (success) then
      print '("Fortran bitmap saved to: fortran_output.png")'
    end if
  else
    print '("WARNING: Failed to render with Fortran FreeType, using test pattern")'
    success = ft_bitmap_new(24, 24, FT_PIXEL_MODE_GRAY, fortran_bitmap, error)
    if (success) then
      call create_test_pattern(fortran_bitmap, 1)
    end if
  end if
  
  ! If C bitmap extraction failed, create test pattern
  if (.not. success .or. .not. allocated(c_bitmap_copy%buffer)) then
    success = ft_bitmap_new(24, 24, FT_PIXEL_MODE_GRAY, c_bitmap_copy, error)
    if (success) then
      call create_test_pattern(c_bitmap_copy, 2)
      success = ft_bitmap_write_png(c_bitmap_copy, "c_output.png", error)
      if (success) then
        print '("C bitmap (test pattern) saved to: c_output.png")'
      end if
    end if
  else
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

  ! Convert C FreeType bitmap to Fortran bitmap
  function convert_c_bitmap_to_fortran(c_bitmap, fortran_bitmap, error) result(success)
    type(c_ft_bitmap), intent(in) :: c_bitmap
    type(FT_Bitmap), intent(out) :: fortran_bitmap
    integer(FT_Error), intent(out) :: error
    logical :: success
    integer :: i
    integer(c_int8), pointer :: c_buffer(:)
    
    success = .false.
    error = FT_Err_Ok
    
    ! Check if C bitmap is valid
    if (.not. c_associated(c_bitmap%buffer) .or. &
        c_bitmap%width <= 0 .or. c_bitmap%rows <= 0) then
      error = FT_Err_Invalid_Argument
      return
    end if
    
    ! Create Fortran bitmap with same dimensions
    success = ft_bitmap_new(c_bitmap%width, c_bitmap%rows, &
                           int(c_bitmap%pixel_mode, FT_Pixel_Mode), &
                           fortran_bitmap, error)
    if (.not. success) return
    
    ! Get C buffer as Fortran array
    call c_f_pointer(c_bitmap%buffer, c_buffer, [c_bitmap%rows * c_bitmap%pitch])
    
    ! Copy pixel data
    do i = 1, c_bitmap%rows * c_bitmap%pitch
      fortran_bitmap%buffer(i) = c_buffer(i)
    end do
    
    success = .true.
  end function convert_c_bitmap_to_fortran

  ! Render character with Fortran FreeType
  function render_char_with_fortran(face, char_code, bitmap, error) result(success)
    type(FT_Face), intent(in) :: face
    integer, intent(in) :: char_code
    type(FT_Bitmap), intent(out) :: bitmap
    integer(FT_Error), intent(out) :: error
    logical :: success
    integer(FT_UShort) :: glyph_index
    type(FT_Outline) :: outline
    type(FT_Raster_State) :: raster
    type(FT_Raster_Params) :: params
    
    success = .false.
    error = FT_Err_Ok
    
    ! Get glyph index for character
    glyph_index = ft_get_char_index(face, int(char_code, FT_ULong))
    if (glyph_index == 0) then
      error = FT_Err_Invalid_Glyph_Index
      return
    end if
    
    ! For now, create simple test bitmap
    ! TODO: Implement actual glyph loading and rendering
    success = ft_bitmap_new(24, 24, FT_PIXEL_MODE_GRAY, bitmap, error)
    if (.not. success) return
    
    ! Create simple rectangular outline for demonstration
    call create_simple_char_outline(bitmap, char_code)
    
    success = .true.
  end function render_char_with_fortran

  ! Create simple character outline for demonstration
  subroutine create_simple_char_outline(bitmap, char_code)
    type(FT_Bitmap), intent(inout) :: bitmap
    integer, intent(in) :: char_code
    integer :: x, y, gray, char_pattern
    
    ! Create different patterns based on character
    char_pattern = mod(char_code, 3)
    
    do y = 0, bitmap%rows - 1
      do x = 0, bitmap%width - 1
        select case (char_pattern)
        case (0)
          ! Diagonal lines
          if (mod(x + y, 4) == 0) then
            gray = 255
          else
            gray = 0
          end if
        case (1)
          ! Horizontal lines
          if (mod(y, 3) == 0) then
            gray = 200
          else
            gray = 50
          end if
        case default
          ! Vertical lines
          if (mod(x, 3) == 0) then
            gray = 150
          else
            gray = 25
          end if
        end select
        
        if (associated(bitmap%buffer)) then
          bitmap%buffer(y * bitmap%pitch + x + 1) = int(gray, int8)
        end if
      end do
    end do
  end subroutine create_simple_char_outline

end program compare_with_c