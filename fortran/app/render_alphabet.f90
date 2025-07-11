program render_alphabet
  use ft_types
  use tt_types
  use ft_geometry
  use ft_face
  use ft_bitmap_mod
  use ft_bitmap_io
  use ft_outline_mod
  use ft_scanline_simple
  use ft_compare
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  
  character(len=256) :: font_file
  type(FT_Face_Type), target :: face
  type(FT_Bitmap) :: bitmap, atlas
  type(FT_Outline), target :: test_outline
  integer(FT_Error) :: error
  logical :: success
  integer :: i, char_code
  character(len=1) :: ch
  integer :: glyph_index
  integer :: x_offset, y_offset
  integer :: char_width, char_height
  integer :: atlas_width, atlas_height
  integer :: chars_per_row
  
  ! Get font file from command line or use default
  if (command_argument_count() >= 1) then
    call get_command_argument(1, font_file)
  else
    font_file = "minimal.ttf"
  end if
  
  print '("FreeType Fortran - Alphabet Renderer")'
  print '("====================================")'
  print '("Font: ", A)', trim(font_file)
  print '("")'
  
  ! Load font (currently using placeholder outlines)
  print '("NOTE: Using placeholder letter outlines for demonstration")'
  print '("      Real font loading to be implemented")'
  print '("")'
  
  ! For now, create hardcoded letter outlines as placeholders
  ! until full glyph loading is implemented
  
  ! Set up atlas dimensions (8x4 grid for 26 letters + extras)
  chars_per_row = 8
  char_width = 32
  char_height = 32
  atlas_width = char_width * chars_per_row
  atlas_height = char_height * 4
  
  ! Create atlas bitmap
  success = ft_bitmap_new(atlas_width, atlas_height, FT_PIXEL_MODE_GRAY, atlas, error)
  if (.not. success) then
    print '("ERROR: Failed to create atlas bitmap")'
    stop 1
  end if
  
  ! Clear atlas (white background)
  call ft_bitmap_clear(atlas)
  
  print '("Rendering alphabet...")'
  
  ! Render A-Z
  do i = 0, 25
    char_code = iachar('A') + i
    ch = char(char_code)
    
    ! Get glyph index for character (placeholder)
    glyph_index = char_code  ! Using char code as glyph index for now
    
    ! Calculate position in atlas
    x_offset = mod(i, chars_per_row) * char_width
    y_offset = (i / chars_per_row) * char_height
    
    ! Create a simple test outline for the letter
    ! (In real implementation, this would load from the font)
    call create_test_letter_outline(ch, test_outline, error)
    
    if (error == FT_Err_Ok) then
      ! Create temporary bitmap for this character
      success = ft_bitmap_new(char_width, char_height, FT_PIXEL_MODE_GRAY, bitmap, error)
      if (success) then
        ! Render the outline
        success = ft_render_outline_filled(test_outline, bitmap, error)
        
        if (success) then
          ! Copy to atlas at correct position
          call copy_bitmap_to_atlas(bitmap, atlas, x_offset, y_offset)
          print '("  ", A, " - rendered (glyph index: ", I0, ")")', ch, glyph_index
        else
          print '("  ", A, " - render failed")', ch
        end if
        
        call ft_bitmap_done(bitmap)
      end if
      
      call ft_outline_done(test_outline)
    else
      print '("  ", A, " - outline creation failed")', ch
    end if
  end do
  
  ! Save the atlas
  print '("")'
  print '("Saving alphabet atlas...")'
  
  success = ft_bitmap_write_png(atlas, "alphabet_atlas.png", error)
  if (success) then
    print '("  PNG saved: alphabet_atlas.png")'
  end if
  
  success = ft_bitmap_write_pgm(atlas, "alphabet_atlas.pgm", error)
  if (success) then
    print '("  PGM saved: alphabet_atlas.pgm")'
  end if
  
  ! Also create individual letter samples
  print '("")'
  print '("Creating individual letter samples...")'
  
  ! Render a few key letters individually
  do i = 1, 5
    select case (i)
    case (1)
      ch = 'A'
    case (2)
      ch = 'B'
    case (3)
      ch = 'H'
    case (4)
      ch = 'O'
    case (5)
      ch = 'Z'
    end select
    
    call create_test_letter_outline(ch, test_outline, error)
    if (error == FT_Err_Ok) then
      success = ft_bitmap_new(64, 64, FT_PIXEL_MODE_GRAY, bitmap, error)
      if (success) then
        success = ft_render_outline_filled(test_outline, bitmap, error)
        if (success) then
          success = ft_bitmap_write_png(bitmap, "letter_" // ch // ".png", error)
          if (success) then
            print '("  Letter ", A, " saved")', ch
          end if
        end if
        call ft_bitmap_done(bitmap)
      end if
      call ft_outline_done(test_outline)
    end if
  end do
  
  ! Cleanup
  call ft_bitmap_done(atlas)
  
  print '("")'
  print '("Alphabet rendering complete!")'

contains

  subroutine create_test_letter_outline(letter, outline, error)
    character(len=1), intent(in) :: letter
    type(FT_Outline), intent(out) :: outline
    integer, intent(out) :: error
    
    error = FT_Err_Ok
    
    ! Create different outlines for different letters
    select case (letter)
    case ('A')
      if (.not. ft_outline_new(7, 2, outline, error)) return
      ! Outer contour
      outline%points(1) = FT_Vector(512, 3584)
      outline%points(2) = FT_Vector(1280, 512)
      outline%points(3) = FT_Vector(1792, 512)
      outline%points(4) = FT_Vector(2560, 3584)
      outline%contours(1) = 3
      ! Inner triangle (crossbar)
      outline%points(5) = FT_Vector(1024, 2048)
      outline%points(6) = FT_Vector(2048, 2048)
      outline%points(7) = FT_Vector(1536, 1536)
      outline%contours(2) = 6
      outline%n_points = 7
      outline%n_contours = 2
      
    case ('B')
      if (.not. ft_outline_new(8, 1, outline, error)) return
      outline%points(1) = FT_Vector(512, 3584)
      outline%points(2) = FT_Vector(512, 512)
      outline%points(3) = FT_Vector(2048, 512)
      outline%points(4) = FT_Vector(2560, 1024)
      outline%points(5) = FT_Vector(2048, 1792)
      outline%points(6) = FT_Vector(2560, 2560)
      outline%points(7) = FT_Vector(2048, 3584)
      outline%points(8) = FT_Vector(512, 3584)
      outline%contours(1) = 7
      outline%n_points = 8
      outline%n_contours = 1
      
    case ('H')
      if (.not. ft_outline_new(12, 1, outline, error)) return
      outline%points(1) = FT_Vector(512, 3584)
      outline%points(2) = FT_Vector(512, 512)
      outline%points(3) = FT_Vector(1024, 512)
      outline%points(4) = FT_Vector(1024, 1792)
      outline%points(5) = FT_Vector(2048, 1792)
      outline%points(6) = FT_Vector(2048, 512)
      outline%points(7) = FT_Vector(2560, 512)
      outline%points(8) = FT_Vector(2560, 3584)
      outline%points(9) = FT_Vector(2048, 3584)
      outline%points(10) = FT_Vector(2048, 2304)
      outline%points(11) = FT_Vector(1024, 2304)
      outline%points(12) = FT_Vector(1024, 3584)
      outline%contours(1) = 11
      outline%n_points = 12
      outline%n_contours = 1
      
    case ('O')
      if (.not. ft_outline_new(8, 1, outline, error)) return
      ! Simple octagon for O
      outline%points(1) = FT_Vector(1024, 3584)
      outline%points(2) = FT_Vector(512, 2816)
      outline%points(3) = FT_Vector(512, 1280)
      outline%points(4) = FT_Vector(1024, 512)
      outline%points(5) = FT_Vector(2048, 512)
      outline%points(6) = FT_Vector(2560, 1280)
      outline%points(7) = FT_Vector(2560, 2816)
      outline%points(8) = FT_Vector(2048, 3584)
      outline%contours(1) = 7
      outline%n_points = 8
      outline%n_contours = 1
      
    case default
      ! Simple rectangle for other letters
      if (.not. ft_outline_new(4, 1, outline, error)) return
      outline%points(1) = FT_Vector(768, 3072)
      outline%points(2) = FT_Vector(768, 1024)
      outline%points(3) = FT_Vector(2304, 1024)
      outline%points(4) = FT_Vector(2304, 3072)
      outline%contours(1) = 3
      outline%n_points = 4
      outline%n_contours = 1
    end select
    
    ! Set all points as on-curve
    outline%tags = FT_CURVE_TAG_ON
    
  end subroutine create_test_letter_outline
  
  subroutine copy_bitmap_to_atlas(src, atlas, x_off, y_off)
    type(FT_Bitmap), intent(in) :: src
    type(FT_Bitmap), intent(inout) :: atlas
    integer, intent(in) :: x_off, y_off
    
    integer :: x, y, src_idx, dst_idx
    
    do y = 0, src%rows - 1
      do x = 0, src%width - 1
        if (x_off + x < atlas%width .and. y_off + y < atlas%rows) then
          src_idx = y * abs(src%pitch) + x + 1
          dst_idx = (y_off + y) * abs(atlas%pitch) + (x_off + x) + 1
          
          if (src_idx <= size(src%buffer) .and. dst_idx <= size(atlas%buffer)) then
            atlas%buffer(dst_idx) = src%buffer(src_idx)
          end if
        end if
      end do
    end do
    
  end subroutine copy_bitmap_to_atlas

end program render_alphabet