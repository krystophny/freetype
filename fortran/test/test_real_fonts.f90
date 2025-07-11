program test_real_fonts
  use ft_types
  use tt_types, only: FT_UShort, FT_ULong, FT_Long
  use ft_face
  use ft_face_unified, only: FT_Unified_Face, ft_new_unified_face, ft_done_unified_face, &
                             ft_unified_get_glyph_index
  use ft_font_format, only: FT_FONT_FORMAT_UNKNOWN, FT_FONT_FORMAT_TRUETYPE, &
                            FT_FONT_FORMAT_CFF, FT_FONT_FORMAT_TYPE1
  use ft_bitmap_mod, only: FT_Bitmap, ft_bitmap_new, ft_bitmap_done, ft_bitmap_get_pixel, &
                           FT_PIXEL_MODE_MONO
  use ft_outline_mod, only: FT_Outline, ft_outline_new, ft_outline_done, FT_CURVE_TAG_ON
  use ft_geometry, only: FT_Vector
  use ft_raster
  use ft_bitmap_io, only: ft_bitmap_write_pbm
  use, intrinsic :: iso_c_binding
  implicit none
  
  type(FT_Face_Type) :: tt_face
  type(FT_Unified_Face) :: face
  integer(FT_Error) :: error
  logical :: success
  integer :: i
  character(len=256) :: font_file
  
  print '("Real Font Loading Tests")'
  print '("======================")'
  print '()'
  
  ! Test 1: Load test_font.ttf
  font_file = "test_font.ttf"
  print '("Test 1: Loading ", A)', trim(font_file)
  
  ! Try loading as TrueType
  success = ft_new_face(font_file, int(0, FT_Long), tt_face, error)
  if (success) then
    print '("  SUCCESS: Loaded as TrueType font")'
    print '("  Family name: ", A)', trim(tt_face%family_name)
    print '("  Style name: ", A)', trim(tt_face%style_name)
    print '("  Number of glyphs: ", I0)', tt_face%num_glyphs
    print '("  Units per EM: ", I0)', tt_face%units_per_em
    print '("  Number of charmaps: ", I0)', tt_face%num_charmaps
    
    ! Get some glyph indices
    print '("  Sample character mappings:")'
    print '("    ''A'' (65) -> glyph ", I0)', ft_get_char_index(tt_face, int(65, FT_ULong))
    print '("    ''a'' (97) -> glyph ", I0)', ft_get_char_index(tt_face, int(97, FT_ULong))
    print '("    ''0'' (48) -> glyph ", I0)', ft_get_char_index(tt_face, int(48, FT_ULong))
    print '("    space (32) -> glyph ", I0)', ft_get_char_index(tt_face, int(32, FT_ULong))
    
    call ft_done_face(tt_face)
  else
    print '("  FAILED: Could not load as TrueType, error=", I0)', error
  end if
  
  print '()'
  
  ! Test 2: Load using unified face
  print '("Test 2: Loading with unified face interface")'
  success = ft_new_unified_face(font_file, 0, face, error)
  if (success) then
    print '("  SUCCESS: Loaded with unified interface")'
    select case (face%font_format)
    case (FT_FONT_FORMAT_TRUETYPE)
      print '("  Font format: TrueType")'
    case (FT_FONT_FORMAT_CFF)
      print '("  Font format: CFF/OpenType")'
    case (FT_FONT_FORMAT_TYPE1)
      print '("  Font format: Type 1")'
    case default
      print '("  Font format: Unknown (", I0, ")")', face%font_format
    end select
    print '("  Family name: ", A)', trim(face%family_name)
    print '("  Number of glyphs: ", I0)', face%num_glyphs
    
    ! Try rendering a simple glyph
    call test_glyph_rendering(face)
    
    call ft_done_unified_face(face)
  else
    print '("  FAILED: Could not load with unified interface, error=", I0)', error
  end if
  
  print '()'
  print '("Test completed")'
  
contains

  subroutine test_glyph_rendering(face)
    type(FT_Unified_Face), intent(inout) :: face
    type(FT_Outline), target :: outline
    type(FT_Bitmap), target :: bitmap
    integer(FT_UShort) :: glyph_index
    integer :: x, y
    character(len=1) :: pixel
    logical :: success
    
    print '()'
    print '("  Attempting to render glyph ''A''...")'
    
    ! Get glyph index for 'A'
    glyph_index = ft_unified_get_glyph_index(face, int(65, FT_ULong))
    print '("    Glyph index for ''A'': ", I0)', glyph_index
    
    if (glyph_index == 0) then
      print '("    No glyph found for character ''A''")'
      return
    end if
    
    ! For now, create a simple test outline since glyph loading isn't implemented yet
    success = ft_outline_new(4, 1, outline, error)
    if (.not. success) return
    
    ! Create a simple 'A' shape
    outline%points(1)%x = 100
    outline%points(1)%y = 100   ! Bottom left
    outline%points(2)%x = 300
    outline%points(2)%y = 700   ! Top
    outline%points(3)%x = 500
    outline%points(3)%y = 100   ! Bottom right
    outline%points(4)%x = 200
    outline%points(4)%y = 300   ! Crossbar left
    outline%contours(1) = 3
    outline%tags = FT_CURVE_TAG_ON
    
    ! Create bitmap
    success = ft_bitmap_new(16, 16, FT_PIXEL_MODE_MONO, bitmap, error)
    if (.not. success) then
      call ft_outline_done(outline)
      return
    end if
    
    ! Render outline
    success = ft_raster_render_outline_scanline(outline, bitmap, error)
    if (success) then
      print '("    Rendered successfully to 16x16 bitmap")'
      print '("    Bitmap preview:")'
      
      ! Show bitmap
      do y = 0, min(15, bitmap%rows - 1)
        write(*, '("      ")', advance='no')
        do x = 0, min(15, bitmap%width - 1)
          if (ft_bitmap_get_pixel(bitmap, x, y)) then
            pixel = '*'
          else
            pixel = '.'
          end if
          write(*, '(A)', advance='no') pixel
        end do
        write(*, *)
      end do
      
      ! Save to file
      if (ft_bitmap_write_pbm(bitmap, "test_real_font_A.pbm", error)) then
        print '("    Saved bitmap to test_real_font_A.pbm")'
      end if
    else
      print '("    Rendering failed, error=", I0)', error
    end if
    
    call ft_bitmap_done(bitmap)
    call ft_outline_done(outline)
    
  end subroutine test_glyph_rendering

end program test_real_fonts