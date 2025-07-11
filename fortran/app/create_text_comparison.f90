program create_text_comparison
  use ft_types
  use ft_geometry
  use ft_bitmap_mod
  use ft_bitmap_io
  use ft_outline_mod
  use ft_scanline_simple
  use ft_compare
  use ft_raster
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  
  integer(FT_Error) :: error
  logical :: success
  type(FT_Bitmap) :: bitmap_hello1, bitmap_hello2, bitmap_test1, bitmap_test2
  
  print '("Text Comparison Creator")'
  print '("======================")'
  print '("")'
  
  ! Create "HELLO" in different rendering styles
  print '("Creating HELLO text samples...")'
  
  ! Style 1: Clean antialiased HELLO
  call create_hello_text(bitmap_hello1, "clean", 64, 32)
  success = ft_bitmap_write_png(bitmap_hello1, "text_hello_clean.png", error)
  if (success) print '("  Clean HELLO: text_hello_clean.png")'
  
  ! Style 2: Bold HELLO
  call create_hello_text(bitmap_hello2, "bold", 64, 32)
  success = ft_bitmap_write_png(bitmap_hello2, "text_hello_bold.png", error)
  if (success) print '("  Bold HELLO: text_hello_bold.png")'
  
  ! Create "TEST" in different sizes
  print '("Creating TEST text samples...")'
  
  ! Small TEST
  call create_test_text(bitmap_test1, "small", 48, 24)
  success = ft_bitmap_write_png(bitmap_test1, "text_test_small.png", error)
  if (success) print '("  Small TEST: text_test_small.png")'
  
  ! Large TEST
  call create_test_text(bitmap_test2, "large", 96, 48)
  success = ft_bitmap_write_png(bitmap_test2, "text_test_large.png", error)
  if (success) print '("  Large TEST: text_test_large.png")'
  
  print '("")'
  print '("Creating text comparisons...")'
  
  ! Create side-by-side comparisons
  success = ft_compare_create_side_by_side(bitmap_hello1, bitmap_hello2, &
                                         "text_hello_clean_vs_bold.png", error)
  if (success) print '("  HELLO Clean vs Bold: text_hello_clean_vs_bold.png")'
  
  success = ft_compare_create_side_by_side(bitmap_test1, bitmap_test2, &
                                         "text_test_small_vs_large.png", error)
  if (success) print '("  TEST Small vs Large: text_test_small_vs_large.png")'
  
  ! Create difference images
  success = ft_compare_create_diff_image(bitmap_hello1, bitmap_hello2, &
                                       "text_hello_difference.png", error)
  if (success) print '("  HELLO Difference: text_hello_difference.png")'
  
  ! Create a text showcase grid
  call create_text_showcase()
  
  ! Cleanup
  call ft_bitmap_done(bitmap_hello1)
  call ft_bitmap_done(bitmap_hello2)
  call ft_bitmap_done(bitmap_test1)
  call ft_bitmap_done(bitmap_test2)
  
  print '("")'
  print '("Text comparison complete! Check the generated PNG files.")'
  
contains
  
  subroutine create_hello_text(bitmap, style, width, height)
    type(FT_Bitmap), intent(out) :: bitmap
    character(len=*), intent(in) :: style
    integer, intent(in) :: width, height
    
    type(FT_Outline) :: outline_h, outline_e1, outline_l1, outline_l2, outline_o
    integer(FT_Error) :: error
    logical :: success
    integer :: letter_width, spacing, x_offset
    
    ! Create bitmap
    success = ft_bitmap_new(width, height, FT_PIXEL_MODE_GRAY, bitmap, error)
    if (.not. success) return
    call ft_bitmap_clear(bitmap)
    
    letter_width = width / 6  ! 5 letters + spacing
    spacing = letter_width / 5
    
    ! Create each letter of "HELLO"
    x_offset = spacing
    
    ! H
    call create_letter_h(outline_h, x_offset, style)
    success = ft_render_outline_filled(outline_h, bitmap, error)
    call ft_outline_done(outline_h)
    x_offset = x_offset + letter_width
    
    ! E
    call create_letter_e(outline_e1, x_offset, style)
    success = ft_render_outline_filled(outline_e1, bitmap, error)
    call ft_outline_done(outline_e1)
    x_offset = x_offset + letter_width
    
    ! L
    call create_letter_l(outline_l1, x_offset, style)
    success = ft_render_outline_filled(outline_l1, bitmap, error)
    call ft_outline_done(outline_l1)
    x_offset = x_offset + letter_width
    
    ! L
    call create_letter_l(outline_l2, x_offset, style)
    success = ft_render_outline_filled(outline_l2, bitmap, error)
    call ft_outline_done(outline_l2)
    x_offset = x_offset + letter_width
    
    ! O
    call create_letter_o(outline_o, x_offset, style)
    success = ft_render_outline_filled(outline_o, bitmap, error)
    call ft_outline_done(outline_o)
    
  end subroutine create_hello_text
  
  subroutine create_test_text(bitmap, size_type, width, height)
    type(FT_Bitmap), intent(out) :: bitmap
    character(len=*), intent(in) :: size_type
    integer, intent(in) :: width, height
    
    type(FT_Outline) :: outline_t1, outline_e, outline_s, outline_t2
    integer(FT_Error) :: error
    logical :: success
    integer :: letter_width, spacing, x_offset
    real :: scale_factor
    
    ! Create bitmap
    success = ft_bitmap_new(width, height, FT_PIXEL_MODE_GRAY, bitmap, error)
    if (.not. success) return
    call ft_bitmap_clear(bitmap)
    
    if (size_type == "small") then
      scale_factor = 0.6
    else
      scale_factor = 1.4
    end if
    
    letter_width = int(width * scale_factor / 5)  ! 4 letters + spacing
    spacing = letter_width / 4
    x_offset = spacing
    
    ! Create each letter of "TEST"
    
    ! T
    call create_letter_t(outline_t1, x_offset, size_type)
    success = ft_render_outline_filled(outline_t1, bitmap, error)
    call ft_outline_done(outline_t1)
    x_offset = x_offset + letter_width
    
    ! E
    call create_letter_e(outline_e, x_offset, size_type)
    success = ft_render_outline_filled(outline_e, bitmap, error)
    call ft_outline_done(outline_e)
    x_offset = x_offset + letter_width
    
    ! S
    call create_letter_s(outline_s, x_offset, size_type)
    success = ft_render_outline_filled(outline_s, bitmap, error)
    call ft_outline_done(outline_s)
    x_offset = x_offset + letter_width
    
    ! T
    call create_letter_t(outline_t2, x_offset, size_type)
    success = ft_render_outline_filled(outline_t2, bitmap, error)
    call ft_outline_done(outline_t2)
    
  end subroutine create_test_text
  
  subroutine create_letter_h(outline, x_offset, style)
    type(FT_Outline), intent(out) :: outline
    integer, intent(in) :: x_offset
    character(len=*), intent(in) :: style
    integer(FT_Error) :: error
    logical :: success
    integer :: base_x, stroke_width
    
    base_x = x_offset * 64  ! Convert to 26.6 fixed point
    stroke_width = 192     ! 3 pixels in 26.6
    if (style == "bold") stroke_width = 256  ! 4 pixels
    
    ! Letter H (two vertical strokes + crossbar)
    success = ft_outline_new(8, 1, outline, error)
    if (.not. success) return
    
    ! Left stroke
    outline%points(1) = FT_Vector(base_x, 512)              ! Top left
    outline%points(2) = FT_Vector(base_x + stroke_width, 512) ! Top left inner
    outline%points(3) = FT_Vector(base_x + stroke_width, 1280) ! Crossbar left inner
    outline%points(4) = FT_Vector(base_x + 512, 1280)      ! Crossbar right inner
    outline%points(5) = FT_Vector(base_x + 512, 512)       ! Top right inner
    outline%points(6) = FT_Vector(base_x + 704, 512)       ! Top right
    outline%points(7) = FT_Vector(base_x + 704, 1920)      ! Bottom right
    outline%points(8) = FT_Vector(base_x, 1920)            ! Bottom left
    
    outline%contours(1) = 7  ! 8 points
    outline%tags = FT_CURVE_TAG_ON
    
  end subroutine create_letter_h
  
  subroutine create_letter_e(outline, x_offset, style)
    type(FT_Outline), intent(out) :: outline
    integer, intent(in) :: x_offset
    character(len=*), intent(in) :: style
    integer(FT_Error) :: error
    logical :: success
    integer :: base_x, stroke_width
    
    base_x = x_offset * 64
    stroke_width = 192
    if (style == "bold") stroke_width = 256
    
    ! Letter E (vertical stroke + three horizontal bars)
    success = ft_outline_new(12, 1, outline, error)
    if (.not. success) return
    
    outline%points(1) = FT_Vector(base_x, 512)              ! Top left
    outline%points(2) = FT_Vector(base_x + 640, 512)        ! Top right
    outline%points(3) = FT_Vector(base_x + 640, 768)        ! Top bar bottom
    outline%points(4) = FT_Vector(base_x + stroke_width, 768) ! Back to stem
    outline%points(5) = FT_Vector(base_x + stroke_width, 1152) ! Middle bar top
    outline%points(6) = FT_Vector(base_x + 512, 1152)       ! Middle bar right
    outline%points(7) = FT_Vector(base_x + 512, 1408)       ! Middle bar bottom
    outline%points(8) = FT_Vector(base_x + stroke_width, 1408) ! Back to stem
    outline%points(9) = FT_Vector(base_x + stroke_width, 1664) ! Bottom bar top
    outline%points(10) = FT_Vector(base_x + 640, 1664)      ! Bottom bar right
    outline%points(11) = FT_Vector(base_x + 640, 1920)      ! Bottom right
    outline%points(12) = FT_Vector(base_x, 1920)            ! Bottom left
    
    outline%contours(1) = 11  ! 12 points
    outline%tags = FT_CURVE_TAG_ON
    
  end subroutine create_letter_e
  
  subroutine create_letter_l(outline, x_offset, style)
    type(FT_Outline), intent(out) :: outline
    integer, intent(in) :: x_offset
    character(len=*), intent(in) :: style
    integer(FT_Error) :: error
    logical :: success
    integer :: base_x, stroke_width
    
    base_x = x_offset * 64
    stroke_width = 192
    if (style == "bold") stroke_width = 256
    
    ! Letter L (vertical stroke + bottom bar)
    success = ft_outline_new(6, 1, outline, error)
    if (.not. success) return
    
    outline%points(1) = FT_Vector(base_x, 512)              ! Top left
    outline%points(2) = FT_Vector(base_x + stroke_width, 512) ! Top right
    outline%points(3) = FT_Vector(base_x + stroke_width, 1664) ! Bottom bar top
    outline%points(4) = FT_Vector(base_x + 640, 1664)       ! Bottom bar right
    outline%points(5) = FT_Vector(base_x + 640, 1920)       ! Bottom right
    outline%points(6) = FT_Vector(base_x, 1920)             ! Bottom left
    
    outline%contours(1) = 5  ! 6 points
    outline%tags = FT_CURVE_TAG_ON
    
  end subroutine create_letter_l
  
  subroutine create_letter_o(outline, x_offset, style)
    type(FT_Outline), intent(out) :: outline
    integer, intent(in) :: x_offset
    character(len=*), intent(in) :: style
    integer(FT_Error) :: error
    logical :: success
    integer :: base_x, stroke_width
    
    base_x = x_offset * 64
    stroke_width = 192
    if (style == "bold") stroke_width = 256
    
    ! Letter O (rectangle with hole)
    success = ft_outline_new(8, 1, outline, error)
    if (.not. success) return
    
    outline%points(1) = FT_Vector(base_x, 512)              ! Outer top left
    outline%points(2) = FT_Vector(base_x + 640, 512)        ! Outer top right
    outline%points(3) = FT_Vector(base_x + 640, 1920)       ! Outer bottom right
    outline%points(4) = FT_Vector(base_x, 1920)             ! Outer bottom left
    outline%points(5) = FT_Vector(base_x + stroke_width, 1664) ! Inner bottom left
    outline%points(6) = FT_Vector(base_x + 640 - stroke_width, 1664) ! Inner bottom right
    outline%points(7) = FT_Vector(base_x + 640 - stroke_width, 768) ! Inner top right
    outline%points(8) = FT_Vector(base_x + stroke_width, 768) ! Inner top left
    
    outline%contours(1) = 7  ! 8 points
    outline%tags = FT_CURVE_TAG_ON
    
  end subroutine create_letter_o
  
  subroutine create_letter_t(outline, x_offset, style)
    type(FT_Outline), intent(out) :: outline
    integer, intent(in) :: x_offset
    character(len=*), intent(in) :: style
    integer(FT_Error) :: error
    logical :: success
    integer :: base_x, stroke_width
    
    base_x = x_offset * 64
    stroke_width = 192
    if (style == "bold" .or. style == "large") stroke_width = 256
    
    ! Letter T (top bar + vertical stroke)
    success = ft_outline_new(8, 1, outline, error)
    if (.not. success) return
    
    outline%points(1) = FT_Vector(base_x, 512)              ! Top left
    outline%points(2) = FT_Vector(base_x + 640, 512)        ! Top right
    outline%points(3) = FT_Vector(base_x + 640, 768)        ! Top bar bottom right
    outline%points(4) = FT_Vector(base_x + 320 + stroke_width/2, 768) ! Stem right
    outline%points(5) = FT_Vector(base_x + 320 + stroke_width/2, 1920) ! Bottom right
    outline%points(6) = FT_Vector(base_x + 320 - stroke_width/2, 1920) ! Bottom left
    outline%points(7) = FT_Vector(base_x + 320 - stroke_width/2, 768) ! Stem left
    outline%points(8) = FT_Vector(base_x, 768)              ! Top bar bottom left
    
    outline%contours(1) = 7  ! 8 points
    outline%tags = FT_CURVE_TAG_ON
    
  end subroutine create_letter_t
  
  subroutine create_letter_s(outline, x_offset, style)
    type(FT_Outline), intent(out) :: outline
    integer, intent(in) :: x_offset
    character(len=*), intent(in) :: style
    integer(FT_Error) :: error
    logical :: success
    integer :: base_x, stroke_width
    
    base_x = x_offset * 64
    stroke_width = 192
    if (style == "bold" .or. style == "large") stroke_width = 256
    
    ! Letter S (simplified as two horizontal bars)
    success = ft_outline_new(12, 1, outline, error)
    if (.not. success) return
    
    ! Top curve (simplified as bar)
    outline%points(1) = FT_Vector(base_x, 512)              ! Top left
    outline%points(2) = FT_Vector(base_x + 640, 512)        ! Top right
    outline%points(3) = FT_Vector(base_x + 640, 768)        ! Top bar bottom
    outline%points(4) = FT_Vector(base_x + 320, 768)        ! Middle connection
    outline%points(5) = FT_Vector(base_x + 320, 1152)       ! Middle bar top
    outline%points(6) = FT_Vector(base_x + 640, 1152)       ! Middle bar right
    outline%points(7) = FT_Vector(base_x + 640, 1408)       ! Middle bar bottom
    outline%points(8) = FT_Vector(base_x, 1408)             ! Bottom connection
    outline%points(9) = FT_Vector(base_x, 1664)             ! Bottom bar top
    outline%points(10) = FT_Vector(base_x + 640, 1664)      ! Bottom bar right
    outline%points(11) = FT_Vector(base_x + 640, 1920)      ! Bottom right
    outline%points(12) = FT_Vector(base_x, 1920)            ! Bottom left
    
    outline%contours(1) = 11  ! 12 points
    outline%tags = FT_CURVE_TAG_ON
    
  end subroutine create_letter_s
  
  subroutine create_text_showcase()
    type(FT_Bitmap) :: showcase_bitmap
    integer(FT_Error) :: error
    logical :: success
    
    ! Create a large showcase image with various text samples
    success = ft_bitmap_new(128, 64, FT_PIXEL_MODE_GRAY, showcase_bitmap, error)
    if (.not. success) return
    
    call ft_bitmap_clear(showcase_bitmap)
    
    ! For now, just create a simple pattern
    ! In a full implementation, this would render multiple text samples
    
    success = ft_bitmap_write_png(showcase_bitmap, "text_showcase.png", error)
    if (success) print '("  Text showcase: text_showcase.png")'
    
    call ft_bitmap_done(showcase_bitmap)
    
  end subroutine create_text_showcase
  
end program create_text_comparison