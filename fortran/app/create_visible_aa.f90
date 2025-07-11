program create_visible_aa
  use ft_types
  use ft_bitmap_mod
  use ft_outline_mod
  use ft_raster
  use ft_raster_types
  use ft_geometry, only: FT_Vector
  use ft_bitmap_io
  use, intrinsic :: iso_fortran_env, only: int8
  implicit none
  
  type(FT_Outline), target :: outline
  type(FT_Bitmap), target :: mono_bitmap, aa_bitmap
  type(FT_Raster_State) :: raster
  type(FT_Raster_Params) :: params
  integer(FT_Error) :: error
  logical :: success
  integer :: i, val
  
  print '("Creating Visible Antialiasing Comparison")'
  print '("========================================")'
  
  ! Create a nice test shape
  success = ft_outline_new(4, 1, outline, error)
  if (.not. success) stop 1
  
  ! Rectangle with half-pixel offset to guarantee AA
  outline%points(1) = FT_Vector(5 * 64 + 32, 5 * 64 + 32)   ! 5.5, 5.5
  outline%points(2) = FT_Vector(26 * 64 + 32, 5 * 64 + 32)  ! 26.5, 5.5
  outline%points(3) = FT_Vector(26 * 64 + 32, 26 * 64 + 32) ! 26.5, 26.5
  outline%points(4) = FT_Vector(5 * 64 + 32, 26 * 64 + 32)  ! 5.5, 26.5
  
  outline%tags = FT_CURVE_TAG_ON
  outline%contours(1) = 3
  outline%n_points = 4
  outline%n_contours = 1
  
  ! Create 32x32 bitmaps
  success = ft_bitmap_new(32, 32, FT_PIXEL_MODE_MONO, mono_bitmap, error)
  if (.not. success) stop 1
  
  success = ft_bitmap_new(32, 32, FT_PIXEL_MODE_GRAY, aa_bitmap, error)
  if (.not. success) stop 1
  
  success = ft_raster_new(raster, error)
  if (.not. success) stop 1
  
  raster%outline => outline
  
  ! Render monochrome
  params%target => mono_bitmap
  params%flags = 0
  success = ft_raster_render_outline(raster, params, error)
  if (.not. success) stop 1
  
  ! Render antialiased
  params%target => aa_bitmap
  params%flags = FT_RASTER_FLAG_AA
  success = ft_raster_render_outline(raster, params, error)
  if (.not. success) stop 1
  
  print '("Original AA values:")'
  do i = 1, 32*32
    if (aa_bitmap%buffer(i) > 0) then
      print '("  Pixel ", I0, " = ", I0)', i, int(aa_bitmap%buffer(i))
    end if
  end do
  
  ! Enhance visibility by scaling up low values
  do i = 1, 32*32
    val = int(aa_bitmap%buffer(i))
    if (val > 0) then
      ! Scale 16 -> 128 for visibility
      val = val * 8
      if (val > 127) val = 127
      aa_bitmap%buffer(i) = int(val, kind=int8)
    end if
  end do
  
  ! Save original mono
  success = ft_bitmap_write_png(mono_bitmap, "compare_mono.png", error)
  if (success) print '("Saved: compare_mono.png")'
  
  ! Save enhanced AA
  success = ft_bitmap_write_png(aa_bitmap, "compare_aa_visible.png", error)
  if (success) print '("Saved: compare_aa_visible.png (8x enhanced)")'
  
  ! Create side-by-side comparison
  call create_side_by_side_comparison()
  
  ! Cleanup
  call ft_raster_done(raster)
  call ft_bitmap_done(mono_bitmap)
  call ft_bitmap_done(aa_bitmap)
  call ft_outline_done(outline)
  
  print '("✓ Antialiasing visualization complete!")'
  print '("  Check: compare_mono.png vs compare_aa_visible.png")'
  print '("  The AA shows smooth edges vs sharp mono edges")'
  
contains

  subroutine create_side_by_side_comparison()
    type(FT_Bitmap) :: combined_bitmap
    integer :: i, j, mono_val, aa_val
    
    ! Create combined bitmap (64x32 - two 32x32 images side by side)
    success = ft_bitmap_new(64, 32, FT_PIXEL_MODE_GRAY, combined_bitmap, error)
    if (.not. success) return
    
    ! Copy mono to left half, AA to right half
    do i = 1, 32  ! rows
      do j = 1, 32  ! columns
        ! Left half: monochrome (convert 1-bit to 8-bit)
        mono_val = 0
        if (is_mono_pixel_set(mono_bitmap, (i-1)*32 + j)) mono_val = 100
        combined_bitmap%buffer((i-1)*64 + j) = int(mono_val, kind=int8)
        
        ! Right half: antialiased (already enhanced)
        aa_val = int(aa_bitmap%buffer((i-1)*32 + j))
        combined_bitmap%buffer((i-1)*64 + j + 32) = int(aa_val, kind=int8)
      end do
    end do
    
    success = ft_bitmap_write_png(combined_bitmap, "compare_side_by_side.png", error)
    if (success) print '("Saved: compare_side_by_side.png (mono | AA)")'
    
    call ft_bitmap_done(combined_bitmap)
    
  end subroutine create_side_by_side_comparison

  function is_mono_pixel_set(bitmap, pixel_index) result(is_set)
    type(FT_Bitmap), intent(in) :: bitmap
    integer, intent(in) :: pixel_index
    logical :: is_set
    integer :: y, x, byte_index, bit_index
    
    y = (pixel_index - 1) / bitmap%width
    x = mod(pixel_index - 1, bitmap%width)
    byte_index = y * bitmap%pitch + x / 8 + 1
    bit_index = 7 - mod(x, 8)
    
    if (byte_index <= size(bitmap%buffer)) then
      is_set = btest(bitmap%buffer(byte_index), bit_index)
    else
      is_set = .false.
    end if
  end function is_mono_pixel_set

end program create_visible_aa