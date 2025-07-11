program create_aa_comparison
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
  integer :: i, j, mono_pixels, aa_pixels
  
  print '("Creating AA vs Mono Visual Comparison")'
  print '("====================================")'
  
  ! Create simple, predictable test case - larger rectangle for visibility
  success = ft_outline_new(4, 1, outline, error)
  if (.not. success) stop 1
  
  ! 40x40 rectangle centered in 64x64 bitmap for better visibility
  outline%points(1) = FT_Vector(12 * 64, 12 * 64)   ! 12, 12
  outline%points(2) = FT_Vector(52 * 64, 12 * 64)   ! 52, 12
  outline%points(3) = FT_Vector(52 * 64, 52 * 64)   ! 52, 52
  outline%points(4) = FT_Vector(12 * 64, 52 * 64)   ! 12, 52
  
  outline%tags(1) = FT_CURVE_TAG_ON
  outline%tags(2) = FT_CURVE_TAG_ON
  outline%tags(3) = FT_CURVE_TAG_ON
  outline%tags(4) = FT_CURVE_TAG_ON
  outline%contours(1) = 3  ! 0-based index of last point
  outline%n_points = 4
  outline%n_contours = 1
  
  ! Create 64x64 bitmaps for better visibility
  success = ft_bitmap_new(64, 64, FT_PIXEL_MODE_MONO, mono_bitmap, error)
  if (.not. success) stop 1
  
  success = ft_bitmap_new(64, 64, FT_PIXEL_MODE_GRAY, aa_bitmap, error)
  if (.not. success) stop 1
  
  ! Initialize raster
  success = ft_raster_new(raster, error)
  if (.not. success) stop 1
  
  ! Render monochrome
  print '("Rendering monochrome...")'
  params%target => mono_bitmap
  params%flags = 0
  success = ft_raster_render_outline(raster, params, error)
  if (.not. success) then
    print '("ERROR: Mono render failed with error: ", I0)', error
    stop 1
  end if
  
  ! Count mono pixels
  mono_pixels = 0
  do i = 1, mono_bitmap%pitch * mono_bitmap%rows
    mono_pixels = mono_pixels + popcnt(mono_bitmap%buffer(i))
  end do
  print '("Mono pixels set: ", I0)', mono_pixels
  
  ! Render antialiased
  print '("Rendering antialiased...")'
  params%target => aa_bitmap
  params%flags = FT_RASTER_FLAG_AA
  success = ft_raster_render_outline(raster, params, error)
  if (.not. success) then
    print '("ERROR: AA render failed")'
    stop 1
  end if
  
  ! Count AA pixels
  aa_pixels = 0
  do i = 1, aa_bitmap%width * aa_bitmap%rows
    if (aa_bitmap%buffer(i) > 0) aa_pixels = aa_pixels + 1
  end do
  print '("AA pixels set: ", I0)', aa_pixels
  
  ! Show comparison
  print '()'
  print '("Visual Comparison Results:")'
  print '("=========================")'
  print '("Shape: 40x40 rectangle from (12,12) to (52,52)")'
  print '("Mono pixels: ", I0, " (filled rectangle)")', mono_pixels
  print '("AA pixels: ", I0, " (should be similar with smooth edges)")', aa_pixels
  print '()'
  
  ! Create comparison images
  success = ft_bitmap_write_png(mono_bitmap, 'comparison_mono.png', error)
  if (success) then
    print '("Saved: comparison_mono.png")'
  else
    print '("ERROR: Could not save mono PNG")'
  end if
  
  success = ft_bitmap_write_png(aa_bitmap, 'comparison_aa.png', error)
  if (success) then
    print '("Saved: comparison_aa.png")'
  else
    print '("ERROR: Could not save AA PNG")'
  end if
  
  ! Create enhanced visibility version
  do i = 1, aa_bitmap%width * aa_bitmap%rows
    if (aa_bitmap%buffer(i) > 0) then
      aa_bitmap%buffer(i) = min(255, int(aa_bitmap%buffer(i) * 5))  ! 5x enhance
    end if
  end do
  
  success = ft_bitmap_write_png(aa_bitmap, 'comparison_aa_enhanced.png', error)
  if (success) then
    print '("Saved: comparison_aa_enhanced.png (5x enhanced visibility)")'
  else
    print '("ERROR: Could not save enhanced AA PNG")'
  end if
  
  print '()'
  print '("Current Status:")'
  print '("- Monochrome: Working correctly (filled rectangle)")'
  print '("- Antialiasing: Edges working, interior filling needs work")'
  print '("- Coverage values: Now correct (0-255 range)")'
  print '("- Edge detection: Both left and right edges processed")'
  print '("- Next step: Fix interior span filling between edges")'
  
  ! Cleanup
  call ft_raster_done(raster)
  call ft_bitmap_done(mono_bitmap)
  call ft_bitmap_done(aa_bitmap)
  call ft_outline_done(outline)
  
end program create_aa_comparison