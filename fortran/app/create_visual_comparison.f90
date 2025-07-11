program create_visual_comparison
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
  
  print '("Creating Large Visual Comparison")'
  print '("===============================")'
  
  ! Create a larger, more visible test case
  success = ft_outline_new(4, 1, outline, error)
  if (.not. success) stop 1
  
  ! Create a 80x80 rectangle in a 128x128 bitmap for clear visibility
  outline%points(1) = FT_Vector(24 * 64, 24 * 64)   ! (24, 24)
  outline%points(2) = FT_Vector(104 * 64, 24 * 64)  ! (104, 24)
  outline%points(3) = FT_Vector(104 * 64, 104 * 64) ! (104, 104)
  outline%points(4) = FT_Vector(24 * 64, 104 * 64)  ! (24, 104)
  
  outline%tags(1) = FT_CURVE_TAG_ON
  outline%tags(2) = FT_CURVE_TAG_ON
  outline%tags(3) = FT_CURVE_TAG_ON
  outline%tags(4) = FT_CURVE_TAG_ON
  outline%contours(1) = 3  ! 0-based index of last point
  outline%n_points = 4
  outline%n_contours = 1
  
  ! Create 128x128 bitmaps for excellent visibility
  success = ft_bitmap_new(128, 128, FT_PIXEL_MODE_MONO, mono_bitmap, error)
  if (.not. success) stop 1
  
  success = ft_bitmap_new(128, 128, FT_PIXEL_MODE_GRAY, aa_bitmap, error)
  if (.not. success) stop 1
  
  ! Initialize raster
  success = ft_raster_new(raster, error)
  if (.not. success) stop 1
  
  ! Render monochrome
  print '("Rendering monochrome (filled rectangle)...")'
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
  
  ! Render antialiased
  print '("Rendering antialiased (smooth edges + filled interior)...")'
  params%target => aa_bitmap
  params%flags = FT_RASTER_FLAG_AA
  success = ft_raster_render_outline(raster, params, error)
  if (.not. success) then
    print '("ERROR: AA render failed with error: ", I0)', error
    stop 1
  end if
  
  ! Count AA pixels
  aa_pixels = 0
  do i = 1, aa_bitmap%width * aa_bitmap%rows
    if (aa_bitmap%buffer(i) > 0) aa_pixels = aa_pixels + 1
  end do
  
  ! Display results
  print '()'
  print '("RESULTS:")'
  print '("========")'
  print '("Shape: 80x80 rectangle from (24,24) to (104,104)")'
  print '("Bitmap size: 128x128 pixels")'
  print '("Monochrome pixels: ", I0)', mono_pixels
  print '("Antialiased pixels: ", I0)', aa_pixels
  print '()'
  
  if (aa_pixels > mono_pixels) then
    print '("✅ SUCCESS: AA has more pixels than mono (includes smooth edges)")'
  else
    print '("❌ ISSUE: AA should have more pixels than mono")'
  end if
  
  ! Save comparison images
  success = ft_bitmap_write_png(mono_bitmap, 'final_mono.png', error)
  if (success) then
    print '("✅ Saved monochrome: final_mono.png")'
  else
    print '("❌ Failed to save monochrome PNG")'
  end if
  
  success = ft_bitmap_write_png(aa_bitmap, 'final_aa.png', error)
  if (success) then
    print '("✅ Saved antialiased: final_aa.png")'
  else
    print '("❌ Failed to save antialiased PNG")'
  end if
  
  print '()'
  print '("WHAT TO EXPECT:")'
  print '("final_mono.png: Sharp black rectangle with crisp edges")'
  print '("final_aa.png: Smooth gray rectangle with anti-aliased edges")'
  print '("             Interior should be light gray, edges darker gray")'
  
  ! Cleanup
  call ft_raster_done(raster)
  call ft_bitmap_done(mono_bitmap)
  call ft_bitmap_done(aa_bitmap)
  call ft_outline_done(outline)
  
end program create_visual_comparison