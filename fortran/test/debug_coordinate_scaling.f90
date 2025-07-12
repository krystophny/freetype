program debug_coordinate_scaling
  use ft_types
  use ft_face_unified
  use ft_outline_mod
  use ft_geometry, only: FT_Vector
  implicit none
  
  type(FT_Unified_Face) :: face
  type(FT_Outline) :: outline
  integer(FT_Error) :: error
  logical :: success
  integer :: glyph_index
  character(len=256) :: font_file
  integer :: i
  real :: scale_factor
  integer :: target_size = 48
  
  print *, "Debug: Coordinate Scaling Analysis"
  print *, "=================================="
  
  font_file = "/usr/share/fonts/TTF/DejaVuSans.ttf"
  
  ! Load font
  success = ft_new_unified_face(font_file, 0, face, error)
  if (.not. success) stop "Failed to load font"
  
  print *, "Font units per EM:", face%units_per_em
  print *, "Target pixel size:", target_size
  
  ! Calculate scale factor: pixels per font unit
  scale_factor = real(target_size) / real(face%units_per_em)
  print *, "Scale factor:", scale_factor
  print *, ""
  
  ! Load 'A' character
  glyph_index = ft_unified_get_glyph_index(face, iachar('A'))
  print *, "Loading glyph 'A' (index", glyph_index, "):"
  
  success = ft_unified_load_glyph(face, glyph_index, outline, error)
  if (.not. success) stop "Failed to load glyph"
  
  print *, "  Points:", outline%n_points
  print *, "  Contours:", outline%n_contours
  print *, ""
  
  ! Show original coordinates and scaled coordinates
  print *, "Original coordinates (font units) -> Scaled coordinates (pixels):"
  do i = 1, min(10, outline%n_points)
    print '("  Point ", I2, ": (", I5, ",", I5, ") -> (", F6.1, ",", F6.1, ")")', &
          i, outline%points(i)%x, outline%points(i)%y, &
          real(outline%points(i)%x) * scale_factor, &
          real(outline%points(i)%y) * scale_factor
  end do
  
  print *, ""
  print *, "Bounding box analysis:"
  
  block
    integer :: min_x, max_x, min_y, max_y
    real :: scaled_min_x, scaled_max_x, scaled_min_y, scaled_max_y
    real :: width, height
    
    min_x = outline%points(1)%x
    max_x = outline%points(1)%x
    min_y = outline%points(1)%y
    max_y = outline%points(1)%y
    
    do i = 2, outline%n_points
      min_x = min(min_x, outline%points(i)%x)
      max_x = max(max_x, outline%points(i)%x)
      min_y = min(min_y, outline%points(i)%y)
      max_y = max(max_y, outline%points(i)%y)
    end do
    
    scaled_min_x = real(min_x) * scale_factor
    scaled_max_x = real(max_x) * scale_factor
    scaled_min_y = real(min_y) * scale_factor
    scaled_max_y = real(max_y) * scale_factor
    
    width = scaled_max_x - scaled_min_x
    height = scaled_max_y - scaled_min_y
    
    print *, "  Font units: (", min_x, ",", min_y, ") to (", max_x, ",", max_y, ")"
    print *, "  Pixels: (", scaled_min_x, ",", scaled_min_y, ") to (", scaled_max_x, ",", scaled_max_y, ")"
    print *, "  Size: ", width, " x ", height, " pixels"
    
    ! Check if coordinates fit in target bitmap
    if (scaled_min_x < 0 .or. scaled_max_x > target_size .or. &
        scaled_min_y < 0 .or. scaled_max_y > target_size) then
      print *, "  WARNING: Glyph coordinates are outside target bitmap!"
      print *, "  Need offset and/or different scaling"
    else
      print *, "  OK: Glyph fits in target bitmap"
    end if
  end block
  
  ! Clean up
  if (associated(outline%points)) deallocate(outline%points)
  if (associated(outline%tags)) deallocate(outline%tags)
  if (associated(outline%contours)) deallocate(outline%contours)
  
  call ft_done_unified_face(face)
  
end program debug_coordinate_scaling