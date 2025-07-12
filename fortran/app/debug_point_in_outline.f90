program debug_point_in_outline
  use ft_face_unified
  use ft_types
  use ft_outline_mod
  use ft_outline_transform_mod
  use ft_antialiasing_enhanced
  use ft_bitmap_mod
  implicit none
  
  type(FT_Unified_Face) :: face
  type(FT_Outline) :: outline, transformed_outline
  integer(FT_Error) :: error
  logical :: success
  integer :: glyph_index
  character(len=256) :: font_file
  integer :: bitmap_size = 64
  integer :: i, j
  real :: test_x, test_y
  logical :: inside
  
  print *, "DEBUG: Point-in-Outline Analysis"
  print *, "================================"
  print *, ""
  
  font_file = "/usr/share/fonts/TTF/DejaVuSans.ttf"
  
  ! Load font and glyph
  success = ft_new_unified_face(font_file, 0, face, error)
  if (.not. success) stop "Failed to load face"
  
  glyph_index = ft_unified_get_glyph_index(face, iachar('A'))
  success = ft_unified_load_glyph(face, glyph_index, outline, error)
  if (.not. success) stop "Failed to load glyph"
  
  ! Transform outline
  success = ft_outline_transform_to_bitmap(outline, transformed_outline, &
                                          face%units_per_em, bitmap_size, bitmap_size, error)
  if (.not. success) stop "Failed to transform outline"
  
  print *, "Transformed outline details:"
  print *, "  Points:", transformed_outline%n_points
  print *, "  Contours:", transformed_outline%n_contours
  print *, ""
  
  ! Show first few points
  do i = 1, min(5, int(transformed_outline%n_points))
    print *, "  Point", i-1, ": (", transformed_outline%points(i)%x, ",", &
             transformed_outline%points(i)%y, ")"
  end do
  print *, ""
  
  ! Test some specific points that should be inside the 'A'
  print *, "Testing specific points for inside/outside:"
  
  ! Test center of bitmap
  test_x = real(bitmap_size) / 2.0
  test_y = real(bitmap_size) / 2.0
  inside = point_in_outline(transformed_outline, test_x, test_y)
  print *, "  Center (", test_x, ",", test_y, "):", inside
  
  ! Test a few other points
  test_x = 32.0; test_y = 40.0
  inside = point_in_outline(transformed_outline, test_x, test_y)
  print *, "  Point (", test_x, ",", test_y, "):", inside
  
  test_x = 30.0; test_y = 35.0
  inside = point_in_outline(transformed_outline, test_x, test_y)
  print *, "  Point (", test_x, ",", test_y, "):", inside
  
  test_x = 35.0; test_y = 35.0
  inside = point_in_outline(transformed_outline, test_x, test_y)
  print *, "  Point (", test_x, ",", test_y, "):", inside
  
  print *, ""
  
  ! Test grid sampling like the antialiasing engine does
  print *, "Grid sampling test (should find some inside points):"
  
  block
    integer :: hits = 0
    integer :: total_tests = 0
    
    do j = 1, bitmap_size
      do i = 1, bitmap_size
        test_x = real(i - 1) + 0.5
        test_y = real(j - 1) + 0.5
        total_tests = total_tests + 1
        
        if (point_in_outline(transformed_outline, test_x, test_y)) then
          hits = hits + 1
          if (hits <= 5) then  ! Show first 5 hits
            print *, "  HIT at (", test_x, ",", test_y, ")"
          end if
        end if
      end do
    end do
    
    print *, "  Total hits:", hits, "/", total_tests
    print *, "  Hit percentage:", real(hits) / real(total_tests) * 100.0, "%"
    
    if (hits == 0) then
      print *, "  PROBLEM: No points inside outline detected!"
    else
      print *, "  SUCCESS: Found points inside outline"
    end if
  end block
  
  ! Clean up
  if (associated(outline%points)) deallocate(outline%points)
  if (associated(outline%tags)) deallocate(outline%tags)
  if (associated(outline%contours)) deallocate(outline%contours)
  if (associated(transformed_outline%points)) deallocate(transformed_outline%points)
  if (associated(transformed_outline%tags)) deallocate(transformed_outline%tags)
  if (associated(transformed_outline%contours)) deallocate(transformed_outline%contours)
  call ft_done_unified_face(face)
  
  print *, ""
  print *, "Analysis complete!"
  
end program debug_point_in_outline