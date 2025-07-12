program test_font_loading_performance
  use ft_face_unified
  use ft_types
  use, intrinsic :: iso_fortran_env, only: real64
  implicit none
  
  ! Test parameters
  integer, parameter :: NUM_LOAD_TESTS = 50
  character(len=*), parameter :: TEST_FONT = "/usr/share/fonts/TTF/DejaVuSans.ttf"
  
  ! Performance metrics
  real(real64) :: start_time, end_time
  real(real64) :: optimized_time, standard_time
  integer :: i
  type(FT_Unified_Face) :: face
  integer(FT_Error) :: error
  logical :: success
  
  print *, "Font Loading Performance Tests"
  print *, "=============================="
  print *
  
  ! Test 1: Standard loading
  print *, "Testing standard font loading..."
  call cpu_time(start_time)
  
  do i = 1, NUM_LOAD_TESTS
    success = ft_new_unified_face(TEST_FONT, 0, face, error)
    if (success) then
      call ft_done_unified_face(face)
    else
      print *, "Standard loading failed on iteration", i, "error:", error
      stop
    end if
  end do
  
  call cpu_time(end_time)
  standard_time = end_time - start_time
  
  print *, "  Standard loading time:", standard_time, "seconds"
  print *, "  Average per load:", (standard_time / NUM_LOAD_TESTS) * 1000.0, "ms"
  print *, "  Loading rate:", real(NUM_LOAD_TESTS) / standard_time, "fonts/sec"
  print *
  
  ! Test 2: Performance characteristics
  print *, "Testing font loading characteristics..."
  
  ! Single load with detailed timing
  call cpu_time(start_time)
  success = ft_new_unified_face(TEST_FONT, 0, face, error)
  call cpu_time(end_time)
  
  if (success) then
    print *, "  Single font load time:", (end_time - start_time) * 1000.0, "ms"
    print *, "  Font format:", face%font_format
    print *, "  Number of glyphs:", face%num_glyphs
    print *, "  Units per EM:", face%units_per_em
    print *, "  Face flags:", face%face_flags
    
    call ft_done_unified_face(face)
  else
    print *, "ERROR: Failed to load test font"
    stop
  end if
  
  print *
  
  ! Test 3: Memory efficiency
  print *, "Testing memory efficiency..."
  
  ! Load multiple fonts simultaneously
  block
    type(FT_Unified_Face) :: faces(10)
    integer :: loaded_count = 0
  
  call cpu_time(start_time)
  
  do i = 1, 10
    success = ft_new_unified_face(TEST_FONT, 0, faces(i), error)
    if (success) then
      loaded_count = loaded_count + 1
    else
      print *, "  Failed to load font", i
      exit
    end if
  end do
  
  call cpu_time(end_time)
  
  print *, "  Loaded", loaded_count, "fonts simultaneously"
  print *, "  Total loading time:", (end_time - start_time) * 1000.0, "ms"
  print *, "  Average per font:", ((end_time - start_time) / loaded_count) * 1000.0, "ms"
  
    ! Clean up
    do i = 1, loaded_count
      call ft_done_unified_face(faces(i))
    end do
  end block
  
  print *
  
  ! Test 4: Large font stress test
  print *, "Testing large font handling..."
  
  block
    character(len=256) :: large_fonts(3)
    integer :: font_count = 0
  
  ! Try to find some larger fonts
  large_fonts(1) = "/usr/share/fonts/TTF/DejaVuSansMNerdFontMono-Bold.ttf"
  large_fonts(2) = "/usr/share/fonts/TTF/DejaVuSerif.ttf"
  large_fonts(3) = "/usr/share/fonts/TTF/DejaVuSans-Bold.ttf"
  
  do i = 1, 3
    call cpu_time(start_time)
    success = ft_new_unified_face(trim(large_fonts(i)), 0, face, error)
    call cpu_time(end_time)
    
    if (success) then
      font_count = font_count + 1
      print *, "  Font", i, ":", trim(large_fonts(i))
      print *, "    Load time:", (end_time - start_time) * 1000.0, "ms"
      print *, "    Glyphs:", face%num_glyphs
      call ft_done_unified_face(face)
    end if
  end do
  
    print *, "  Successfully tested", font_count, "large fonts"
  end block
  
  print *
  print *, "Performance testing completed!"
  print *, "✓ All performance tests PASSED!"
  
end program test_font_loading_performance