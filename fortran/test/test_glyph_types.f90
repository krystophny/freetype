program test_glyph_types
  use ft_types
  use ft_glyph_types
  use, intrinsic :: iso_c_binding
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Test glyph types functionality
  call test_glyph_metrics()
  call test_size_metrics()
  call test_glyph_formats()
  call test_metrics_scaling()
  
  ! Print test summary
  print '(/, "Tests run: ", I0)', test_count
  print '("Tests failed: ", I0)', failed_count
  
  if (failed_count == 0) then
    print '(/, "All tests passed!")'
    stop 0
  else
    print '(/, "Some tests failed!")'
    stop 1
  end if

contains

  subroutine test_glyph_metrics()
    type(FT_Glyph_Metrics) :: metrics
    
    print '(/, "Testing glyph metrics...")'
    
    ! Test initialization
    test_count = test_count + 1
    call ft_glyph_metrics_init(metrics)
    
    if (metrics%width /= 0 .or. metrics%height /= 0 .or. &
        metrics%horiAdvance /= 0 .or. metrics%vertAdvance /= 0) then
      print '("FAIL: Glyph metrics initialization")'
      failed_count = failed_count + 1
    else
      print '("PASS: Glyph metrics initialization")'
    end if
    
    ! Test setting values
    test_count = test_count + 1
    metrics%width = ft_f26dot6_from_pixels(10)
    metrics%height = ft_f26dot6_from_pixels(12)
    metrics%horiAdvance = ft_f26dot6_from_pixels(11)
    metrics%horiBearingX = ft_f26dot6_from_pixels(1)
    metrics%horiBearingY = ft_f26dot6_from_pixels(10)
    
    if (ft_f26dot6_to_int(metrics%width) /= 10 .or. &
        ft_f26dot6_to_int(metrics%height) /= 12) then
      print '("FAIL: Glyph metrics value storage")'
      failed_count = failed_count + 1
    else
      print '("PASS: Glyph metrics value storage")'
    end if
    
  end subroutine test_glyph_metrics
  
  subroutine test_size_metrics()
    type(FT_Size_Metrics) :: metrics
    
    print '(/, "Testing size metrics...")'
    
    ! Test initialization
    test_count = test_count + 1
    call ft_size_metrics_init(metrics)
    
    if (metrics%x_ppem /= 0 .or. metrics%y_ppem /= 0 .or. &
        metrics%ascender /= 0 .or. metrics%height /= 0) then
      print '("FAIL: Size metrics initialization")'
      failed_count = failed_count + 1
    else
      print '("PASS: Size metrics initialization")'
    end if
    
    ! Test setting values
    test_count = test_count + 1
    metrics%x_ppem = 24
    metrics%y_ppem = 24
    metrics%x_scale = ft_fixed_from_float(1.0)
    metrics%y_scale = ft_fixed_from_float(1.0)
    metrics%ascender = ft_f26dot6_from_pixels(18)
    metrics%descender = ft_f26dot6_from_pixels(-6)
    metrics%height = ft_f26dot6_from_pixels(24)
    
    if (metrics%x_ppem /= 24 .or. &
        ft_f26dot6_to_int(metrics%height) /= 24) then
      print '("FAIL: Size metrics value storage")'
      failed_count = failed_count + 1
    else
      print '("PASS: Size metrics value storage")'
    end if
    
  end subroutine test_size_metrics
  
  subroutine test_glyph_formats()
    print '(/, "Testing glyph format constants...")'
    
    ! Test format values are distinct
    test_count = test_count + 1
    
    if (FT_GLYPH_FORMAT_NONE == FT_GLYPH_FORMAT_BITMAP .or. &
        FT_GLYPH_FORMAT_BITMAP == FT_GLYPH_FORMAT_OUTLINE .or. &
        FT_GLYPH_FORMAT_OUTLINE == FT_GLYPH_FORMAT_COMPOSITE) then
      print '("FAIL: Glyph format constants not distinct")'
      failed_count = failed_count + 1
    else
      print '("PASS: Glyph format constants")'
    end if
    
  end subroutine test_glyph_formats
  
  subroutine test_metrics_scaling()
    type(FT_Glyph_Metrics) :: metrics
    integer(FT_Fixed) :: scale_2x
    
    print '(/, "Testing metrics scaling...")'
    
    ! Initialize metrics with known values
    metrics%width = ft_f26dot6_from_pixels(10)
    metrics%height = ft_f26dot6_from_pixels(20)
    metrics%horiAdvance = ft_f26dot6_from_pixels(12)
    metrics%horiBearingX = ft_f26dot6_from_pixels(1)
    metrics%horiBearingY = ft_f26dot6_from_pixels(18)
    metrics%vertBearingX = ft_f26dot6_from_pixels(0)
    metrics%vertBearingY = ft_f26dot6_from_pixels(2)
    metrics%vertAdvance = ft_f26dot6_from_pixels(22)
    
    ! Scale by 2x
    scale_2x = ft_fixed_from_int(2)
    
    test_count = test_count + 1
    call ft_glyph_metrics_scale(metrics, scale_2x, scale_2x)
    
    if (ft_f26dot6_to_int(metrics%width) /= 20 .or. &
        ft_f26dot6_to_int(metrics%height) /= 40 .or. &
        ft_f26dot6_to_int(metrics%horiAdvance) /= 24) then
      print '("FAIL: Metrics scaling - width=", I0, " height=", I0, " advance=", I0)', &
        ft_f26dot6_to_int(metrics%width), &
        ft_f26dot6_to_int(metrics%height), &
        ft_f26dot6_to_int(metrics%horiAdvance)
      failed_count = failed_count + 1
    else
      print '("PASS: Metrics scaling")'
    end if
    
  end subroutine test_metrics_scaling

end program test_glyph_types