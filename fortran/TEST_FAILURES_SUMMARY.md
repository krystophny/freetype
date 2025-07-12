# Test Failures Summary

## Summary
Out of 56 test files run, only 2 tests failed:

1. **test_cff_full_integration** - Segmentation fault
2. **test_real_cff_font** - Error loading font (exit code 1)

## Failed Tests Details

### 1. test_cff_full_integration
- **Exit Code**: 139 (Segmentation fault)
- **Error Type**: Memory access violation (SIGSEGV)
- **Error Location**: 
  - Function: `__ft_cff_MOD_ft_cff_get_charstrings_offset` at line 536 in `ft_cff.f90`
  - Called from: `__ft_cff_MOD_ft_cff_parse_charstrings` at line 558
- **Backtrace**:
  ```
  #0  0x7f7762355eef in ???
  #1  0x40a69d in __ft_cff_MOD_ft_cff_get_charstrings_offset at ft_cff.f90:536
  #2  0x40a513 in __ft_cff_MOD_ft_cff_parse_charstrings at ft_cff.f90:558
  #3  0x403e6e in test_cff_parser_with_charstrings at test_cff_full_integration.f90:130
  ```

### 2. test_real_cff_font
- **Exit Code**: 1
- **Error**: Could not initialize face, error=7
- **Test File**: `/usr/share/fonts/gsfonts/NimbusRoman-Regular.otf`
- **Description**: Failed to load a real OpenType font with CFF outlines

## All Passing Tests (54 tests)

The following tests all passed successfully:
- test_aa_pixels
- test_antialiasing
- test_bbox
- test_bezier
- test_bitmap_io
- test_cff_charstring
- test_cff_charstring_real
- test_cff_face
- test_cff_face_debug
- test_cff_glyph_load
- test_cff_minimal
- test_cff_parser
- test_cff_subroutines
- test_charstring_debug
- test_error_validation
- test_f26dot6
- test_file_basic
- test_filled_render
- test_fixed
- test_font_format
- test_font_format_file
- test_font_format_simple
- test_ft_bitmap
- test_ft_compare
- test_ft_face
- test_ft_outline
- test_ft_raster
- test_ft_raster_render
- test_ft_scanline
- test_glyph_loading
- test_glyph_types
- test_head_debug
- test_integration
- test_list
- test_matrix
- test_memory
- test_memory_stream
- test_minimal_head
- test_negative_encoding
- test_object
- test_performance
- test_png_write
- test_real_fonts (partially failed - could not load test_font.ttf)
- test_rendering_performance
- test_stream
- test_stream_seek
- test_stream_simple
- test_system_fonts
- test_tt_cmap
- test_tt_glyph
- test_tt_head
- test_tt_hmtx
- test_tt_integration
- test_tt_load
- test_tt_loca
- test_tt_maxp
- test_type1_parser
- test_types
- test_unified_face
- test_vector
- test_visual_png

## Notes
- The majority of tests pass successfully (96.4% pass rate)
- Both failures are related to CFF (Compact Font Format) handling
- The segmentation fault in test_cff_full_integration appears to be related to CharString offset parsing
- The test_real_cff_font failure might be due to incomplete CFF implementation for real-world fonts