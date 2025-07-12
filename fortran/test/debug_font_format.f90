program debug_font_format
  use ft_types
  use ft_stream
  use ft_font_format
  use, intrinsic :: iso_c_binding, only: c_size_t
  use, intrinsic :: iso_fortran_env, only: int8
  implicit none
  
  type(FT_Stream_Type) :: stream
  integer(FT_Error) :: error
  logical :: success
  integer :: format
  character(len=256) :: font_file
  character(len=12) :: header_string
  integer(int8) :: header_bytes(12)
  integer :: i
  
  print '("Font Format Detection Debug")'
  print '("==========================")'
  print '()'
  
  ! Test with the real system font
  font_file = "/usr/share/fonts/TTF/DejaVuSansMNerdFontMono-Bold.ttf"
  
  print '("Testing font: ", A)', trim(font_file)
  
  ! Open the font file stream
  success = ft_stream_open(stream, font_file, error)
  if (.not. success) then
    print '("FAILED to open stream, error=", I0)', error
    stop 1
  end if
  
  print '("SUCCESS: Stream opened")'
  print '("Stream size: ", I0)', ft_stream_size(stream)
  
  ! Read first 12 bytes to see what we have
  if (.not. ft_stream_seek(stream, 0_c_size_t, error)) then
    print '("FAILED to seek to start, error=", I0)', error
    stop 1
  end if
  
  do i = 1, 12
    if (.not. ft_stream_read_byte(stream, header_bytes(i), error)) then
      print '("FAILED to read byte ", I0, ", error=", I0)', i, error
      stop 1
    end if
    header_string(i:i) = char(header_bytes(i))
  end do
  
  print '("First 12 bytes (hex): ", 12(I0,1X))', (header_bytes(i), i=1,12)
  print '("First 4 bytes as string: ''", A4, "''")', header_string(1:4)
  
  ! Test font format detection
  success = ft_detect_font_format(stream, format, error)
  if (.not. success) then
    print '("FAILED to detect format, error=", I0)', error
    print '("Error constants for reference:")'
    print '("  FT_Err_Unknown_File_Format = 3")'
    print '("  FT_Err_Invalid_Stream_Handle = ??")'
  else
    print '("SUCCESS: Format detected = ", I0)', format
    print '("Format constants:")'
    print '("  FT_FONT_FORMAT_TRUETYPE = ", I0)', FT_FONT_FORMAT_TRUETYPE
    print '("  FT_FONT_FORMAT_OPENTYPE = ", I0)', FT_FONT_FORMAT_OPENTYPE
    print '("  FT_FONT_FORMAT_CFF = ", I0)', FT_FONT_FORMAT_CFF
  end if
  
  ! Clean up
  call ft_stream_close(stream)
  
  print '()'
  print '("Debug complete")'
  
end program debug_font_format