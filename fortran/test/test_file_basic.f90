program test_file_basic
  use ft_stream
  use ft_types
  implicit none
  
  type(FT_Stream_Type) :: stream
  integer(FT_Error) :: error
  logical :: success
  character(len=256) :: filename
  
  filename = "test/test_font.ttf"
  
  print '("Testing basic file operations")'
  print '("File: ", A)', trim(filename)
  
  ! Check if file exists
  inquire(file=filename, exist=success)
  print '("File exists: ", L1)', success
  
  ! Try opening with ft_stream_open
  success = ft_stream_open(stream, filename, error)
  print '("ft_stream_open success: ", L1)', success
  print '("ft_stream_open error: ", I0)', error
  
  if (success) then
    print '("Stream size: ", I0)', ft_stream_size(stream)
    call ft_stream_close(stream)
  end if
  
end program test_file_basic