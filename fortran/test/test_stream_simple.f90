program test_stream_simple
  use ft_types
  use ft_stream
  implicit none
  
  type(FT_Stream_Type) :: stream
  integer(FT_Error) :: error
  logical :: success
  character(len=256) :: temp_file
  integer :: unit, iostat
  
  print '("Simple Stream Test")'
  print '("==================")'
  
  ! Create a simple test file
  temp_file = "/tmp/test_stream_simple.dat"
  open(newunit=unit, file=temp_file, form='unformatted', access='stream', &
       status='replace', iostat=iostat)
  if (iostat /= 0) then
    print '("ERROR: Could not create test file")'
    stop 1
  end if
  
  ! Write some data
  write(unit) "Hello, World!"
  close(unit)
  
  print '("Created file: ", A)', trim(temp_file)
  
  ! Try to open with stream
  success = ft_stream_open(stream, temp_file, error)
  if (.not. success) then
    print '("ERROR: Could not open stream, error=", I0)', error
    stop 1
  end if
  
  print '("SUCCESS: Stream opened!")'
  print '("Stream size: ", I0)', ft_stream_size(stream)
  
  ! Close stream
  call ft_stream_close(stream)
  
  ! Clean up
  call execute_command_line("rm -f " // trim(temp_file))
  
  print '("Test completed successfully!")'
  
end program test_stream_simple