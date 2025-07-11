program test_stream
  use ft_types
  use ft_stream
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32
  implicit none
  
  integer :: test_count = 0
  integer :: failed_count = 0
  
  ! Run all tests
  call test_stream_open_close()
  call test_stream_read_operations()
  call test_stream_seek_tell()
  call test_endian_conversion()
  
  ! Print test summary
  print '(/, "Stream Tests - Tests run: ", I0)', test_count
  print '("Stream Tests - Tests failed: ", I0)', failed_count
  
  if (failed_count == 0) then
    print '(/, "All stream tests passed!")'
    stop 0
  else
    print '(/, "Some stream tests failed!")'
    stop 1
  end if

contains

  subroutine test_stream_open_close()
    type(FT_Stream_Type) :: stream
    integer(FT_Error) :: error
    logical :: success
    
    print '(/, "Testing stream open/close...")'
    
    ! Test opening non-existent file
    test_count = test_count + 1
    success = ft_stream_open(stream, "non_existent_file.ttf", error)
    
    if (.not. success .and. error == FT_Err_Cannot_Open_Stream) then
      print '("PASS: Non-existent file handling")'
    else
      print '("FAIL: Non-existent file should fail")'
      failed_count = failed_count + 1
    end if
    
    ! Create a test file
    call create_test_file()
    
    ! Test opening existing file
    test_count = test_count + 1
    success = ft_stream_open(stream, "test_stream.bin", error)
    
    if (success .and. error == FT_Err_Ok) then
      print '("PASS: Open existing file")'
      
      ! Check file size
      test_count = test_count + 1
      if (ft_stream_size(stream) == 256) then
        print '("PASS: File size detection")'
      else
        print '("FAIL: Incorrect file size: ", I0)', ft_stream_size(stream)
        failed_count = failed_count + 1
      end if
      
      ! Close stream
      call ft_stream_close(stream)
    else
      print '("FAIL: Could not open test file")'
      failed_count = failed_count + 1
    end if
    
    ! Clean up
    call delete_test_file()
    
  end subroutine test_stream_open_close
  
  subroutine test_stream_read_operations()
    type(FT_Stream_Type) :: stream
    integer(FT_Error) :: error
    integer(int8) :: byte_val
    integer(int16) :: short_val
    integer(int32) :: long_val
    logical :: success
    
    print '(/, "Testing stream read operations...")'
    
    ! Create test file with known data
    call create_test_file_with_data()
    
    ! Open stream
    success = ft_stream_open(stream, "test_stream.bin", error)
    if (.not. success) then
      print '("FAIL: Could not open test file for reading")'
      failed_count = failed_count + 1
      return
    end if
    
    ! Test read byte
    test_count = test_count + 1
    success = ft_stream_read_byte(stream, byte_val, error)
    
    if (success .and. byte_val == 0_int8) then
      print '("PASS: Read byte")'
    else
      print '("FAIL: Read byte value: ", I0)', byte_val
      failed_count = failed_count + 1
    end if
    
    ! Test read short (big-endian)
    test_count = test_count + 1
    success = ft_stream_read_short(stream, short_val, error)
    
    if (success .and. short_val == 258_int16) then  ! 0x01 0x02 = 258
      print '("PASS: Read short (big-endian)")'
    else
      print '("FAIL: Read short value: ", I0)', short_val
      failed_count = failed_count + 1
    end if
    
    ! Test read long (big-endian)
    test_count = test_count + 1
    success = ft_stream_read_long(stream, long_val, error)
    
    if (success .and. long_val == 50595078_int32) then  ! 0x03 0x04 0x05 0x06
      print '("PASS: Read long (big-endian)")'
    else
      print '("FAIL: Read long value: ", I0)', long_val
      failed_count = failed_count + 1
    end if
    
    ! Close stream
    call ft_stream_close(stream)
    
    ! Clean up
    call delete_test_file()
    
  end subroutine test_stream_read_operations
  
  subroutine test_stream_seek_tell()
    type(FT_Stream_Type) :: stream
    integer(FT_Error) :: error
    integer(int8) :: byte_val
    logical :: success
    integer(c_size_t) :: pos
    
    print '(/, "Testing stream seek/tell...")'
    
    ! Create test file
    call create_test_file_with_data()
    
    ! Open stream
    success = ft_stream_open(stream, "test_stream.bin", error)
    if (.not. success) return
    
    ! Test initial position
    test_count = test_count + 1
    pos = ft_stream_tell(stream)
    
    if (pos == 0) then
      print '("PASS: Initial position is 0")'
    else
      print '("FAIL: Initial position: ", I0)', pos
      failed_count = failed_count + 1
    end if
    
    ! Seek to position 10
    test_count = test_count + 1
    success = ft_stream_seek(stream, 10_c_size_t, error)
    
    if (success .and. ft_stream_tell(stream) == 10) then
      print '("PASS: Seek to position 10")'
    else
      print '("FAIL: Seek failed")'
      failed_count = failed_count + 1
    end if
    
    ! Read byte at position 10
    test_count = test_count + 1
    success = ft_stream_read_byte(stream, byte_val, error)
    
    if (success .and. byte_val == 10_int8) then
      print '("PASS: Read after seek")'
    else
      print '("FAIL: Read after seek value: ", I0)', byte_val
      failed_count = failed_count + 1
    end if
    
    ! Test seeking beyond file size
    test_count = test_count + 1
    success = ft_stream_seek(stream, 1000_c_size_t, error)
    
    if (.not. success .and. error == FT_Err_Invalid_Stream_Seek) then
      print '("PASS: Seek beyond EOF handling")'
    else
      print '("FAIL: Should fail seeking beyond EOF")'
      failed_count = failed_count + 1
    end if
    
    ! Close stream
    call ft_stream_close(stream)
    
    ! Clean up
    call delete_test_file()
    
  end subroutine test_stream_seek_tell
  
  subroutine test_endian_conversion()
    integer(int16) :: test_short
    integer(int32) :: test_long
    integer(int8) :: bytes(4)
    
    print '(/, "Testing endian conversion...")'
    
    ! Test 16-bit big-endian conversion
    test_count = test_count + 1
    bytes(1) = int(z'12', int8)
    bytes(2) = int(z'34', int8)
    
    test_short = ior(ishft(int(bytes(1), int16), 8), int(bytes(2), int16))
    
    if (test_short == int(z'1234', int16)) then
      print '("PASS: 16-bit big-endian conversion")'
    else
      print '("FAIL: 16-bit conversion: ", Z4.4)', test_short
      failed_count = failed_count + 1
    end if
    
    ! Test 32-bit big-endian conversion
    test_count = test_count + 1
    bytes(1) = int(z'12', int8)
    bytes(2) = int(z'34', int8)
    bytes(3) = int(z'56', int8)
    bytes(4) = int(z'78', int8)
    
    test_long = ior(ior(ior(ishft(int(bytes(1), int32), 24), &
                            ishft(int(bytes(2), int32), 16)), &
                            ishft(int(bytes(3), int32), 8)), &
                            int(bytes(4), int32))
    
    if (test_long == int(z'12345678', int32)) then
      print '("PASS: 32-bit big-endian conversion")'
    else
      print '("FAIL: 32-bit conversion: ", Z8.8)', test_long
      failed_count = failed_count + 1
    end if
    
  end subroutine test_endian_conversion
  
  ! Helper: Create empty test file
  subroutine create_test_file()
    integer :: unit, i
    
    open(newunit=unit, file="test_stream.bin", access="stream", &
         form="unformatted", status="replace")
    
    ! Write 256 bytes of zeros
    do i = 1, 256
      write(unit) 0_int8
    end do
    
    close(unit)
  end subroutine create_test_file
  
  ! Helper: Create test file with specific data
  subroutine create_test_file_with_data()
    integer :: unit, i
    integer(int8) :: val
    
    open(newunit=unit, file="test_stream.bin", access="stream", &
         form="unformatted", status="replace")
    
    ! Write sequential bytes
    do i = 0, 255
      val = int(mod(i, 256), int8)
      write(unit) val
    end do
    
    close(unit)
  end subroutine create_test_file_with_data
  
  ! Helper: Delete test file
  subroutine delete_test_file()
    integer :: unit, stat
    
    open(newunit=unit, file="test_stream.bin", status="old", iostat=stat)
    if (stat == 0) then
      close(unit, status="delete")
    end if
  end subroutine delete_test_file

end program test_stream