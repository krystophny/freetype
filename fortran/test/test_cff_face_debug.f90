program test_cff_face_debug
  use ft_types
  use ft_cff
  use ft_cff_face
  implicit none
  
  type(FT_CFF_Face_Type) :: face
  integer(FT_Error) :: error
  logical :: success
  
  ! Minimal working CFF data (same as test_cff_minimal)
  character(len=1), parameter :: cff_data(*) = [ &
    char(1), char(0), char(4), char(1), &    ! Header: major=1, minor=0, hdrSize=4, offSize=1 (4 bytes)
    ! Name INDEX
    char(0), char(1), &                      ! count=1 (2 bytes)
    char(1), &                               ! offset_size=1 (1 byte)
    char(1), char(5), &                      ! offsets: 1, 5 (2 bytes) 
    char(84), char(101), char(115), char(116), & ! "Test" (4 bytes)
    ! Top DICT INDEX
    char(0), char(1), &                      ! count=1 (2 bytes)
    char(1), &                               ! offset_size=1 (1 byte)
    char(1), char(2), &                      ! offsets: 1, 2 (2 bytes)
    char(0), &                               ! Empty DICT (1 byte)
    ! String INDEX (empty)
    char(0), char(0), &                      ! count=0 (2 bytes)
    ! Global Subr INDEX (empty)
    char(0), char(0), &                       ! count=0 (2 bytes)
    ! Padding to match test_cff_minimal size
    char(0), char(0), char(0), char(0), char(0), char(0), char(0) &  ! padding (7 bytes)
  ]
  
  print '("CFF Face Loading Debug")'
  print '("=====================")'
  print '()'
  print '("CFF data size: ", I0)', size(cff_data)
  
  ! Initialize face
  print '("Initializing CFF face...")'
  success = ft_cff_face_init(face, cff_data, size(cff_data), error)
  if (.not. success) then
    print '("ERROR: Could not initialize CFF face, error=", I0)', error
    stop
  end if
  print '("SUCCESS: CFF face initialized")'
  print '("Parser position after init: ", I0)', face%cff_parser%position
  print '()'
  
  ! Load font - this is where it fails
  print '("Loading CFF font...")'
  success = ft_cff_load_font(face, error)
  if (.not. success) then
    print '("ERROR: Could not load CFF font, error=", I0)', error
    print '("Parser position at failure: ", I0)', face%cff_parser%position
    
    ! Let's manually check what's at the current position
    if (face%cff_parser%position > 0 .and. face%cff_parser%position <= face%cff_parser%data_length) then
      print '("Data at position ", I0, ": ", I0)', face%cff_parser%position, &
        ichar(face%cff_parser%data(face%cff_parser%position))
    end if
    
    ! Try parsing Name INDEX manually
    print '()'
    print '("Attempting to parse Name INDEX manually...")'
    print '("Parser position before Name INDEX: ", I0)', face%cff_parser%position
    success = ft_cff_parse_index(face%cff_parser, face%cff_parser%name_index, error)
    if (.not. success) then
      print '("Name INDEX parse failed, error=", I0)', error
    else
      print '("Name INDEX parsed successfully")'
      print '("Name INDEX count: ", I0)', face%cff_parser%name_index%count
      print '("Parser position after Name INDEX: ", I0)', face%cff_parser%position
    end if
  else
    print '("SUCCESS: CFF font loaded")'
  end if
  
  ! Clean up
  call ft_cff_face_done(face)
  
end program test_cff_face_debug