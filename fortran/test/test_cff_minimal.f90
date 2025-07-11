program test_cff_minimal
  use ft_types
  use ft_cff
  use ft_cff_face
  implicit none
  
  type(FT_CFF_Face_Type) :: face
  integer(FT_Error) :: error
  logical :: success
  integer :: i
  
  ! Minimal but complete CFF data
  ! This represents the absolute minimum valid CFF file
  character(len=1), parameter :: cff_data(*) = [ &
    ! Header (4 bytes)
    char(1), char(0), char(4), char(1), &    ! major=1, minor=0, hdr_size=4, offset_size=1
    
    ! Name INDEX (min 4 bytes for empty + 8 for one name)
    char(0), char(1), &                      ! count=1
    char(1), &                               ! offset_size=1
    char(1), char(5), &                      ! offsets: 1, 5 (data is 4 bytes)
    char(84), char(101), char(115), char(116), & ! "Test"
    
    ! Top DICT INDEX (min 4 bytes for empty + content)
    char(0), char(1), &                      ! count=1
    char(1), &                               ! offset_size=1
    char(1), char(2), &                      ! offsets: 1, 2 (minimal dict)
    char(0), &                               ! empty dict
    
    ! String INDEX (4 bytes for empty)
    char(0), char(0), &                      ! count=0
    
    ! Global Subr INDEX (4 bytes for empty)
    char(0), char(0), &                      ! count=0
    
    ! Padding to ensure we don't run out of data
    char(0), char(0), char(0), char(0), &
    char(0), char(0), char(0), char(0) &
  ]
  
  print '("Minimal CFF Test")'
  print '("================")'
  print '()'
  print '("CFF data size: ", I0, " bytes")', size(cff_data)
  
  ! Show hex dump of data
  print '("Hex dump:")'
  block
    integer :: j
    do i = 1, size(cff_data), 16
      print '(16(Z2.2, 1X))', (ichar(cff_data(min(i+j-1, size(cff_data)))), j=1,min(16,size(cff_data)-i+1))
    end do
  end block
  print '()'
  
  ! Test 1: Initialize face
  print '("Test 1: Initialize CFF face")'
  success = ft_cff_face_init(face, cff_data, size(cff_data), error)
  if (.not. success) then
    print '("  FAIL: Could not initialize, error=", I0)', error
    stop
  end if
  print '("  PASS: Face initialized")'
  print '("  Parser position: ", I0)', face%cff_parser%position
  print '()'
  
  ! Test 2: Load font
  print '("Test 2: Load CFF font")'
  success = ft_cff_load_font(face, error)
  if (.not. success) then
    print '("  FAIL: Could not load font, error=", I0)', error
    print '("  Parser position at failure: ", I0)', face%cff_parser%position
  else
    print '("  PASS: Font loaded")'
    print '("  Name INDEX count: ", I0)', face%cff_parser%name_index%count
    print '("  Top DICT INDEX count: ", I0)', face%cff_parser%top_dict_index%count
    print '("  String INDEX count: ", I0)', face%cff_parser%string_index%count
    print '("  Global Subr INDEX count: ", I0)', face%cff_parser%global_subr_index%count
  end if
  print '()'
  
  ! Test 3: Get font info
  if (success) then
    print '("Test 3: Get font information")'
    success = ft_cff_get_font_info(face, error)
    if (.not. success) then
      print '("  FAIL: Could not get font info, error=", I0)', error
    else
      print '("  PASS: Font info extracted")'
      print '("  Family name: ", A)', trim(face%family_name)
      print '("  Units per EM: ", I0)', face%units_per_em
    end if
  end if
  
  ! Clean up
  call ft_cff_face_done(face)
  
  print '()'
  print '("Test completed")'
  
end program test_cff_minimal