module tt_loca
  use ft_types
  use ft_stream
  use tt_types, only: FT_UShort, FT_ULong
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int16, int32
  implicit none
  private
  
  ! Public types
  public :: TT_Loca_Table
  
  ! Public functions
  public :: tt_load_loca_table
  public :: tt_loca_get_offset
  public :: tt_loca_get_size
  public :: tt_loca_free
  
  ! Loca table - glyph location offsets
  type :: TT_Loca_Table
    logical :: is_long_format    ! True if long (32-bit) offsets, false if short (16-bit)
    integer :: num_glyphs        ! Number of glyphs
    integer(FT_ULong), allocatable :: offsets(:)  ! Glyph offsets (num_glyphs + 1)
  end type TT_Loca_Table

contains

  ! Load loca table
  function tt_load_loca_table(stream, offset, length, num_glyphs, is_long, loca, error) result(success)
    type(FT_Stream_Type), intent(inout) :: stream
    integer(c_size_t), intent(in) :: offset      ! Table offset in file
    integer(c_size_t), intent(in) :: length      ! Table length
    integer, intent(in) :: num_glyphs           ! From maxp table
    logical, intent(in) :: is_long              ! From head table indexToLocFormat
    type(TT_Loca_Table), intent(out) :: loca
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: i, expected_size
    integer(int16) :: temp16
    integer(int32) :: temp32
    
    success = .false.
    error = FT_Err_Ok
    
    ! Initialize
    loca%is_long_format = is_long
    loca%num_glyphs = num_glyphs
    
    ! Validate table size
    if (is_long) then
      expected_size = 4 * (num_glyphs + 1)
    else
      expected_size = 2 * (num_glyphs + 1)
    end if
    
    if (int(length) < expected_size) then
      error = FT_Err_Invalid_Table
      return
    end if
    
    ! Allocate offset array
    allocate(loca%offsets(0:num_glyphs))
    
    ! Seek to loca table
    if (.not. ft_stream_seek(stream, offset, error)) then
      deallocate(loca%offsets)
      error = FT_Err_Invalid_Stream_Seek
      return
    end if
    
    ! Read offsets
    if (is_long) then
      ! 32-bit offsets
      do i = 0, num_glyphs
        if (.not. ft_stream_read_long(stream, temp32, error)) then
          deallocate(loca%offsets)
          return
        end if
        loca%offsets(i) = temp32
      end do
    else
      ! 16-bit offsets (multiply by 2 to get byte offset)
      do i = 0, num_glyphs
        if (.not. ft_stream_read_short(stream, temp16, error)) then
          deallocate(loca%offsets)
          return
        end if
        ! Short offsets are stored as word offsets, convert to byte offsets
        loca%offsets(i) = int(temp16, FT_ULong) * 2
      end do
    end if
    
    ! Validate offsets are monotonic
    do i = 1, num_glyphs
      if (loca%offsets(i) < loca%offsets(i-1)) then
        deallocate(loca%offsets)
        error = FT_Err_Invalid_Table
        return
      end if
    end do
    
    success = .true.
    
  end function tt_load_loca_table
  
  ! Get offset for a glyph
  function tt_loca_get_offset(loca, glyph_index) result(offset)
    type(TT_Loca_Table), intent(in) :: loca
    integer, intent(in) :: glyph_index
    integer(FT_ULong) :: offset
    
    offset = 0
    
    if (allocated(loca%offsets)) then
      if (glyph_index >= 0 .and. glyph_index < loca%num_glyphs) then
        offset = loca%offsets(glyph_index)
      end if
    end if
    
  end function tt_loca_get_offset
  
  ! Get size of a glyph's data
  function tt_loca_get_size(loca, glyph_index) result(size)
    type(TT_Loca_Table), intent(in) :: loca
    integer, intent(in) :: glyph_index
    integer(FT_ULong) :: size
    
    size = 0
    
    if (allocated(loca%offsets)) then
      if (glyph_index >= 0 .and. glyph_index < loca%num_glyphs) then
        size = loca%offsets(glyph_index + 1) - loca%offsets(glyph_index)
      end if
    end if
    
  end function tt_loca_get_size
  
  ! Free loca table
  subroutine tt_loca_free(loca)
    type(TT_Loca_Table), intent(inout) :: loca
    
    if (allocated(loca%offsets)) deallocate(loca%offsets)
    loca%num_glyphs = 0
    loca%is_long_format = .false.
    
  end subroutine tt_loca_free

end module tt_loca