module tt_hmtx
  use ft_types
  use ft_stream
  use tt_types, only: FT_UShort, FT_Short
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int16, int32
  implicit none
  private
  
  ! Public types
  public :: TT_HMetric
  public :: TT_HMtx_Table
  
  ! Public functions
  public :: tt_load_hmtx_table
  public :: tt_hmtx_get_advance
  public :: tt_hmtx_get_lsb
  public :: tt_hmtx_free
  
  ! Horizontal metric record
  type :: TT_HMetric
    integer(FT_UShort) :: advance_width    ! Advance width, in font units
    integer(FT_Short) :: lsb               ! Left side bearing, in font units
  end type TT_HMetric
  
  ! Horizontal metrics table
  type :: TT_HMtx_Table
    integer :: num_metrics            ! Number of hMetrics (from hhea)
    integer :: num_glyphs            ! Total number of glyphs (from maxp)
    type(TT_HMetric), allocatable :: metrics(:)     ! Array of metrics
    integer(FT_Short), allocatable :: left_side_bearings(:)  ! Additional LSBs
  end type TT_HMtx_Table

contains

  ! Load hmtx table
  function tt_load_hmtx_table(stream, offset, num_metrics, num_glyphs, hmtx, error) result(success)
    type(FT_Stream_Type), intent(inout) :: stream
    integer(c_size_t), intent(in) :: offset          ! Table offset in file
    integer, intent(in) :: num_metrics              ! From hhea table
    integer, intent(in) :: num_glyphs               ! From maxp table
    type(TT_HMtx_Table), intent(out) :: hmtx
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: i, num_lsbs
    integer(int16) :: temp16
    
    success = .false.
    error = FT_Err_Ok
    
    ! Validate parameters
    if (num_metrics == 0 .or. num_metrics > num_glyphs) then
      error = FT_Err_Invalid_Argument
      return
    end if
    
    ! Initialize
    hmtx%num_metrics = num_metrics
    hmtx%num_glyphs = num_glyphs
    
    ! Seek to hmtx table
    if (.not. ft_stream_seek(stream, offset, error)) then
      error = FT_Err_Invalid_Stream_Seek
      return
    end if
    
    ! Allocate metrics array
    allocate(hmtx%metrics(num_metrics))
    
    ! Read hMetrics
    do i = 1, num_metrics
      ! Read advance width
      if (.not. ft_stream_read_short(stream, temp16, error)) then
        deallocate(hmtx%metrics)
        return
      end if
      hmtx%metrics(i)%advance_width = temp16
      
      ! Read left side bearing
      if (.not. ft_stream_read_short(stream, temp16, error)) then
        deallocate(hmtx%metrics)
        return
      end if
      hmtx%metrics(i)%lsb = temp16
    end do
    
    ! If there are more glyphs than metrics, read additional LSBs
    num_lsbs = num_glyphs - num_metrics
    if (num_lsbs > 0) then
      allocate(hmtx%left_side_bearings(num_lsbs))
      
      do i = 1, num_lsbs
        if (.not. ft_stream_read_short(stream, temp16, error)) then
          deallocate(hmtx%metrics)
          deallocate(hmtx%left_side_bearings)
          return
        end if
        hmtx%left_side_bearings(i) = temp16
      end do
    end if
    
    success = .true.
    
  end function tt_load_hmtx_table
  
  ! Get advance width for a glyph
  function tt_hmtx_get_advance(hmtx, glyph_index) result(advance)
    type(TT_HMtx_Table), intent(in) :: hmtx
    integer, intent(in) :: glyph_index
    integer(FT_UShort) :: advance
    
    advance = 0
    
    if (glyph_index < 0 .or. glyph_index >= hmtx%num_glyphs) return
    
    if (allocated(hmtx%metrics)) then
      if (glyph_index < hmtx%num_metrics) then
        advance = hmtx%metrics(glyph_index + 1)%advance_width
      else
        ! Use the last advance width for all remaining glyphs
        advance = hmtx%metrics(hmtx%num_metrics)%advance_width
      end if
    end if
    
  end function tt_hmtx_get_advance
  
  ! Get left side bearing for a glyph
  function tt_hmtx_get_lsb(hmtx, glyph_index) result(lsb)
    type(TT_HMtx_Table), intent(in) :: hmtx
    integer, intent(in) :: glyph_index
    integer(FT_Short) :: lsb
    
    lsb = 0
    
    if (glyph_index < 0 .or. glyph_index >= hmtx%num_glyphs) return
    
    if (glyph_index < hmtx%num_metrics) then
      if (allocated(hmtx%metrics)) then
        lsb = hmtx%metrics(glyph_index + 1)%lsb
      end if
    else
      ! Use the additional LSB array
      if (allocated(hmtx%left_side_bearings)) then
        lsb = hmtx%left_side_bearings(glyph_index - hmtx%num_metrics + 1)
      end if
    end if
    
  end function tt_hmtx_get_lsb
  
  ! Free hmtx table
  subroutine tt_hmtx_free(hmtx)
    type(TT_HMtx_Table), intent(inout) :: hmtx
    
    if (allocated(hmtx%metrics)) deallocate(hmtx%metrics)
    if (allocated(hmtx%left_side_bearings)) deallocate(hmtx%left_side_bearings)
    hmtx%num_metrics = 0
    hmtx%num_glyphs = 0
    
  end subroutine tt_hmtx_free

end module tt_hmtx