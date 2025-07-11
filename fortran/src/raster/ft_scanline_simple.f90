module ft_scanline_simple
  use ft_types
  use ft_geometry, only: FT_Vector
  use ft_bitmap_mod, only: FT_Bitmap, ft_bitmap_set_pixel, ft_bitmap_clear
  use ft_outline_mod, only: FT_Outline
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, real32
  implicit none
  private
  
  ! Public functions
  public :: ft_render_outline_filled
  
  ! Simple edge for scanline algorithm
  type :: Edge
    real(real32) :: x_start
    real(real32) :: x_end
    integer :: y_start
    integer :: y_end
    real(real32) :: dx
    real(real32) :: current_x
  end type Edge
  
contains

  ! Render filled outline using simple scanline algorithm
  function ft_render_outline_filled(outline, bitmap, error) result(success)
    type(FT_Outline), intent(in) :: outline
    type(FT_Bitmap), intent(inout) :: bitmap
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    type(Edge), allocatable :: edges(:)
    integer :: n_edges, i, j, y
    integer :: x0, y0, x1, y1
    integer :: first, last
    logical, allocatable :: scanline(:)
    
    success = .false.
    error = FT_Err_Ok
    
    ! Clear bitmap
    call ft_bitmap_clear(bitmap)
    
    ! Count edges
    n_edges = 0
    do i = 1, outline%n_contours
      n_edges = n_edges + outline%contours(i) + 1
      if (i > 1) n_edges = n_edges - outline%contours(i-1) - 1
    end do
    
    if (n_edges == 0) then
      success = .true.
      return
    end if
    
    ! Allocate edge array
    allocate(edges(n_edges), stat=error)
    if (error /= 0) then
      error = FT_Err_Out_Of_Memory
      return
    end if
    
    ! Build edges from outline
    n_edges = 0
    first = 1
    do i = 1, outline%n_contours
      last = outline%contours(i) + 1  ! Convert to 1-based
      
      ! Add edges for this contour
      do j = first, last - 1
        x0 = int(outline%points(j)%x / 256)
        y0 = int(outline%points(j)%y / 256)
        x1 = int(outline%points(j + 1)%x / 256)
        y1 = int(outline%points(j + 1)%y / 256)
        
        if (y0 /= y1) then  ! Skip horizontal edges
          n_edges = n_edges + 1
          if (y0 < y1) then
            edges(n_edges)%x_start = real(x0)
            edges(n_edges)%x_end = real(x1)
            edges(n_edges)%y_start = y0
            edges(n_edges)%y_end = y1
          else
            edges(n_edges)%x_start = real(x1)
            edges(n_edges)%x_end = real(x0)
            edges(n_edges)%y_start = y1
            edges(n_edges)%y_end = y0
          end if
          edges(n_edges)%dx = (edges(n_edges)%x_end - edges(n_edges)%x_start) / &
                               real(edges(n_edges)%y_end - edges(n_edges)%y_start)
          edges(n_edges)%current_x = edges(n_edges)%x_start
        end if
      end do
      
      ! Close contour
      x0 = int(outline%points(last)%x / 256)
      y0 = int(outline%points(last)%y / 256)
      x1 = int(outline%points(first)%x / 256)
      y1 = int(outline%points(first)%y / 256)
      
      if (y0 /= y1) then
        n_edges = n_edges + 1
        if (y0 < y1) then
          edges(n_edges)%x_start = real(x0)
          edges(n_edges)%x_end = real(x1)
          edges(n_edges)%y_start = y0
          edges(n_edges)%y_end = y1
        else
          edges(n_edges)%x_start = real(x1)
          edges(n_edges)%x_end = real(x0)
          edges(n_edges)%y_start = y1
          edges(n_edges)%y_end = y0
        end if
        edges(n_edges)%dx = (edges(n_edges)%x_end - edges(n_edges)%x_start) / &
                             real(edges(n_edges)%y_end - edges(n_edges)%y_start)
        edges(n_edges)%current_x = edges(n_edges)%x_start
      end if
      
      first = last + 1
    end do
    
    ! Allocate scanline buffer
    allocate(scanline(bitmap%width))
    
    ! Process each scanline
    do y = 0, bitmap%rows - 1
      scanline = .false.
      
      ! Find all edges that intersect this scanline
      do i = 1, n_edges
        if (y >= edges(i)%y_start .and. y < edges(i)%y_end) then
          ! Calculate X intersection
          x0 = int(edges(i)%current_x + 0.5)
          if (x0 >= 0 .and. x0 < bitmap%width) then
            scanline(x0 + 1) = .not. scanline(x0 + 1)
          end if
        end if
      end do
      
      ! Fill scanline using even-odd rule
      j = 0
      do i = 1, bitmap%width
        if (scanline(i)) j = 1 - j
        if (j == 1) then
          call ft_bitmap_set_pixel(bitmap, i - 1, y, .true.)
        end if
      end do
      
      ! Update edge X coordinates for next scanline
      do i = 1, n_edges
        if (y >= edges(i)%y_start .and. y < edges(i)%y_end - 1) then
          edges(i)%current_x = edges(i)%current_x + edges(i)%dx
        end if
      end do
    end do
    
    ! Cleanup
    deallocate(edges)
    deallocate(scanline)
    
    error = FT_Err_Ok
    success = .true.
    
  end function ft_render_outline_filled

end module ft_scanline_simple