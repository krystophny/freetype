module ft_scanline
  use ft_types
  use ft_geometry, only: FT_Vector
  use ft_raster_types
  use ft_bitmap_mod, only: FT_Bitmap, ft_bitmap_set_pixel
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, real32
  implicit none
  private
  
  ! Public types
  public :: FT_Edge
  public :: FT_Edge_Table
  public :: FT_Active_Edge_List
  
  ! Public functions
  public :: ft_edge_table_new
  public :: ft_edge_table_done
  public :: ft_edge_table_add_line
  public :: ft_edge_table_sort
  public :: ft_scanline_convert
  public :: ft_fill_scanline
  
  ! Edge structure for scan conversion
  type :: FT_Edge
    integer :: y_min      ! Minimum Y coordinate
    integer :: y_max      ! Maximum Y coordinate
    real(real32) :: x    ! Current X coordinate
    real(real32) :: dx   ! X increment per scanline
    type(FT_Edge), pointer :: next => null()
  end type FT_Edge
  
  ! Edge table - edges sorted by Y
  type :: FT_Edge_Table
    type(FT_Edge), pointer :: edges(:) => null()  ! Array of edge lists by Y
    integer :: min_y
    integer :: max_y
    integer :: height
    type(FT_Edge), pointer :: edge_pool(:) => null()  ! Pool for edge allocation
    integer :: pool_size
    integer :: pool_index
  end type FT_Edge_Table
  
  ! Active edge list for current scanline
  type :: FT_Active_Edge_List
    type(FT_Edge), pointer :: head => null()
    integer :: count
  end type FT_Active_Edge_List
  
contains

  ! Create new edge table
  function ft_edge_table_new(min_y, max_y, edge_table, error) result(success)
    integer, intent(in) :: min_y, max_y
    type(FT_Edge_Table), intent(out) :: edge_table
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: height, i
    
    success = .false.
    error = FT_Err_Ok
    
    edge_table%min_y = min_y
    edge_table%max_y = max_y
    edge_table%height = max_y - min_y + 1
    
    if (edge_table%height <= 0) then
      error = FT_Err_Invalid_Argument
      return
    end if
    
    ! Allocate edge table
    allocate(edge_table%edges(edge_table%height), stat=error)
    if (error /= 0) then
      error = FT_Err_Out_Of_Memory
      return
    end if
    
    ! Initialize edge lists as dummy heads
    do i = 1, edge_table%height
      edge_table%edges(i)%y_min = 0
      edge_table%edges(i)%y_max = 0
      edge_table%edges(i)%x = 0.0
      edge_table%edges(i)%dx = 0.0
      edge_table%edges(i)%next => null()
    end do
    
    ! Allocate edge pool
    edge_table%pool_size = 1024  ! Start with 1024 edges
    allocate(edge_table%edge_pool(edge_table%pool_size), stat=error)
    if (error /= 0) then
      deallocate(edge_table%edges)
      error = FT_Err_Out_Of_Memory
      return
    end if
    
    edge_table%pool_index = 1
    
    ! Initialize edge pool
    do i = 1, edge_table%pool_size
      edge_table%edge_pool(i)%y_min = 0
      edge_table%edge_pool(i)%y_max = 0
      edge_table%edge_pool(i)%x = 0.0
      edge_table%edge_pool(i)%dx = 0.0
      edge_table%edge_pool(i)%next => null()
    end do
    
    error = FT_Err_Ok
    success = .true.
    
  end function ft_edge_table_new
  
  ! Free edge table
  subroutine ft_edge_table_done(edge_table)
    type(FT_Edge_Table), intent(inout) :: edge_table
    
    if (associated(edge_table%edges)) then
      deallocate(edge_table%edges)
      edge_table%edges => null()
    end if
    
    if (associated(edge_table%edge_pool)) then
      deallocate(edge_table%edge_pool)
      edge_table%edge_pool => null()
    end if
    
    edge_table%pool_index = 1
    edge_table%pool_size = 0
    
  end subroutine ft_edge_table_done
  
  ! Add line to edge table
  subroutine ft_edge_table_add_line(edge_table, x0, y0, x1, y1)
    type(FT_Edge_Table), intent(inout) :: edge_table
    integer, intent(in) :: x0, y0, x1, y1
    
    type(FT_Edge), pointer :: new_edge
    integer :: y_start, y_end, y_index
    real(real32) :: x_start, dx
    
    ! Skip horizontal lines
    if (y0 == y1) return
    
    ! Order points by Y
    if (y0 < y1) then
      y_start = y0
      y_end = y1
      x_start = real(x0)
      dx = real(x1 - x0) / real(y1 - y0)
    else
      y_start = y1
      y_end = y0
      x_start = real(x1)
      dx = real(x0 - x1) / real(y0 - y1)
    end if
    
    ! Check bounds
    if (y_start < edge_table%min_y .or. y_end > edge_table%max_y) return
    if (edge_table%pool_index > edge_table%pool_size) return
    
    ! Allocate new edge from pool
    new_edge => edge_table%edge_pool(edge_table%pool_index)
    edge_table%pool_index = edge_table%pool_index + 1
    
    ! Initialize edge
    new_edge%y_min = y_start
    new_edge%y_max = y_end
    new_edge%x = x_start
    new_edge%dx = dx
    
    ! Insert into edge table at y_start
    y_index = y_start - edge_table%min_y + 1
    new_edge%next => edge_table%edges(y_index)%next
    edge_table%edges(y_index)%next => new_edge
    
  end subroutine ft_edge_table_add_line
  
  ! Sort edges in each bucket by X coordinate
  subroutine ft_edge_table_sort(edge_table)
    type(FT_Edge_Table), intent(inout) :: edge_table
    
    integer :: i
    type(FT_Edge), pointer :: list
    
    do i = 1, edge_table%height
      list => edge_table%edges(i)%next
      if (associated(list)) then
        edge_table%edges(i)%next => sort_edge_list(list)
      end if
    end do
    
  end subroutine ft_edge_table_sort
  
  ! Sort edge list by X coordinate (simple insertion sort)
  function sort_edge_list(list) result(sorted)
    type(FT_Edge), pointer :: list
    type(FT_Edge), pointer :: sorted
    
    type(FT_Edge), pointer :: current, next_edge, prev, pos
    
    if (.not. associated(list)) then
      sorted => null()
      return
    end if
    
    sorted => list
    current => list%next
    sorted%next => null()
    
    do while (associated(current))
      next_edge => current%next
      
      ! Find insertion position
      if (current%x < sorted%x) then
        ! Insert at beginning
        current%next => sorted
        sorted => current
      else
        ! Find position in sorted list
        prev => sorted
        pos => sorted%next
        
        do while (associated(pos) .and. pos%x < current%x)
          prev => pos
          pos => pos%next
        end do
        
        ! Insert after prev
        current%next => pos
        prev%next => current
      end if
      
      current => next_edge
    end do
    
  end function sort_edge_list
  
  ! Main scanline conversion
  subroutine ft_scanline_convert(edge_table, bitmap)
    type(FT_Edge_Table), intent(inout) :: edge_table
    type(FT_Bitmap), intent(inout) :: bitmap
    
    type(FT_Active_Edge_List) :: active_list
    integer :: y, y_index
    type(FT_Edge), pointer :: edge, next_edge
    
    ! Initialize active edge list
    active_list%head => null()
    active_list%count = 0
    
    ! Process each scanline
    do y = edge_table%min_y, edge_table%max_y
      y_index = y - edge_table%min_y + 1
      
      ! Add new edges starting at this scanline
      if (y_index >= 1 .and. y_index <= edge_table%height) then
        edge => edge_table%edges(y_index)%next
        do while (associated(edge))
          next_edge => edge%next
          edge%next => null()  ! Detach from bucket list
          call add_to_active_list(active_list, edge)
          edge => next_edge
        end do
      end if
      
      ! Fill scanline using active edges
      if (active_list%count > 0) then
        call ft_fill_scanline(bitmap, y, active_list)
      end if
      
      ! Update active edges and remove completed ones
      call update_active_list(active_list, y)
    end do
    
  end subroutine ft_scanline_convert
  
  ! Add edge to active list, maintaining X order
  subroutine add_to_active_list(active_list, edge)
    type(FT_Active_Edge_List), intent(inout) :: active_list
    type(FT_Edge), pointer :: edge
    
    type(FT_Edge), pointer :: prev, current
    
    if (.not. associated(edge)) return
    
    if (.not. associated(active_list%head)) then
      active_list%head => edge
      edge%next => null()
    else if (edge%x < active_list%head%x) then
      edge%next => active_list%head
      active_list%head => edge
    else
      prev => active_list%head
      current => active_list%head%next
      
      do while (associated(current) .and. current%x < edge%x)
        prev => current
        current => current%next
      end do
      
      edge%next => current
      prev%next => edge
    end if
    
    active_list%count = active_list%count + 1
    
  end subroutine add_to_active_list
  
  ! Update active edge list for next scanline
  subroutine update_active_list(active_list, y)
    type(FT_Active_Edge_List), intent(inout) :: active_list
    integer, intent(in) :: y
    
    type(FT_Edge), pointer :: prev, current, next_edge
    
    prev => null()
    current => active_list%head
    
    do while (associated(current))
      next_edge => current%next
      
      ! Remove edge if we've reached its end
      if (y >= current%y_max - 1) then
        if (associated(prev)) then
          prev%next => next_edge
        else
          active_list%head => next_edge
        end if
        active_list%count = active_list%count - 1
      else
        ! Update X coordinate for next scanline
        current%x = current%x + current%dx
        prev => current
      end if
      
      current => next_edge
    end do
    
    ! Re-sort active list by X
    if (associated(active_list%head)) then
      active_list%head => sort_edge_list(active_list%head)
    end if
    
  end subroutine update_active_list
  
  ! Fill scanline using even-odd rule
  subroutine ft_fill_scanline(bitmap, y, active_list)
    type(FT_Bitmap), intent(inout) :: bitmap
    integer, intent(in) :: y
    type(FT_Active_Edge_List), intent(in) :: active_list
    
    type(FT_Edge), pointer :: edge
    integer :: x, x_start, x_end
    logical :: inside
    
    if (y < 0 .or. y >= bitmap%rows) return
    if (.not. associated(active_list%head)) return
    
    inside = .false.
    edge => active_list%head
    
    do while (associated(edge))
      if (inside) then
        ! Fill from previous edge to current edge
        x_end = min(int(edge%x), bitmap%width - 1)
        do x = x_start, x_end
          call ft_bitmap_set_pixel(bitmap, x, y, .true.)
        end do
      else
        x_start = max(int(edge%x), 0)
      end if
      
      inside = .not. inside
      edge => edge%next
    end do
    
  end subroutine ft_fill_scanline

end module ft_scanline