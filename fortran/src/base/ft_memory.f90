module ft_memory
  use ft_types, only: FT_Error, FT_Err_Ok, FT_Err_Out_Of_Memory
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int64
  implicit none
  private
  
  ! Public interface
  public :: ft_mem_alloc
  public :: ft_mem_free
  public :: ft_mem_realloc
  public :: ft_mem_copy
  public :: ft_mem_set
  public :: ft_mem_move
  public :: ft_mem_init
  public :: ft_mem_done
  public :: ft_mem_get_stats
  
  ! Memory statistics type
  type, public :: FT_Memory_Stats
    integer(int64) :: current_bytes = 0
    integer(int64) :: peak_bytes = 0
    integer(int64) :: total_allocations = 0
    integer(int64) :: total_deallocations = 0
    integer(int64) :: allocation_count = 0
  end type FT_Memory_Stats
  
  ! Module variables for tracking
  type(FT_Memory_Stats) :: mem_stats
  logical :: tracking_enabled = .false.
  
  ! C interop interfaces
  interface
    function c_malloc(size) bind(C, name="malloc")
      import :: c_ptr, c_size_t
      integer(c_size_t), value :: size
      type(c_ptr) :: c_malloc
    end function c_malloc
    
    subroutine c_free(ptr) bind(C, name="free")
      import :: c_ptr
      type(c_ptr), value :: ptr
    end subroutine c_free
  end interface
  
contains

  subroutine ft_mem_init(enable_tracking)
    logical, intent(in), optional :: enable_tracking
    
    ! Initialize memory tracking
    if (present(enable_tracking)) then
      tracking_enabled = enable_tracking
    else
      tracking_enabled = .false.
    end if
    
    ! Reset statistics
    mem_stats%current_bytes = 0
    mem_stats%peak_bytes = 0
    mem_stats%total_allocations = 0
    mem_stats%total_deallocations = 0
    mem_stats%allocation_count = 0
  end subroutine ft_mem_init
  
  subroutine ft_mem_done()
    ! Clean up memory system
    if (tracking_enabled .and. mem_stats%allocation_count > 0) then
      print '("WARNING: Memory leak detected - ", I0, " allocations not freed")', &
        mem_stats%allocation_count
      print '("         Leaked bytes: ", I0)', mem_stats%current_bytes
    end if
    
    tracking_enabled = .false.
  end subroutine ft_mem_done
  
  function ft_mem_alloc(size, ptr, error) result(success)
    integer(c_size_t), intent(in) :: size
    type(c_ptr), intent(out) :: ptr
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    ! Allocate memory
    ptr = c_null_ptr
    error = FT_Err_Ok
    success = .false.
    
    if (size == 0) then
      return
    end if
    
    ! Use C malloc
    ptr = c_malloc(size)
    
    if (.not. c_associated(ptr)) then
      error = FT_Err_Out_Of_Memory
      return
    end if
    
    ! Update tracking
    if (tracking_enabled) then
      mem_stats%current_bytes = mem_stats%current_bytes + int(size, int64)
      mem_stats%total_allocations = mem_stats%total_allocations + 1
      mem_stats%allocation_count = mem_stats%allocation_count + 1
      
      if (mem_stats%current_bytes > mem_stats%peak_bytes) then
        mem_stats%peak_bytes = mem_stats%current_bytes
      end if
    end if
    
    success = .true.
  end function ft_mem_alloc
  
  subroutine ft_mem_free(ptr, size)
    type(c_ptr), intent(inout) :: ptr
    integer(c_size_t), intent(in), optional :: size
    
    if (.not. c_associated(ptr)) return
    
    ! Free memory
    call c_free(ptr)
    
    ! Update tracking
    if (tracking_enabled .and. present(size)) then
      mem_stats%current_bytes = mem_stats%current_bytes - int(size, int64)
      mem_stats%total_deallocations = mem_stats%total_deallocations + 1
      mem_stats%allocation_count = mem_stats%allocation_count - 1
    end if
    
    ptr = c_null_ptr
  end subroutine ft_mem_free
  
  function ft_mem_realloc(ptr, old_size, new_size, error) result(new_ptr)
    type(c_ptr), intent(inout) :: ptr
    integer(c_size_t), intent(in) :: old_size, new_size
    integer(FT_Error), intent(out) :: error
    type(c_ptr) :: new_ptr
    
    error = FT_Err_Ok
    new_ptr = c_null_ptr
    
    ! Handle special cases
    if (new_size == 0) then
      if (c_associated(ptr)) then
        call ft_mem_free(ptr, old_size)
      end if
      return
    end if
    
    if (.not. c_associated(ptr)) then
      ! Just allocate new memory
      if (.not. ft_mem_alloc(new_size, new_ptr, error)) then
        return
      end if
    else
      ! Allocate new block
      if (.not. ft_mem_alloc(new_size, new_ptr, error)) then
        return
      end if
      
      ! Copy old data
      if (old_size > 0) then
        call ft_mem_copy(new_ptr, ptr, min(old_size, new_size))
      end if
      
      ! Free old block
      call ft_mem_free(ptr, old_size)
    end if
    
  end function ft_mem_realloc
  
  subroutine ft_mem_copy(dest, src, size)
    type(c_ptr), intent(in) :: dest, src
    integer(c_size_t), intent(in) :: size
    character(c_char), pointer :: dest_ptr(:), src_ptr(:)
    
    if (.not. c_associated(dest) .or. .not. c_associated(src)) return
    if (size == 0) return
    
    ! Use pointers to copy memory
    call c_f_pointer(dest, dest_ptr, [size])
    call c_f_pointer(src, src_ptr, [size])
    dest_ptr = src_ptr
    
  end subroutine ft_mem_copy
  
  subroutine ft_mem_set(ptr, value, size)
    type(c_ptr), intent(in) :: ptr
    integer(c_int8_t), intent(in) :: value
    integer(c_size_t), intent(in) :: size
    character(c_char), pointer :: mem_ptr(:)
    
    if (.not. c_associated(ptr)) return
    if (size == 0) return
    
    call c_f_pointer(ptr, mem_ptr, [size])
    mem_ptr = char(value)
  end subroutine ft_mem_set
  
  subroutine ft_mem_move(dest, src, size)
    type(c_ptr), intent(in) :: dest, src
    integer(c_size_t), intent(in) :: size
    character(c_char), allocatable :: temp(:)
    character(c_char), pointer :: dest_ptr(:), src_ptr(:)
    
    if (.not. c_associated(dest) .or. .not. c_associated(src)) return
    if (size == 0) return
    
    ! For overlapping memory, use temporary buffer
    allocate(temp(size))
    
    call c_f_pointer(src, src_ptr, [size])
    temp = src_ptr
    
    call c_f_pointer(dest, dest_ptr, [size])
    dest_ptr = temp
    
    deallocate(temp)
  end subroutine ft_mem_move
  
  function ft_mem_get_stats() result(stats)
    type(FT_Memory_Stats) :: stats
    
    stats = mem_stats
  end function ft_mem_get_stats

end module ft_memory