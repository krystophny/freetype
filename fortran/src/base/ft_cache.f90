module ft_cache
  use ft_types
  use ft_memory, only: ft_mem_alloc, ft_mem_free
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int32, int64
  implicit none
  private
  
  ! Public types
  public :: FT_Cache_Entry
  public :: FT_Cache_Manager
  
  ! Public functions
  public :: ft_cache_manager_new
  public :: ft_cache_manager_done
  public :: ft_cache_lookup_glyph
  public :: ft_cache_store_glyph
  public :: ft_cache_clear
  
  ! Cache configuration
  integer, parameter :: FT_CACHE_MAX_ENTRIES = 256
  integer, parameter :: FT_CACHE_HASH_SIZE = 64
  
  ! Cache entry
  type :: FT_Cache_Entry
    integer :: face_id = 0
    integer :: glyph_index = 0
    integer :: pixel_size = 0
    integer(int64) :: timestamp = 0
    integer :: data_size = 0
    integer(int8), allocatable :: glyph_data(:)
    type(FT_Cache_Entry), pointer :: next => null()
  end type FT_Cache_Entry
  
  ! Cache manager
  type :: FT_Cache_Manager  
    type(FT_Cache_Entry), pointer :: hash_table(:) => null()
    integer :: num_entries = 0
    integer :: max_entries = FT_CACHE_MAX_ENTRIES
    integer(int64) :: current_time = 0
    logical :: initialized = .false.
  end type FT_Cache_Manager

contains

  ! Create new cache manager
  function ft_cache_manager_new(manager, error) result(success)
    type(FT_Cache_Manager), intent(out) :: manager
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    integer :: i
    
    success = .false.
    error = FT_Err_Ok
    
    ! Allocate hash table
    allocate(manager%hash_table(FT_CACHE_HASH_SIZE), stat=error)
    if (error /= 0) then
      error = FT_Err_Out_Of_Memory
      return
    end if
    
    ! Initialize hash table
    do i = 1, FT_CACHE_HASH_SIZE
      manager%hash_table(i) => null()
    end do
    
    manager%num_entries = 0
    manager%current_time = 0
    manager%initialized = .true.
    
    success = .true.
    
  end function ft_cache_manager_new
  
  ! Clean up cache manager
  subroutine ft_cache_manager_done(manager)
    type(FT_Cache_Manager), intent(inout) :: manager
    
    if (manager%initialized) then
      call ft_cache_clear(manager)
      
      if (associated(manager%hash_table)) then
        deallocate(manager%hash_table)
        manager%hash_table => null()
      end if
      
      manager%initialized = .false.
    end if
    
  end subroutine ft_cache_manager_done
  
  ! Look up glyph in cache
  function ft_cache_lookup_glyph(manager, face_id, glyph_index, pixel_size, &
                                 glyph_data, data_size, error) result(found)
    type(FT_Cache_Manager), intent(inout) :: manager
    integer, intent(in) :: face_id, glyph_index, pixel_size
    integer(int8), allocatable, intent(out) :: glyph_data(:)
    integer, intent(out) :: data_size
    integer(FT_Error), intent(out) :: error
    logical :: found
    
    type(FT_Cache_Entry), pointer :: entry
    integer :: hash_index
    
    found = .false.
    error = FT_Err_Ok
    data_size = 0
    
    if (.not. manager%initialized) then
      error = FT_Err_Invalid_Handle
      return
    end if
    
    ! Calculate hash
    hash_index = compute_hash(face_id, glyph_index, pixel_size)
    
    ! Search in hash bucket
    entry => manager%hash_table(hash_index)
    do while (associated(entry))
      if (entry%face_id == face_id .and. &
          entry%glyph_index == glyph_index .and. &
          entry%pixel_size == pixel_size) then
        
        ! Found - copy data and update timestamp
        data_size = entry%data_size
        if (allocated(entry%glyph_data)) then
          allocate(glyph_data(data_size))
          glyph_data = entry%glyph_data
        end if
        
        manager%current_time = manager%current_time + 1
        entry%timestamp = manager%current_time
        
        found = .true.
        return
      end if
      
      entry => entry%next
    end do
    
  end function ft_cache_lookup_glyph
  
  ! Store glyph in cache
  function ft_cache_store_glyph(manager, face_id, glyph_index, pixel_size, &
                                glyph_data, data_size, error) result(success)
    type(FT_Cache_Manager), intent(inout) :: manager
    integer, intent(in) :: face_id, glyph_index, pixel_size
    integer(int8), intent(in) :: glyph_data(:)
    integer, intent(in) :: data_size
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    type(FT_Cache_Entry), pointer :: entry
    integer :: hash_index
    
    success = .false.
    error = FT_Err_Ok
    
    if (.not. manager%initialized) then
      error = FT_Err_Invalid_Handle
      return
    end if
    
    ! Make room if cache is full
    if (manager%num_entries >= manager%max_entries) then
      call evict_oldest_entry(manager)
    end if
    
    ! Allocate new entry
    allocate(entry, stat=error)
    if (error /= 0) then
      error = FT_Err_Out_Of_Memory
      return
    end if
    
    ! Initialize entry
    entry%face_id = face_id
    entry%glyph_index = glyph_index
    entry%pixel_size = pixel_size
    entry%data_size = data_size
    
    manager%current_time = manager%current_time + 1
    entry%timestamp = manager%current_time
    
    ! Copy glyph data
    if (data_size > 0) then
      allocate(entry%glyph_data(data_size), stat=error)
      if (error /= 0) then
        deallocate(entry)
        error = FT_Err_Out_Of_Memory
        return
      end if
      entry%glyph_data = glyph_data(1:data_size)
    end if
    
    ! Insert into hash table
    hash_index = compute_hash(face_id, glyph_index, pixel_size)
    entry%next => manager%hash_table(hash_index)
    manager%hash_table(hash_index) => entry
    
    manager%num_entries = manager%num_entries + 1
    success = .true.
    
  end function ft_cache_store_glyph
  
  ! Clear all cache entries
  subroutine ft_cache_clear(manager)
    type(FT_Cache_Manager), intent(inout) :: manager
    
    type(FT_Cache_Entry), pointer :: entry, next_entry
    integer :: i
    
    if (.not. manager%initialized) return
    
    ! Clear all hash buckets
    do i = 1, FT_CACHE_HASH_SIZE
      entry => manager%hash_table(i)
      
      do while (associated(entry))
        next_entry => entry%next
        
        if (allocated(entry%glyph_data)) then
          deallocate(entry%glyph_data)
        end if
        deallocate(entry)
        
        entry => next_entry
      end do
      
      manager%hash_table(i) => null()
    end do
    
    manager%num_entries = 0
    
  end subroutine ft_cache_clear
  
  ! Compute hash for cache key
  function compute_hash(face_id, glyph_index, pixel_size) result(hash_index)
    integer, intent(in) :: face_id, glyph_index, pixel_size
    integer :: hash_index
    
    integer :: hash_value
    
    ! Simple hash function
    hash_value = face_id * 31 + glyph_index * 17 + pixel_size * 7
    hash_index = mod(abs(hash_value), FT_CACHE_HASH_SIZE) + 1
    
  end function compute_hash
  
  ! Evict oldest cache entry
  subroutine evict_oldest_entry(manager)
    type(FT_Cache_Manager), intent(inout) :: manager
    
    type(FT_Cache_Entry), pointer :: oldest_entry, entry, prev_entry
    integer(int64) :: oldest_time
    integer :: i, oldest_bucket
    logical :: found_oldest
    
    oldest_time = manager%current_time + 1
    oldest_entry => null()
    oldest_bucket = 1
    found_oldest = .false.
    
    ! Find oldest entry across all buckets
    do i = 1, FT_CACHE_HASH_SIZE
      entry => manager%hash_table(i)
      
      do while (associated(entry))
        if (entry%timestamp < oldest_time) then
          oldest_time = entry%timestamp
          oldest_entry => entry
          oldest_bucket = i
          found_oldest = .true.
        end if
        entry => entry%next
      end do
    end do
    
    ! Remove oldest entry if found
    if (found_oldest .and. associated(oldest_entry)) then
      ! Find and remove from linked list
      if (associated(manager%hash_table(oldest_bucket), oldest_entry)) then
        ! First entry in bucket
        manager%hash_table(oldest_bucket) => oldest_entry%next
      else
        ! Find previous entry
        entry => manager%hash_table(oldest_bucket)
        do while (associated(entry))
          if (associated(entry%next, oldest_entry)) then
            entry%next => oldest_entry%next
            exit
          end if
          entry => entry%next
        end do
      end if
      
      ! Clean up entry
      if (allocated(oldest_entry%glyph_data)) then
        deallocate(oldest_entry%glyph_data)
      end if
      deallocate(oldest_entry)
      
      manager%num_entries = manager%num_entries - 1
    end if
    
  end subroutine evict_oldest_entry

end module ft_cache