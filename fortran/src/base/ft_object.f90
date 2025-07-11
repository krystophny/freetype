module ft_object
  use ft_types, only: FT_Error, FT_Err_Ok
  use ft_memory
  use, intrinsic :: iso_c_binding
  implicit none
  private
  
  ! Public types
  public :: FT_Object_Type
  public :: FT_Object_Class
  public :: FT_List
  public :: FT_ListNode
  
  ! Object operations
  public :: ft_object_create
  public :: ft_object_init
  public :: ft_object_done
  public :: ft_object_ref
  public :: ft_object_unref
  public :: ft_object_get_class_id
  
  ! List operations
  public :: ft_list_init
  public :: ft_list_done
  public :: ft_list_add
  public :: ft_list_remove
  public :: ft_list_find
  public :: ft_list_iterate
  public :: ft_list_count
  
  ! Class IDs for runtime type identification
  integer, parameter, public :: FT_CLASS_OBJECT = 0
  integer, parameter, public :: FT_CLASS_FACE = 1
  integer, parameter, public :: FT_CLASS_SIZE = 2
  integer, parameter, public :: FT_CLASS_GLYPH = 3
  integer, parameter, public :: FT_CLASS_CHARMAP = 4
  
  ! Object class type (vtable-like structure)
  type :: FT_Object_Class
    integer :: class_id = FT_CLASS_OBJECT
    integer :: object_size = 0
    procedure(object_init_proc), pointer, nopass :: init => null()
    procedure(object_done_proc), pointer, nopass :: done => null()
  end type FT_Object_Class
  
  ! Base object type
  type :: FT_Object_Type
    type(FT_Object_Class), pointer :: clazz => null()
    integer :: ref_count = 1
    type(c_ptr) :: internal = c_null_ptr  ! For subclass data
  end type FT_Object_Type
  
  ! List node type
  type :: FT_ListNode
    type(c_ptr) :: data = c_null_ptr
    type(FT_ListNode), pointer :: next => null()
    type(FT_ListNode), pointer :: prev => null()
  end type FT_ListNode
  
  ! List type
  type :: FT_List
    type(FT_ListNode), pointer :: head => null()
    type(FT_ListNode), pointer :: tail => null()
    integer :: count = 0
  end type FT_List
  
  ! Abstract interfaces for object methods
  abstract interface
    subroutine object_init_proc(object, error)
      import :: FT_Object_Type, FT_Error
      type(FT_Object_Type), intent(inout) :: object
      integer(FT_Error), intent(out) :: error
    end subroutine object_init_proc
    
    subroutine object_done_proc(object)
      import :: FT_Object_Type
      type(FT_Object_Type), intent(inout) :: object
    end subroutine object_done_proc
  end interface
  
  ! Iterator callback interface
  abstract interface
    logical function list_iterator_func(node, user_data)
      import :: FT_ListNode, c_ptr
      type(FT_ListNode), intent(in) :: node
      type(c_ptr), intent(in) :: user_data
    end function list_iterator_func
  end interface

contains

  ! Object operations
  
  function ft_object_create(clazz, object, error) result(success)
    type(FT_Object_Class), target, intent(in) :: clazz
    type(FT_Object_Type), intent(out) :: object
    integer(FT_Error), intent(out) :: error
    logical :: success
    
    success = .false.
    error = FT_Err_Ok
    
    ! Initialize object
    object%clazz => clazz
    object%ref_count = 1
    object%internal = c_null_ptr
    
    ! Allocate internal data if needed
    if (clazz%object_size > 0) then
      success = ft_mem_alloc(int(clazz%object_size, c_size_t), &
                            object%internal, error)
      if (.not. success) return
    end if
    
    ! Call init method if provided
    if (associated(clazz%init)) then
      call clazz%init(object, error)
      if (error /= FT_Err_Ok) then
        if (c_associated(object%internal)) then
          call ft_mem_free(object%internal, int(clazz%object_size, c_size_t))
        end if
        return
      end if
    end if
    
    success = .true.
  end function ft_object_create
  
  subroutine ft_object_init(object, clazz)
    type(FT_Object_Type), intent(out) :: object
    type(FT_Object_Class), target, intent(in) :: clazz
    
    object%clazz => clazz
    object%ref_count = 1
    object%internal = c_null_ptr
  end subroutine ft_object_init
  
  subroutine ft_object_done(object)
    type(FT_Object_Type), intent(inout) :: object
    
    if (.not. associated(object%clazz)) return
    
    ! Call done method if provided
    if (associated(object%clazz%done)) then
      call object%clazz%done(object)
    end if
    
    ! Free internal data
    if (c_associated(object%internal) .and. object%clazz%object_size > 0) then
      call ft_mem_free(object%internal, int(object%clazz%object_size, c_size_t))
    end if
    
    object%clazz => null()
    object%ref_count = 0
    object%internal = c_null_ptr
  end subroutine ft_object_done
  
  subroutine ft_object_ref(object)
    type(FT_Object_Type), intent(inout) :: object
    
    if (associated(object%clazz)) then
      object%ref_count = object%ref_count + 1
    end if
  end subroutine ft_object_ref
  
  function ft_object_unref(object) result(destroyed)
    type(FT_Object_Type), intent(inout) :: object
    logical :: destroyed
    
    destroyed = .false.
    
    if (.not. associated(object%clazz)) return
    
    object%ref_count = object%ref_count - 1
    
    if (object%ref_count <= 0) then
      call ft_object_done(object)
      destroyed = .true.
    end if
  end function ft_object_unref
  
  function ft_object_get_class_id(object) result(class_id)
    type(FT_Object_Type), intent(in) :: object
    integer :: class_id
    
    if (associated(object%clazz)) then
      class_id = object%clazz%class_id
    else
      class_id = -1
    end if
  end function ft_object_get_class_id
  
  ! List operations
  
  subroutine ft_list_init(list)
    type(FT_List), intent(out) :: list
    
    list%head => null()
    list%tail => null()
    list%count = 0
  end subroutine ft_list_init
  
  subroutine ft_list_done(list)
    type(FT_List), intent(inout) :: list
    type(FT_ListNode), pointer :: node, next
    
    node => list%head
    do while (associated(node))
      next => node%next
      deallocate(node)
      node => next
    end do
    
    list%head => null()
    list%tail => null()
    list%count = 0
  end subroutine ft_list_done
  
  function ft_list_add(list, data, error) result(node)
    type(FT_List), intent(inout) :: list
    type(c_ptr), intent(in) :: data
    integer(FT_Error), intent(out) :: error
    type(FT_ListNode), pointer :: node
    
    error = FT_Err_Ok
    
    allocate(node)
    node%data = data
    node%next => null()
    node%prev => list%tail
    
    if (associated(list%tail)) then
      list%tail%next => node
    else
      list%head => node
    end if
    
    list%tail => node
    list%count = list%count + 1
  end function ft_list_add
  
  subroutine ft_list_remove(list, node)
    type(FT_List), intent(inout) :: list
    type(FT_ListNode), pointer, intent(inout) :: node
    
    if (.not. associated(node)) return
    
    ! Update links
    if (associated(node%prev)) then
      node%prev%next => node%next
    else
      list%head => node%next
    end if
    
    if (associated(node%next)) then
      node%next%prev => node%prev
    else
      list%tail => node%prev
    end if
    
    list%count = list%count - 1
    
    deallocate(node)
    node => null()
  end subroutine ft_list_remove
  
  function ft_list_find(list, data) result(node)
    type(FT_List), intent(in) :: list
    type(c_ptr), intent(in) :: data
    type(FT_ListNode), pointer :: node
    
    node => list%head
    do while (associated(node))
      if (c_associated(node%data, data)) return
      node => node%next
    end do
    
    node => null()
  end function ft_list_find
  
  subroutine ft_list_iterate(list, iterator, user_data)
    type(FT_List), intent(in) :: list
    procedure(list_iterator_func) :: iterator
    type(c_ptr), intent(in) :: user_data
    type(FT_ListNode), pointer :: node, next
    logical :: continue
    
    node => list%head
    do while (associated(node))
      next => node%next
      continue = iterator(node, user_data)
      if (.not. continue) exit
      node => next
    end do
  end subroutine ft_list_iterate
  
  function ft_list_count(list) result(count)
    type(FT_List), intent(in) :: list
    integer :: count
    
    count = list%count
  end function ft_list_count

end module ft_object