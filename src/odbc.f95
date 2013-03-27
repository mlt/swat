module odbc

  use fodbc_ext
  implicit none

  ! private :: print_diag

  type db
     type(c_ptr) :: dbc = C_NULL_PTR
     type(c_ptr) :: env = C_NULL_PTR
     contains
       procedure, pass :: connect
       procedure, pass :: disconnect
  end type db

  type(db) :: db_out
  type(db) :: db_in

contains

  function allocate_statement(self)
    class(db) :: self
    type(c_ptr) :: allocate_statement
    allocate_statement = C_NULL_PTR
    if (SQL_SUCCESS /= SQLAllocHandle(SQL_HANDLE_STMT, self%dbc, allocate_statement)) then
       write(*,*) "Can't allocate statement handle"
       call print_diag(SQL_HANDLE_STMT, allocate_statement)
       stop
    end if
  end function allocate_statement


  function connect(self, connstr)
    class(db) :: self
    character(*), intent(in) :: connstr
    logical :: connect
    connect = .false.
    write (*, '("Connection string: ", A)') connstr
    if (SQL_SUCCESS /= SQLAllocHandle(SQL_HANDLE_ENV, C_NULL_PTR, self%env)) then
       print *, "Can't allocate environment"
       return
    end if
    if (SQL_SUCCESS /= SQLSetEnvAttr(self%env, SQL_ATTR_ODBC_VERSION, transfer(SQL_OV_ODBC3, C_NULL_PTR), 0)) then
       print *, "Can't request version"
       return
    end if
    if (SQL_SUCCESS /= SQLAllocHandle(SQL_HANDLE_DBC, self%env, self%dbc)) then
       print *, "Can't allocate DBC handle"
       return
    end if
    if (SQL_SUCCESS /= SQLDriverConnect(self%dbc, C_NULL_PTR, connstr // C_NULL_CHAR, &
         int(len_trim(connstr), 2), C_STR_NULL_PTR, 0_2, C_SHORT_NULL_PTR, SQL_DRIVER_COMPLETE)) then
       print *, "Can't connect"
       call print_diag(SQL_HANDLE_DBC, self%dbc)
       return
    end if
    connect = .true.
  end function connect


  subroutine disconnect(self)
    class(db) :: self
    if (c_associated(self%dbc)) then
       if (SQL_SUCCESS /= SQLDisconnect(self%dbc)) &
            print *, "Failed to disconnect"
       if (SQL_SUCCESS /= SQLFreeHandle(SQL_HANDLE_DBC, self%dbc)) &
            print *, "Failed to free DB handle"
    end if
    if (c_associated(self%env)) then
       if (SQL_SUCCESS /= SQLFreeHandle(SQL_HANDLE_ENV, self%env)) &
            print *, "Failed to free environment handle"
    end if
  end subroutine disconnect


  subroutine print_diag(type, handle)
    integer(c_short) :: type
    type(c_ptr) :: handle
    integer(c_short) :: length
    character(SQL_MAX_MESSAGE_LENGTH, c_char) :: msg
    integer(c_int) :: native
    character(6, c_char) :: state
    integer(c_short) :: idx
    idx = 1
    do while (SQL_SUCCESS == SQLGetDiagRec(type, handle, idx, &
         state, native, msg, SQL_MAX_MESSAGE_LENGTH, length))
       write(*,*) msg(1:length)
       idx = idx + 1
    end do
  end subroutine print_diag

end module odbc
