module odbc

  use fodbc
  use parm, only: msubo
  implicit none

  type db_type
     type(c_ptr) :: dbc = C_NULL_PTR
     type(c_ptr) :: env = C_NULL_PTR
   contains
     procedure, pass :: allocate_statement
     procedure, pass :: connect
     procedure, pass :: disconnect => disconnect_db
     ! http://gcc.gnu.org/bugzilla/show_bug.cgi?id=37336
     ! final :: disconnect
  end type db_type

  type, extends(db_type) :: db_out_type
     type(c_ptr) :: sub_stmt = C_NULL_PTR
   contains
     procedure, pass :: create_sub
     procedure, pass :: create_tables
     procedure, pass :: disconnect => disconnect_db_out
     procedure, pass :: prepare
     procedure, pass :: prepare_sub
  end type db_out_type

  type(db_out_type) :: db_out
  type(db_type) :: db_in

  ! Private stuff

  ! similar to hedb but with valid SQL names
  character(10), dimension(msubo), private, parameter :: sub_columns = (/ &
       "PRECIPmm  ", "SNOMELTmm ", "PETmm     ", "ETmm      ", &
       "SWmm      ", "PERCmm    ", "SURQmm    ", "GW_Qmm    ", &
       "WYLDmm    ", "SYLDt_ha  ", "ORGNkg_ha ", "ORGPkg_ha ", &
       "NSURQkg_ha", "SOLPkg_ha ", "SEDPkg_ha ", "LAT_Qmm   ", &
       "LATNO3kg_h", "GWNO3kg_ha", "CHOLAmic_L", "CBODUmg_L ", &
       "DOXQmg_L  ", "TNO3kg_ha " &
       /)

contains

  function allocate_statement(self)
    class(db_type) :: self
    type(c_ptr) :: allocate_statement
    allocate_statement = C_NULL_PTR
    if (SQL_SUCCESS /= SQLAllocHandle(SQL_HANDLE_STMT, self%dbc, allocate_statement)) then
       write(*,*) "Can't allocate statement handle"
       call print_diag(SQL_HANDLE_STMT, allocate_statement)
       stop
    end if
  end function allocate_statement


  function connect(self, connstr)
    class(db_type) :: self
    character(*), intent(in) :: connstr
    logical :: connect
    connect = .false.
    write (*, '("Connection string: ", A)') trim(connstr)
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


  subroutine create_sub(self)
    use parm, only: itotb
    class(db_out_type) :: self
    type(c_ptr) :: stmt
    character(512) :: fmt, sql
    integer :: ii
    write (fmt, '(A, I2, A)') &
         '("create table output_sub (SUB integer, MON integer, AREAkm2 real", ', &
         itotb, '(", ", A, " real"), ")")'
    write (sql, fmt) (sub_columns(ii), ii=1, itotb)
    write (*,*) len_trim(sql), trim(sql)
    stmt = allocate_statement(db_out)
    if (SQL_SUCCESS /= SQLExecDirect(stmt, sql, len_trim(sql))) then
       write (*,*) "Failed to create output_sub"
       call print_diag(SQL_HANDLE_STMT, stmt)
    end if
    if (SQL_SUCCESS /= SQLFreeHandle(SQL_HANDLE_STMT, stmt)) then
       write (*,*) "Failed to free statement handle"
       call print_diag(SQL_HANDLE_STMT, stmt)
    end if
  end subroutine create_sub


  subroutine create_tables(self)
    class(db_out_type) :: self
    call create_sub(self)
  end subroutine create_tables


  subroutine disconnect_db(self)
    class(db_type) :: self
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
  end subroutine disconnect_db

  subroutine disconnect_db_out(self)
    class(db_out_type) :: self
    if (c_associated(self%sub_stmt)) then
       if (SQL_SUCCESS /= SQLFreeHandle(SQL_HANDLE_STMT, self%sub_stmt)) then
          write (*,*) "Failed to free sub statement handle"
          call print_diag(SQL_HANDLE_STMT, self%sub_stmt)
       end if
    end if
    call disconnect_db(self)
  end subroutine disconnect_db_out


  subroutine print_diag(type, handle)
    integer(c_short) :: type
    type(c_ptr) :: handle
    integer(c_short) :: length
    character(SQL_MAX_MESSAGE_LENGTH, c_char) :: msg
    integer(c_int) :: native
    character(6, c_char) :: state
    integer(c_short) :: idx
    integer(c_int), target :: count
    integer(c_short) :: StringLengthPtr
    count = 0
    if (SQL_SUCCESS /= SQLGetDiagField0(type, handle, 0_2, SQL_DIAG_NUMBER, c_loc(count), 0_2, C_SHORT_NULL_PTR)) then
       write (*,*) "Failed to get the number of diagnostic messages!"
    else
       do idx=1, count+1        ! FIXME: something is fishy. memory leaks?
          if (SQL_SUCCESS == SQLGetDiagRec(type, handle, idx, &
               state, native, msg, SQL_MAX_MESSAGE_LENGTH, length)) &
               write(*,*) msg(1:length)
       end do
    end if
  end subroutine print_diag


  subroutine prepare(self)
    class(db_out_type) :: self
    call create_tables(self)
    call prepare_sub(self)
  end subroutine prepare


  subroutine prepare_sub(self)
    use parm, only: itotb, iyr, ipdvab
    class(db_out_type) :: self
    character(512, c_char) :: sql, fmt
    integer :: ii
    self%sub_stmt = allocate_statement(self)
    write (fmt, '(A, I2, A)')'("insert into ", A, " (SUB,MON,AREAkm2",', &
         itotb, '(",", A), ") values(?,?,?' // repeat(",?", itotb) // ') ")'

    write (sql, fmt) "output_sub", (sub_columns(ii), ii=1, itotb)
    write(*,*) sql

    if (SQL_SUCCESS /= SQLPrepare(self%sub_stmt, sql, len_trim(sql))) then
       write (*,*) "Failed to prepare statement"
       call print_diag(SQL_HANDLE_STMT, self%sub_stmt)
    end if
    if (SQL_SUCCESS /= SQLBindParameter(self%sub_stmt, 2_2, iyr)) then
       write (*,*) "Failed to bind iyr"
       call print_diag(SQL_HANDLE_STMT, self%sub_stmt)
    end if
  end subroutine prepare_sub

end module odbc
