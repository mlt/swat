      subroutine subyr

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes annual subbasin output to the output.sub file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ipdvab(:)     |none          |output variable codes for output.sub file
!!    itotb         |none          |number of output variables printed (output.sub
!!    iyr           |year          |current year of simulation (eg 1980)
!!    msubo         |none          |max number of variables in output.sub file
!!    sub_km(:)     |km^2          |area of subbasin in square kilometers
!!    sub_sw(:)     |mm H2O        |water in soil profile in subbasin
!!    subgis(:)     |none          |GIS code printed to output files(output.sub)
!!    subtot        |none          |number of subbasins in watershed
!!    subyro(1,:)   |mm H2O        |precipitation in subbasin for year
!!    subyro(2,:)   |mm H2O        |snow melt in subbasin for year
!!    subyro(3,:)   |mm H2O        |surface runoff loading in subbasin for year
!!    subyro(4,:)   |mm H2O        |water yield from subbasin for year
!!    subyro(5,:)   |mm H2O        |potential evapotranspiration in subbasin for
!!                                 |year
!!    subyro(6,:)   |mm H2O        |actual evapotranspiration in subbasin for
!!                                 |year
!!    subyro(7,:)   |metric tons/ha|sediment yield from subbasin for year
!!    subyro(8,:)   |kg N/ha       |organic N loading from subbasin for year
!!    subyro(9,:)   |kg P/ha       |organic P loading from subbasin for year
!!    subyro(10,:)  |kg N/ha       |NO3 loading from surface runoff in subbasin
!!                                 |for year
!!    subyro(11,:)  |kg P/ha       |soluble P loading from subbasin for year
!!    subyro(12,:)  |mm H2O        |groundwater loading from subbasin for year
!!    subyro(13,:)  |mm H2O        |percolation out of soil profile in subbasin
!!                                 |for year
!!    subyro(14,:)  |kg P/ha       |loading to reach of mineral P attached to
!!                                 |sediment from subbasin for year
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    pdvab(:)    |varies        |array to hold subbasin output values
!!    pdvb(:)     |varies        |array of user selected subbasin output
!!                               |values
!!    sb          |none          |subbasin number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      use odbc

      integer :: sb, ii
      real, dimension (msubo) :: pdvab, pdvb
      type(c_ptr) :: stmt

      stmt = allocate_statement(db_out)

      if (SQL_SUCCESS /= SQLPrepare(stmt, C_CHAR_"insert into output_sub
c$$$     $ (SUB, GIS, MON)
c$$$     $ values(?,?,?)
     $ values(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
     $ " // C_NULL_CHAR, SQL_NTS)) then
         write (*,*) "Failed to prepare statement"
         call print_diag(SQL_HANDLE_STMT, stmt)
      end if

      do sb = 1, subtot

        pdvab = 0.
        pdvb = 0.
  
        pdvab(1) = subyro(1,sb)
        pdvab(2) = subyro(2,sb)
        pdvab(3) = subyro(5,sb)
        pdvab(4) = subyro(6,sb)
        pdvab(5) = sub_sw(sb)
        pdvab(6) = subyro(13,sb)
        pdvab(7) = subyro(3,sb)
        pdvab(8) = subyro(12,sb)
        pdvab(9) = subyro(4,sb)
        pdvab(10) = subyro(7,sb)
        pdvab(11) = subyro(8,sb)
        pdvab(12) = subyro(9,sb)
        pdvab(13) = subyro(10,sb)
        pdvab(14) = subyro(11,sb)
        pdvab(15) = subyro(14,sb)
        pdvab(16) = subyro(15,sb)
        pdvab(17) = subyro(16,sb)
        pdvab(18) = subyro(17,sb)
!!      chl_a, cbodu and doxq were all written out in daily
!!      output code.  not set up for monthly/yearly
!!      all values will be zero for all codes except daily
!!      added for jennifer b.
        pdvab(19) = 0.0
        pdvab(20) = 0.0
        pdvab(21) = 0.0
        pdvab(22) = subyro(18,sb)    !!tile_no3

        if (ipdvab(1) > 0) then
          do ii = 1, itotb
            pdvb(ii) = pdvab(ipdvab(ii))
          end do
          if (SQL_SUCCESS /= SQLBindParameter(stmt,1_2, SQL_PARAM_INPUT,
     &         SQL_INTEGER, 0, 0_2, sb)) then
             write (*,*) "Failed to bind sb"
             call print_diag(SQL_HANDLE_STMT, stmt)
          end if
          if (SQL_SUCCESS /= SQLBindParameter(stmt,2_2, SQL_PARAM_INPUT,&
     &         SQL_INTEGER, 0, 0_2, subgis(sb)))
     &         write (*,*) "Failed to bind subgis"
          if (SQL_SUCCESS /= SQLBindParameter(stmt,3_2, SQL_PARAM_INPUT,&
     &         SQL_REAL, 0, 0_2, iyr)) then
             write (*,*) "Failed to bind iyr"
             call print_diag(SQL_HANDLE_STMT, stmt)
          end if
          if (SQL_SUCCESS /= SQLBindParameter(stmt,4_2, SQL_PARAM_INPUT,&
     &         SQL_REAL, 0, 0_2, sub_km(sb)))
     &         write (*,*) "Failed to bind sub_km"
          do ii = 1, itotb
             if (SQL_SUCCESS /= SQLBindParameter(stmt,
     &      int(ii+4, 2), SQL_PARAM_INPUT, SQL_REAL, 0, 0_2, pdvb(ii))) &
     &            write (*,*) "Failed to bind rest"
          end do
          if (SQL_SUCCESS /= SQLExecute(stmt)) then
             write (*,*) "Failed to execute statement"
             call print_diag(SQL_HANDLE_STMT, stmt)
          end if
          write (31,1000) sb, subgis(sb), iyr, sub_km(sb),              &
     &                                         (pdvb(ii), ii = 1, itotb)
        else
          write (31,1000) sb, subgis(sb), iyr, sub_km(sb),              &
     &                                        (pdvab(ii), ii = 1, msubo)
        end if
      end do

      if (SQL_SUCCESS /= SQLFreeHandle(SQL_HANDLE_STMT, stmt)) then
         write (*,*) "Failed to free statement handle"
         call print_diag(SQL_HANDLE_STMT, stmt)
      end if

      return
!1000 format ('BIGSUB',i4,1x,i8,1x,i4,e10.5,21f10.3)
 1000 format ('BIGSUB',i4,1x,i8,1x,i4,e10.5,18f10.3,1x,e10.5,3e10.3)
      end 
