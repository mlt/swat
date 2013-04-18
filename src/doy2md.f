      subroutine doy2md()
      use iso_c_binding
      use parm, only: iida, leapyr, i_mo, i_cl
      integer :: k
      ! Nothing can be faster than unconditional lookup from static tables
      integer(c_short),dimension(366,0:1),parameter :: days = reshape((/&
     &     (k, k=1, 31), (k, k=1, 29), (k, k=1, 31), (k, k=1, 30),      &
     &     (k, k=1, 31), (k, k=1, 30), (k, k=1, 31), (k, k=1, 31),      &
     &     (k, k=1, 30), (k, k=1, 31), (k, k=1, 30), (k, k=1, 31)       &
     &     ,                                                            &
     &     (k, k=1, 31), (k, k=1, 28), (k, k=1, 31), (k, k=1, 30),      &
     &     (k, k=1, 31), (k, k=1, 30), (k, k=1, 31), (k, k=1, 31),      &
     &     (k, k=1, 30), (k, k=1, 31), (k, k=1, 30), (k, k=1, 31), -1   &
     &     /), shape(days))
      integer(c_short),dimension(366,0:1),parameter :: months=reshape((/&
     &     ( 1, k=1, 31), ( 2, k=1, 29), ( 3, k=1, 31), ( 4, k=1, 30),  &
     &     ( 5, k=1, 31), ( 6, k=1, 30), ( 7, k=1, 31), ( 8, k=1, 31),  &
     &     ( 9, k=1, 30), (10, k=1, 31), (11, k=1, 30), (12, k=1, 31)   &
     &     ,                                                            &
     &     ( 1, k=1, 31), ( 2, k=1, 28), ( 3, k=1, 31), ( 4, k=1, 30),  &
     &     ( 5, k=1, 31), ( 6, k=1, 30), ( 7, k=1, 31), ( 8, k=1, 31),  &
     &     ( 9, k=1, 30), (10, k=1, 31), (11, k=1, 30), (12, k=1, 31),-1&
     &     /), shape(months))
      i_mo = months(mod(iida-1, 366-leapyr)+1, leapyr)
      i_cl = days(mod(iida-1, 366-leapyr)+1, leapyr)
      end subroutine doy2md
