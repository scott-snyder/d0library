      integer function lib$date_time(date_time)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Emulate the VMS rtl routine.
C-
C-   Returned value  : 1 (true)
C-   Inputs  :
C-   Outputs : date_time (character) - Local date and time. 
C-   Controls: 
C-
C-   Created   29-Feb-1992   Herbert Greenlee
C-
C----------------------------------------------------------------------
      implicit none
      character*(*) date_time
      character*20 time_string
      integer local_time(2)
      integer*2 time_len
      logical ok
      integer sys$gettim, sys$asctim
C---------------------------------------------------------------------
      ok = sys$gettim(local_time)
      if(ok)ok = sys$asctim(time_len, time_string, local_time)
      date_time = time_string
      lib$date_time = ok
  999 return
      end
