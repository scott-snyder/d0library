      integer function ez_get_logical_array (param, ary, n_max,
     &                                       callers_name)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-     Fetch an array of logical values from SRCP.
C-
C-   Returned value  :
C-     The number of values read.
C-
C-   Inputs  :
C-     param : The name of the RCP parmeter from which to read.
C-     n_max : The maximum number of values to read (the dimension
C-             of ARY).  If the length of the array exceeds this value,
C-             an error will be generated and only the first
C-             N_MAX values will be returned.
C-     callers_name : Name of the calling procedure, for error
C-                    reporting.
C-
C-   Outputs :
C-     ary   : Array in which to return data.
C-
C-   Controls:
C-     The bank containing PARAM must have been selected with ezpick.
C-
C-   Created   1-JAN-1995   scott snyder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      character*(*) param, callers_name
      integer n_max
      logical ary(n_max)

      integer arylen, i

      logical  ez_get_logical_elt
      integer  ez_array_length
      external ez_array_length, ez_get_logical_elt
C----------------------------------------------------------------------

      arylen = ez_array_length (param, callers_name)
      if (arylen .gt. n_max) then
        call errmsg ('array is too long', callers_name, param, 'E')
        arylen = n_max
      endif

      do i=1, arylen
        ary(i) = ez_get_logical_elt (param, i, callers_name)
      enddo

      ez_get_logical_array = arylen
  999 RETURN
      END
