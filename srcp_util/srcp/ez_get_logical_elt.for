      logical function ez_get_logical_elt (param, index_in,
     &                                     callers_name)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Retrieve the value of PARAM from an array in
C-     the currently selected RCP
C-     bank as an logical  CALLERS_NAME is the name of the calling
C-     routine, for error reporting.
C-
C-   Returned value  :
C-     Value of the parameter, as an integer.
C-
C-   Inputs  :
C-     param : Name of the RCP parameter to retrieve.
C-     index : The array index of the value to retrieve.
C-             If 0, treated as unspecified (defaults to 1).
C-     callers_name : Name of calling routine, for error reporting.
C-
C-   Outputs : 
C-   Controls:
C-   Bugs :
C-     I'm not sure that mixed arrays are handled properly in all cases.
C-
C-   Created  21-FEB-1994   scott snyder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      include 'd0$offline_Util$general:value.def'

      character*(*) param, callers_name
      integer index_in

      integer ival, ier, id, type, slen, index, ary_size
      integer i
      real rval
      logical lval
      character*128 sval, param_name
      character c
C----------------------------------------------------------------------

      index = index_in
      call ez_get_helper (param, callers_name,
     &                    param_name, id, index, type, ary_size)
c
c *** Retrieve the value of the appropriate type, and cast it to int.
c
      if (type .eq. VTINT .or. type .eq. VTHEX) then
        call ezget1_i (id, index, index, 1, ival, ier)
        lval = (ival .ne. 0)

      else if (type .eq. VTREAL .or. type .eq. VTREFM .or.
     &         type .eq. VTDBL) then
        call ezget1 (id, index, index, 1, rval, ier)
        lval = (rval .ne. 0)

      else if (type .eq. VTLOG) then
        call ezget1_l (id, index, index, 1, lval, ier)

      else if (type .eq. VTCHAR .or. type .ge. VTCHR) then
        call ezgets (param_name, index, sval, slen, ier)

        if (ier .eq. 0) then
          i = 1
          do while (i .le. slen .and.
     &              (sval(i:i) .eq. ' ' .or. sval(i:i) .eq. '.'))
            i = i + 1
          enddo
          lval = .false.
          if (i .le. slen) then
            c = sval(i:i)
            if (c .eq. 'y' .or. c .eq. 'Y' .or.
     &          c .eq. 't' .or. c .eq. 'T' .or.
     &          (c .ge. '1' .and. c .le. '9') .or.
     &          (c .eq. '-' .and. i .lt. slen .and.
     &           sval(i+1:i+1) .eq. '1')) then
              lval = .true.
            endif
          endif
        endif

      else
        call errmsg ('funny type for parameter', callers_name,
     &               param, 'f')
      endif
c
c *** Signal an error if there was a problem with the ezget.
c
      if (ier .ne. 0) then
        call ezget_error_text (ier, sval)
        call errmsg (sval, callers_name, param, 'f')
      endif

      ez_get_logical_elt = lval

  999 RETURN
      END
