      real function ez_get_float_elt (param, index_in, callers_name)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Retrieve the value of PARAM from an array in
C-     the currently selected RCP
C-     bank as an float  CALLERS_NAME is the name of the calling
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
      integer ii, jj
      real rval
      logical lval
      character*128 sval, param_name

      real     value
      external value
C----------------------------------------------------------------------

      index = index_in
      call ez_get_helper (param, callers_name,
     &                    param_name, id, index, type, ary_size)
c
c *** Retrieve the value of the appropriate type, and cast it to int.
c
      if (type .eq. VTINT .or. type .eq. VTHEX) then
        call ezget1 (id, index, index, 1, ival, ier)
        rval = real (ival)

      else if (type .eq. VTREAL .or. type .eq. VTREFM .or.
     &         type .eq. VTDBL) then
        call ezget1 (id, index, index, 1, rval, ier)

      else if (type .eq. VTLOG) then
        call ezget1 (id, index, index, 1, lval, ier)
        if (lval) then
          rval = 1
        else
          rval = 0
        endif

      else if (type .eq. VTCHAR .or. type .ge. VTCHR) then
        call ezgets (param_name, index, sval, slen, ier)
        if (ier .eq. 0) then
          rval = value (sval(1:slen), ii, jj, type)
          if (type .le. 0) then
            call errmsg ('invalid string for float', callers_name,
     &                   param, 'e')
            rval = 0
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

      ez_get_float_elt = rval

  999 RETURN
      END
