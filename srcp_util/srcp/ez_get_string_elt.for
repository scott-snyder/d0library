      subroutine ez_get_string_elt (param, index_in,
     &                              val, vallen, callers_name)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Retrieve the value of PARAM from an array in
C-     the currently selected RCP
C-     bank as a string.  CALLERS_NAME is the name of the calling
C-     routine, for error reporting.
C-
C-   Inputs  : 
C-     param : Name of the RCP parameter to retrieve.
C-     index : The array index of the value to retrieve.
C-             If 0, treated as unspecified (defaults to 1).
C-     callers_name : Name of calling routine, for error reporting.
C-
C-   Outputs :
C-     val : The retrieved string.
C-     vallen : Its length.
C-
C-   Controls: 
C-   Bugs :
C-     I'm not sure that mixed arrays are handled properly in all cases.
C-
C-   Created  21-FEB-1994   scott snyder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      include 'd0$offline_Util$general:value.def'

      character*(*) param, val, callers_name
      integer vallen, index_in

      integer index, id, ier, type, beg, end, ary_size
      integer ival
      logical lval
      real    rval
      character*128 errtext, param_name, sbuf
C----------------------------------------------------------------------

      index = index_in
      call ez_get_helper (param, callers_name,
     &                    param_name, id, index, type, ary_size)
c
c *** Retrieve the value of the appropriate type, and cast it to a string.
c
      if (type .eq. VTINT .or. type .eq. VTHEX) then
        call ezget1_i (id, index, index, 1, ival, ier)
        write (sbuf, '(i12)') ival
        call word (sbuf, beg, end, vallen)
        val = sbuf(beg:end)

      else if (type .eq. VTREAL .or. type .eq. VTREFM .or.
     &         type .eq. VTDBL) then
        call ezget1 (id, index, index, 1, rval, ier)
        write (sbuf, '(g12.4)') rval
        call word (sbuf, beg, end, vallen)
        val = sbuf(beg:end)

      else if (type .eq. VTLOG) then
        call ezget1_l (id, index, index, 1, lval, ier)
        write (sbuf, '(l12)') lval
        call word (sbuf, beg, end, vallen)
        val = sbuf(beg:end)

      else if (type .eq. VTCHAR .or. type .ge. VTCHR) then
        call ezgets (param_name, index, val, vallen, ier)

      else
        call errmsg ('funny type for parameter', callers_name,
     &               param, 'f')
      endif
c
c *** Signal an error if there was a problem with the ezget.
c
      if (ier .ne. 0) then
        call ezget_error_text (ier, errtext)
        call errmsg (errtext, callers_name, param, 'f')
      endif

  999 RETURN
      END
