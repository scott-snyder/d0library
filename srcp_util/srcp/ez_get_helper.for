      subroutine ez_get_helper (param, callers_name,
     &                          param_name, id, index, type,
     &                          ary_size)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Do preliminary processing for ez_get_* routines.
C-
C-   Inputs  :
C-     param : Name of parameter to retrive.
C-     callers_name : Name of calling routine, for error reporting.
C-     index : The array index of the value to retrieve.
C-             This is the _item number_, not the SRCP array index.
C-             If 0, treated as unspecified (defaults to 1, but can
C-             be overridden by a subscript on the parameter name).
C-
C-   Outputs :
C-     param_name: Name given as output by ezzdcd.
C-     id : Parameter id value.
C-     index : The actual index into the SRCP array for non-string
C-             parameters; otherwise, the ordinal numbe of the string.
C-     type : Parameter type code.
C-     ary_size : Size of array.
C-
C-   Controls: 
C-
C-   Created  21-FEB-1994   scott snyder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      include 'd0$offline_Util$general:value.def'

      character*(*) param, callers_name
      character*(*) param_name
      integer id, index, type, ary_size

      integer slen, hi_index, step, ier, ival, index1
      integer string_count, item_count, ary_pos
      character*128 errtext
      character*32 name
C----------------------------------------------------------------------
c
c *** Parse the name.
c
      call ezzdcd (param, param_name, slen, index1, hi_index, step)
      if (hi_index .eq. 0) hi_index = index1
      if (index1 .ne. hi_index .or. index1 .le. 0 .or. step .gt. 1) then
        call errmsg ('bad array index', callers_name, param, 'f')
      endif
c
c *** Look up the index of the parameter.
c
      call ezgeti (param_name, id, ier)
      if (ier .ne. 0) then
        call ezget_error_text (ier, errtext)
        call errmsg (errtext, callers_name, param, 'f')
      endif
c
c *** Check any supplied index.
c
      if (index .eq. 0) then
        index = index1
      else
        if (index1 .ne. 1 .and. index1 .ne. index) then
          call errmsg ('index conflict', callers_name, param, 'e')
        endif
      endif
c
c *** Now we gotta find where this sucker is located in the array.
c *** This is complicated greatly by the fact that strings take up
c *** multiple array slots...
c
c *** Get the total length of the array and the type of the first parm.
c
      call ezgett (id, name, slen, type, ary_size)
c
c *** First, the simple case where index == 1.
c
      if (index .eq. 1) then
        return
      endif
c
c *** Otherwise, must search the array.
c
      ary_pos = 1
      item_count = 0
      string_count = 0

      do while (ary_pos .le. ary_size)
c
c ***   Get the type.
c
        call ezget2 (id, ary_pos, ary_pos, 1,
     &               ival, type, hi_index, step, ier)
        if (ier .ne. 0) then
          call ezget_error_text (ier, errtext)
          call errmsg (errtext, callers_name, param, 'f')
        endif
c
c ***   Bump counters.
c
        item_count = item_count + 1
        if (type .ge. VTCHR) string_count = string_count + 1
c
c ***   Stop if this is the item we're looking for.
c
        if (index .eq. item_count) then
c
c ***     Set index to this position if this isn't a string;
c ***     otherwise, set it to the ordinal number of this string.
c
          if (type .ge. VTCHR) then
            index = string_count
          else
            index = ary_pos
          endif

          return
        endif
c
c ***   Skip over this element.
c
        if (type .gt. VTCHR) then
          ary_pos =  ary_pos + int ((type - VTCHR + 3) / 4)
        else
          ary_pos = ary_pos + 1
        endif
      enddo
c
c *** Complain if we fall through to here.
c
      call errmsg ('array index out of range', callers_name,
     &             param, 'f')
      index = 1

  999 RETURN
      END
