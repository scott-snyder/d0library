      integer function ez_array_length (param, callers_name)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-     Return the length (number of items) of the SRCP array PARAM.
C-     If the array does not exist, signal an error and return 0.
C-
C-     You'd think this would be easy, wouldn't you?
C-     Well, you'd be wrong!
C-
C-   Returned value  :
C-     Length of the array, or 0 if there was a problem.
C-
C-   Inputs  :
C-     param : Name of the RCP arary parameter.
C-     callers_name : Name of the calling routine, for error reporting.
C-
C-   Created   2-SEP-1994   scott snyder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      include 'd0$offline_Util$general:value.def'

      character*(*) param, callers_name

      integer id, ier, len, i, arylen, ival, type, dum1, dum2
      character*80 str
C----------------------------------------------------------------------
c
c *** Look up the ID of the parameter.
c
      ez_array_length = 0
      call upcase (param, str)
      call ezgeti (str, id, ier)
      if (ier .ne. 0) then
        call ezget_error_text (ier, str)
        call errmsg (str, callers_name, param, 'e')
        return
      endif
c
c *** And get the total length of the parameter.
c
      call ezget1 (id, 0, 0, 0, len, ier)
      if (ier .ne. 0) then
        call ezget_error_text (ier, str)
        call errmsg (str, callers_name, param, 'e')
        return
      endif
c
c *** Test for the easy case.
c
      if (len .eq. 1) then
        ez_array_length = 1
        return
      endif
c
c *** Now that we have the length, we must step through and _count_
c *** how many parameters there are.  I still find it hard to believe
c *** that we have to go through this rigamarole...
c
      i = 1
      arylen = 0
      do while (i .le. len)
        arylen = arylen + 1

c ***   bump i by the length of this item
        call ezget2 (id, i, i, 1, ival, type, dum1, dum2, ier)
        if (ier .ne. 0) then
          call ezget_error_text (ier, str)
          call errmsg (str, callers_name, param, 'f')
        endif

c ***   It'll be a cold day in hell when any of the zebra stuff
c ***   deals with non-32-bit integers...
        if (type .gt. VTCHR) then
          i =  i + int ((type - VTCHR + 3) / 4)
        else
          i = i + 1
        endif
      enddo

      ez_array_length = arylen
  999 RETURN
      END
