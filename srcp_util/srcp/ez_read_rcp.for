      subroutine ez_read_rcp (rcp_name, callers_name)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-     Read in the RCP file RCP_NAME; signal a fatal error if there's
C-     a problem reading it.
C-
C-     Then look for a file with the same name with an `e' tacked
C-     on the end; if it exists, read it as an overwrite file.
C-
C-   Inputs  :
C-     rcp_name : Name of the rcp bank to read.
C-     callers_name : Name of the calling routine, for error reporting.
C-
C-   Created  23-FEB-1994   scott snyder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      character*(*) rcp_name, callers_name

      integer ier, ios
      logical file_exists
      character*132 errtext, rcpe_name

      integer  trulen
      external trulen
C----------------------------------------------------------------------
      call inrcp (rcp_name, ier)
      if (ier .ne. 0) then
        call ezget_error_text (ier, errtext)
        call errmsg (errtext, callers_name, rcp_name, 'f')
      endif

      rcpe_name = rcp_name(1:trulen (rcp_name)) // 'E'
C&IF VAXVMS
      inquire (file = rcpe_name, exist = file_exists, iostat = ios)
      if (ios .eq. 0 .and. file_exists) then
C&ELSE
C&      call lib$find_file (rcpe_name, errtext, ios)
C&      call lib$find_file_end (ios)
C&      if (errtext .ne. ' ') then
C&ENDIF
        call inrcpe (rcpe_name, ier)
        if (ier .eq. 0) then
          errtext = 'default ' // rcp_name // ' modified'
          call errmsg (errtext, callers_name, rcp_name, 'w')
        else
          call ezget_error_text (ier, errtext)
          call errmsg (errtext, callers_name, rcp_name, 'e')
        endif
      endif

  999 RETURN
      END
