      subroutine ezpick_and_signal (rcp_name, callers_name)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Call ezpick for RCP_NAME.
C-     If it fails, try to read in the RCP file of the same name.
C-     If _that_ fails, signal a fatal error.
C-     CALLERS_NAME will be used as the routine name
C-     in the error message.
C-
C-   Inputs  :
C-     rcp_name : Name of RCP bank to attempt to pick.
C-     callers_name : Name of calling routine, for error reporting.
C-
C-   Outputs : 
C-   Controls: 
C-
C-   Created  21-FEB-1994   scott snyder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      character*(*) rcp_name, callers_name

      integer ier
C----------------------------------------------------------------------

      call ezpick_errors (0)
      call ezpick (rcp_name)
      call ezerr (ier)
      call ezpick_errors (-1)
      if (ier .ne. 0) then
c ***   maybe it hasn't been read yet.
        call ez_read_rcp (rcp_name, callers_name)

        call ezpick (rcp_name)
        call ezerr (ier)
        if (ier .ne. 0) then
          call errmsg ('ezpick fails', callers_name, rcp_name, 'f')
        endif
      endif

  999 RETURN
      END
