      SUBROUTINE D0CSOPEN_HOOK(LUN,FILNAM,CHOPT,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine acts as a front end for D0CSOPEN.
C-                         It either does nothing, returning OK=.FALSE.
C-                         or simply passes the call do D0CSOPEN.  The 
C-                         former case is to avoid linking in CSPACK 
C-                         routines.  In either case, the calling sequence 
C-                         is the same as D0CSOPEN (and D0OPEN).
C-
C-   Inputs  :
C-   LUN   = unit number
C-   FILNAM= file name
C-   CHOPT = character options
C-   Outputs :
C-   OK    = set to false if there is a problem opening file
C-           true otherwise
C-
C-   Created 21-Dec-1993   Herbert B. Greenlee
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      CHARACTER*(*) FILNAM,CHOPT
      INTEGER LUN
      LOGICAL OK
C----------------------------------------------------------------------
C&IF VAXVMS
      OK = .FALSE.
C&ELSE
C&      CALL D0CSOPEN(LUN, FILNAM, CHOPT, OK)
C&ENDIF
      GO TO 999

      ENTRY D0CSCLOSE_HOOK(LUN, CHOPT, OK)
C&IF VAXVMS
      OK = .FALSE.
C&ELSE
C&      CALL D0CSCLOSE(LUN, CHOPT, OK)
C&ENDIF
  999 RETURN
      END
