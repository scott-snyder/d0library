      SUBROUTINE D0_ABORT(MSG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Deliberately abort program
C-
C-   Inputs  :
C-   MSG= some illuminating message before abortion
C-
C-   Created  14-AUG-1987   Serban D. Protopopescu
C-   Updated   2-AUG-1988   Olivier Callot  : Remove the LEAVE_UNSATISFIED
C-   Updated  12-Feb-1992   Herbert Greenlee
C-      UNIX compatible version
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C&IF VAXVMS
      INCLUDE '($SSDEF)'
C&ENDIF
      CHARACTER*(*) MSG
C----------------------------------------------------------------------
C
      CALL UQUIT        ! user hook for cleanup before exit
      CALL INTMSG(MSG)
C
C ****  This is an ENTRY to avoid Unsatisfied External Message at link time.
C
      ENTRY QNEXT
C
C&IF VAXVMS
      CALL LIB$SIGNAL(%VAL(SS$_ACCVIO) )        
C
C ****   Aborts with access violation and traceback. Allows the debugger to
C ****   access the relevant information. Same effect as having an unsatisfied
C ****   external . See VAX/VMS Run-Time Library manual
C
C&ELSE
C&      CALL ABORT
C&      STOP
C&ENDIF
      END
