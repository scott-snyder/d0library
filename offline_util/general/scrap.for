      SUBROUTINE SCRAP(MSG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Deliberately abort program by calling ABORT
C-
C-   Inputs  :
C-   MSG= some illuminating message before abortion
C-
C-   Created  14-AUG-1987   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) MSG
C&IF VAXVMS,ULTRIX,SIUNIX
      CALL ABORT(MSG)
C&ENDIF
C&IF IBMAIX
C&      WRITE (6,'(5X,A)') MSG
C&      CALL ABORT
C&ENDIF
      END
