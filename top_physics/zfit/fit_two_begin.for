      LOGICAL FUNCTION FIT_TWO_BEGIN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Initialise Fit_two package
C-
C-   Created  11-FEB-1993   Pushpa C. Bhat
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER
C----------------------------------------------------------------------
      FIT_TWO_BEGIN=.TRUE.
      CALL INRCP('FIT_TWO_RCP',IER)
      IF(IER .NE. 0)THEN
        CALL ERRMSG('FIT_TWO','FIT_TWO_BEGIN',
     &    'Cannot find file','FIT_TWO_RCP','F')
      ENDIF
      CALL OPEN_NTUPLE('FIT_TWO_RCP',IER)
      IF(IER .NE. 0)THEN
        CALL ERRMSG('FIT_TWO','FIT_TWO_BEGIN',
     &    'Cannot book Ntuple','OPEN_NTUPLE','F')
      ENDIF
C      CALL LSQ_INI
C
  999 RETURN
      END
