      FUNCTION PJTINI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialization routine for PJET
C-
C-   Returned value  : TRUE if all OK
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-MAR-1990   Boaz Klima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PJTINI
C
      INTEGER IER
      LOGICAL FIRST,OK
      SAVE FIRST,OK
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST =.FALSE.
C
        CALL INRCP('PJET_RCP',IER)      ! Read in RCP file
        OK = IER .EQ. 0
        IF ( .NOT. OK ) GOTO 999        ! Failed
C
        CALL INRCPE('PJET_RCPE',IER)    ! Read overwrite file (RCPE)
        IF( IER .EQ. 0 )
     &  CALL ERRMSG('PJET','PJTINI',
     &  ' Default PJET_RCP modified','W')
      ENDIF
C
  999 CONTINUE
      PJTINI = OK
      RETURN
      END


