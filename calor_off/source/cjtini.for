      FUNCTION CJTINI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialization routine for Jets
C-
C-   Returned value  : TRUE if all OK
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  25-APR-1989   Rajendran Raja
C-   Updated   2-OCT-1989   Gerald C. Blazey, Harrison B. Prosper
C-   Moved calls to CONCLI and CLUPRI to CJET_CONE
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CJTINI
C
      INTEGER IER
      LOGICAL FIRST,OK
      SAVE FIRST,OK
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST =.FALSE.
C
        CALL INRCP('CAJETS_RCP',IER)    ! Read in RCP file
        OK = IER .EQ. 0
        IF ( .NOT. OK ) GOTO 999        ! Failed
C
        CALL INRCPE('CAJETS_RCPE',IER)  ! Read overwrite file (RCPE)
        IF( IER .EQ. 0 )
     &  CALL ERRMSG('CAJETS','CJTINI',
     &  ' Default CAJETS_RCP modified','W')
      ENDIF
C
  999 CONTINUE
      CJTINI = OK
      RETURN
      END
