      SUBROUTINE CCLANL2
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Analyze H matrix quantities
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Updated   6-JUL-1992   Rajendran Raja   .DUMMY FOR NEW HMATRIX
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL ERRMSG('CALORIMETER','CCLANL2',
     &    ' ANALYSIS NOT IMPLEMENTED YET ','W')
        
      ENDIF
C
  999 RETURN
      END
