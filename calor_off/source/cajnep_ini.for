      FUNCTION CAJNEP_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  19-NOV-1991   Dhiman Chakraborty
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CAJNEP_INI
      LOGICAL FIRST,OK
      SAVE FIRST,OK
      DATA FIRST/.TRUE./
      INTEGER IER
C----------------------------------------------------------------------
      CAJNEP_INI = .TRUE.
      IF ( FIRST ) THEN
        FIRST=.FALSE.
C
C ****  Read in RCP file for this package
C
        CALL INRCP('CAJNEP_RCP',IER)       ! read in RCP file
        OK = IER.EQ.0
        CAJNEP_INI = OK
        IF( .NOT.OK) GOTO 999              ! failed
C
        CALL INRCPE('CAJNEP_RCPE',IER)     ! read overwrite file (RCPE)
        IF(IER.EQ.0)
     &    CALL ERRMSG('CAJNEP','CAJNEP_INI',
     &    ' Default CAJNEP_RCP modified','W')
        ENDIF
C
  999 RETURN
      END
