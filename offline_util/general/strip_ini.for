      FUNCTION STRIP_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Used to strip STA files
C-
C-   Inputs  : RCP file and GRANNIS OBJ files for signal events
C-   Outputs : 
C-   Controls: 
C-
C-   Created  25-NOV-1991   Norman A. Graf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL STRIP_INI
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER IER
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C       read in files
        STRIP_INI = .FALSE.
        CALL INRCP('STRIP_RCP',IER)       ! read in RCP file
        IF(IER.NE.0) GOTO 999              ! failed
        IF(IER.EQ.0)
     &  CALL ERRMSG('CALORIMETER','STRIP_INI',
     &  ' Default STRIP_RCP modified','W')
        STRIP_INI = .TRUE.
      ENDIF
C
  999 RETURN
      END
