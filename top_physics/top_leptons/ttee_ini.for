      FUNCTION TTEE_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-                         
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   2-OCT-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL TTEE_INI
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
      INTEGER IER
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST=.FALSE.
        TTEE_INI=.FALSE.
        CALL INRCP('TTEE_RCP',IER)       ! read in RCP file
        IF(IER.NE.0) GOTO 999            ! failed
        CALL EZPICK('TTEE_RCP')          ! select TTEE bank
        CALL EZERR(IER)
        IF(IER.EQ.0) THEN
          TTEE_INI= .TRUE.
        ELSE
          CALL ERRMSG('TTEE','TTEE_INI',
     &      ' TTEE_RCP file does not have a TTEE bank.','W')
          TTEE_INI = .FALSE.
        ENDIF
        CALL EZRSET
      ELSE
C
C ****  Not first entry into TTEE_INI - set TTEE_INI .TRUE.
C
        TTEE_INI=.TRUE.
      ENDIF
  999 RETURN
      END
