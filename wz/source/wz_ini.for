      FUNCTION WZ_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize intermediate vector boson (WZ)
C-                         analysis package
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   2-OCT-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL WZ_INI
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
      INTEGER IER
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST=.FALSE.
        WZ_INI=.FALSE.
C
C       read in files
        CALL INRCP('WZ_RCP',IER)       ! read in RCP file
        IF(IER.NE.0) GOTO 999              ! failed
C
        CALL INRCPE('WZ_RCPE',IER)     ! read overwrite file (RCPE)
        IF(IER.EQ.0)
     &  CALL ERRMSG('WZ','WZ_INI',
     &  ' Default WZ_RCP modified','W')
C
C DECLARE LINK AREA
C
        CALL EZPICK('WZ_RCP')              ! select WZ bank
        CALL EZERR(IER)
        IF(IER.EQ.0) THEN
          WZ_INI= .TRUE.
        ELSE
          CALL ERRMSG('WZ','WZ_INI',
     &      ' WZ_RCP file does not have a WZ bank.','W')
          WZ_INI = .FALSE.
        ENDIF
C
        CALL EZRSET
      ELSE
C
C ****  Not first entry into WZ_INI - set WZ_INI .TRUE.
C
        WZ_INI=.TRUE.
      ENDIF
  999 RETURN
      END
