      FUNCTION LJTOP_HMATRIX_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize Top into Lepton + jets
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
      LOGICAL LJTOP_HMATRIX_INI
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
      INTEGER IER
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST=.FALSE.
        LJTOP_HMATRIX_INI=.FALSE.
C
C       read in files
        CALL INRCP('LJTOP_HMATRIX_RCP',IER)       ! read in RCP file
        IF(IER.NE.0) GOTO 999              ! failed
C
        CALL INRCPE('LJTOP_HMATRIX_RCPE',IER)     ! read overwrite file (RCPE)
        IF(IER.EQ.0)
     &  CALL ERRMSG('LJTOP_HMATRIX','LJTOP_HMATRIX_INI',
     &  ' Default LJTOP_HMATRIX_RCP modified','W')
C
C DECLARE LINK AREA
C
        CALL EZPICK('LJTOP_HMATRIX_RCP')              ! select LJTOP_HMATRIX bank
        CALL EZERR(IER)
        IF(IER.EQ.0) THEN
          LJTOP_HMATRIX_INI= .TRUE.
        ELSE
          CALL ERRMSG('LJTOP_HMATRIX',
     &      'LJTOP_HMATRIX_INI',
     &      ' LJTOP_HMATRIX_RCP has no a LJTOP_HMATRIX bank',
     &      'W')
          LJTOP_HMATRIX_INI = .FALSE.
        ENDIF
C
        CALL EZRSET
      ELSE
C
C ****  Not first entry into LJTOP_HMATRIX_INI - set LJTOP_HMATRIX_INI .TRUE.
C
        LJTOP_HMATRIX_INI=.TRUE.
      ENDIF
  999 RETURN
      END
