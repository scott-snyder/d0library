      FUNCTION LV0INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize the Level 0 reconstruction
C-
C-   Returned value  : TRUE if RCP file read in
C-
C-   Created   2-JUN-1992   Jeffrey Bantly
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER IER,LRCP
C
      CHARACTER*(*) RCPFIL
      PARAMETER( RCPFIL = 'LEVEL0_RCP' )   ! Logical name of control file
C
      LOGICAL LV0INI
      LOGICAL OK, FIRST
C
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      LV0INI = .TRUE.
      IF (.NOT.FIRST) RETURN
      FIRST = .FALSE.
C
C  Read in Level 0 RCP parameter file.
C
      CALL EZLOC(RCPFIL,LRCP)
      IF(LRCP.LE.0) THEN
        CALL INRCP (RCPFIL,IER)  ! Read parameter file into an SRCP bank
        IF (IER .NE. 0) THEN
          CALL ERRMSG('LEVEL0-no-rcp','LV0INI',
     &    'LEVEL RCP had a bad read','W')
          LV0INI = .FALSE.
          GO TO 999
        END IF
      END IF
C
      CALL FLGSET('LEVEL0_RCP',.TRUE.)
      CALL FLGERR(IER)
      IF ( IER.NE.0 ) THEN
        CALL FLGBK('LEVEL0_RCP',1)
        CALL FLGSET('LEVEL0_RCP',.TRUE.)
      ENDIF
C
C  Get list of banks to dump
C
      CALL LV0DDF
C
C  Get list of banks to drop
C
      CALL LV0DRP(IER)
C
C  Initialize Geometry (read STP file, etc.)
C
C      OK = LV0_GEOM_INIT()
C
C      LV0INI = OK
C----------------------------------------------------------------------
  999 RETURN
      END
