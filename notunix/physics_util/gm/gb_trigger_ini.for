      FUNCTION GB_TRIGGER_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Global Monitor Trigger initialization
C-
C-   Returned value  : TRUE if GB_TRIGGER_RCP read in successfully
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  19-NOV-1992   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL GB_TRIGGER_INI
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER IER,LRCP
C
      CHARACTER*(*) RCPFIL
      PARAMETER( RCPFIL = 'GB_TRIGGER_RCP' )   ! Log name of control file
C
      LOGICAL OK, FIRST
C
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      GB_TRIGGER_INI = .TRUE.
      IF (.NOT.FIRST) RETURN
      FIRST = .FALSE.
C
C  Read in RCP parameter file.
C
      CALL EZLOC(RCPFIL,LRCP)
      IF(LRCP.LE.0) THEN
        CALL INRCP (RCPFIL,IER)  ! Read parameter file into an SRCP bank
        IF (IER .NE. 0) THEN
          CALL ERRMSG('GB_TRIGGER-no-rcp','GB_TRIGGER_INI',
     &    'GB TRIGGER RCP had a bad read','W')
          GB_TRIGGER_INI = .FALSE.
          GO TO 999
        END IF
      END IF
C
      CALL FLGSET('GB_TRIGGER_RCP',.TRUE.)
      CALL FLGERR(IER)
      IF ( IER.NE.0 ) THEN
        CALL FLGBK('GB_TRIGGER_RCP',1)
        CALL FLGSET('GB_TRIGGER_RCP',.TRUE.)
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
