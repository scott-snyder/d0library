      FUNCTION L2JETS_TRGRFL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill common block L2JETS_TRGR
C-
C-   Returned value  : .TRUE. if successful
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   6-MAY-1990   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL L2JETS_TRGRFL
      INCLUDE 'D0$INC:L2JETS_TRGR.INC'
      LOGICAL OK,EZERROR
      INTEGER IER                       ! Error variable
C----------------------------------------------------------------------
      L2JETS_TRGRFL = .FALSE.           ! Set false for now
C---Select correct RCP bank and fill the common block
      CALL EZPICK('L2JETS_TRGR')
      OK = EZERROR(IER)
      IF(.NOT.OK) THEN 
        CALL ERRMSG('NO_L2J_TRGR','L2JETS_TRGRFL','bank not found','F')
        GO TO 999
      ENDIF
C
      CALL EZGET('LEM_HOT',LEM_HOT,IER)
      IF (IER.NE.0) GOTO 900
      CALL EZGET('LJT_HOT',LJT_HOT,IER)
      IF (IER.NE.0) GOTO 900
      CALL EZGET('LEN_EM1',LEN_EM1,IER)
      IF (IER.NE.0) GOTO 900
      CALL EZGET('LEN_EM2',LEN_EM2,IER)
      IF (IER.NE.0) GOTO 900
      CALL EZGET('LEN_HD1',LEN_HD1,IER)
      IF (IER.NE.0) GOTO 900
      CALL EZGET('LEN_HD2',LEN_HD2,IER)
      IF (IER.NE.0) GOTO 900

      CALL EZGET('CTS_TO_GEV',CTS_TO_GEV,IER)
      IF (IER.NE.0) GOTO 900
      CALL EZGET('CTS_EM_OFF',CTS_EM_OFF,IER)
      IF (IER.NE.0) GOTO 900
      CALL EZGET('CTS_HD_OFF',CTS_HD_OFF,IER)
      IF (IER.NE.0) GOTO 900


C--- If we reach this point, then we are successful:
      L2JETS_TRGRFL = .TRUE.
      GOTO 999
  900 CONTINUE                          ! Error
      CALL ERRMSG('JETS','L2JETS_TRGRFL',' Error reading parameters: ',
     &  'W')
  999 CONTINUE
      CALL EZRSET
      RETURN
      END
