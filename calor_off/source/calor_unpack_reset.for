      FUNCTION CALOR_UNPACK_RESET ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To Reset histograms at begining of run
C-                         for the EXAMINE2 CALOR_UNPACK package 
C-
C-   Returned value  : TRUE if OK
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  23-APR-1991   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL OK,DO_ANALYSIS,CHTINI,CALOR_UNPACK_RESET
      INTEGER IER
C----------------------------------------------------------------------
      CALOR_UNPACK_RESET = .FALSE.
      OK = CHTINI()
      CALL EZPICK('CAHITS_RCP')
      CALL EZERR(IER)
      IF ( IER.EQ.0) THEN
        CALL EZGET('DO_ANALYSIS',DO_ANALYSIS,IER)
      END IF
      IF(.NOT.DO_ANALYSIS) GOTO 999
      CALL DHDIR('CAHITS_RCP','HBOOK_DIRECTORY',IER,' ')
C         ! Create/Set HBOOK directory for CAHits
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('ERROR SETTING HBOOK DIRECTORY ',
     &    'CALOR_UNPACK_RESET', ' FROM DHDIR ','W')
        GOTO 999
      ENDIF
      CALL EZRSET
      CALL HRESET (0,' ')
      CALOR_UNPACK_RESET = .TRUE.
  999 RETURN
      END
