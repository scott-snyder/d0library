      SUBROUTINE RCPYTH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reads in PYTHIA parameters from RCP
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  29-JAN-1991   Norman A. Graf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER RCPAR(10000)
      INTEGER IER
      CHARACTER*50 COMMANDS(1000),UCOMMANDS(1000)
      CHARACTER*10 FRAME,BEAM,TARGET
      REAL WIN
      INTEGER DCOMMANDS,NCOMMANDS
      INTEGER I,NCALLS,IND,LENGTH
C----------------------------------------------------------------------
C
C ****  Initialize STP structure
C
      CALL INZSTP
C
C:::  Get initialization parameters from RCP file
C
      CALL EZPICK('PYTHIA_RCP')
C
      CALL EZGETS('EVENT_FRAME',1,FRAME,LENGTH,IER)
      CALL EZGETS('BEAM_1_TYPE',1,BEAM,LENGTH,IER)
      CALL EZGETS('BEAM_2_TYPE',1,TARGET,LENGTH,IER)
      CALL EZGET('SYSTEM_ENERGY',WIN,IER)
C
C:::  Get default list of commands for D0 PYTHIA
C
      CALL EZ_GET_CHARS('DEFAULT_LIST_OF_COMMANDS',DCOMMANDS,COMMANDS,
     &  IER)
      IF(IER.NE.0)THEN
        CALL ERRMSG('PYTHRA','RCPYTH',
     &    'DEFAULT LIST OF COMMANDS ERROR ','W')
      ENDIF
C
C      IF(DCOMMANDS.LE.0) GO TO 998
C
      DO I = 1 , DCOMMANDS
        CALL LUGIVE(COMMANDS(I))
      ENDDO
C
C:::  Now get any other user-supplied commands...
C
      CALL EZ_GET_CHARS('USER_LIST_OF_COMMANDS',NCOMMANDS,UCOMMANDS,IER)
      IF(IER.NE.0)THEN
        CALL ERRMSG('PYTHRA','RCPYTH',
     &    'USER LIST OF COMMANDS ERROR ','W')
      ENDIF
C
C      IF(NCOMMANDS.LE.0) GO TO 998
C
      DO I = 1 , NCOMMANDS
        CALL LUGIVE(UCOMMANDS(I))
      ENDDO
C
      CALL EZRSET
  998 CONTINUE
      CALL PYINIT(FRAME,BEAM,TARGET,WIN)
  999 RETURN
      END
