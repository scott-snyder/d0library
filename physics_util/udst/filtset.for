      LOGICAL FUNCTION FILTSET(FILTER,IRUN,WORD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : check for passed filters
C-
C-   Returned value  : true if FILTER passed, false if not
C-   Inputs  :         FILTER,IRUN,WORD
C-
C-   Created   6-DEC-1993   Ulrich Heintz
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NFILT, WORD(4), AFILTER_WORD, FILTERMASK,IRUN, GET_INDEX
      PARAMETER(NFILT=13)
      CHARACTER*20 NAMES(NFILT),FILTER
      INTEGER RUNS(13),BITS(13,NFILT), BIT, BIT1,J,JBIT,I
      DATA RUNS
C list   5.3   5.4   5.5   6.0   6.1   6.2   6.3   6.4   6.5   7.0
     & /51422,53761,54965,55217,55600,56298,57712,58130,59390,60100,
C        7.1   7.2   7.3
     &  62150,64086,99999/
      DATA BITS/42,44,44,43,44,44,44,41,42, 0, 0, 0, 0
     &         ,46,48,48,47,47,48,48,45,46,24,24,24,24
     &         ,45,47,47,46,46,47,47,43,44,41,41,42,42
     &         ,51,54,54,52,51,52,52,49,50,46,46,47,47
     &         ,50,53,53,51,50,51,51,48,49,26,26,26,26
     &         ,41,43,43,42,43,43,43,23,23, 0, 0, 0, 0
     &         ,28,28,28,28,28,28,28,28,28,28,28,28,28
     &         ,52,58,58,57,56,57,57,54,55,49,49,50,51
     &         ,15,15,15,15,15,15,15,15,15,18,18,18,18
     &         , 0, 0, 0, 0, 0, 0, 0,44,45,42,42,43,43
     &         ,24,24,24,24, 0,24,24,24,24,23,23,23,23
     &         , 0,27,27,27,27,27,27,27,27,27,27,27,27
     &         , 0, 0, 0,53,52,53,53,50,51,44,44,45,45/
      DATA NAMES/'ELE_MEDIUM','ELE_MAX','ELE_HIGH','ELE_2_MAX',
     &  'ELE_2_HIGH','GAM_MED_ISO','ELE_JET','ELE_JET_MAX','JET_MIN',
     &  'ELE_HIGH_TRK','ESC_HIGH','ELE_MISS','ELE_HIGH2'/
      EXTERNAL GET_INDEX
C----------------------------------------------------------------------
      FILTSET = .FALSE.
      IF(IRUN.LT.50226)THEN
        CALL ERRMSG('RUN<50226','FILTSET',
     &    'cannot select filters for these runs','W')
        GOTO 999
      ELSEIF(IRUN.EQ.51977)THEN
        CALL ERRMSG('RUN=51977','FILTSET',
     &    'cannot select filters for this run','W')
        GOTO 999
      ELSEIF(IRUN.EQ.52593)THEN
        CALL ERRMSG('RUN=52593','FILTSET',
     &    'cannot select filters for this run','W')
        GOTO 999
      ENDIF
C
      J = GET_INDEX(NFILT, NAMES, FILTER)
      IF(J.LE.0)CALL ERRMSG('UNKNOWN FILTER','FILTSET',FILTER,'F')
C
      DO I=1,13
        IF(IRUN.LT.RUNS(I))THEN
          BIT=BITS(I,J)
          GOTO 100
        ENDIF
      ENDDO
  100 IF(BIT.EQ.0)GOTO 999
C
      FILTERMASK = BIT+1
      IF(FILTERMASK.LE.16)THEN
        FILTSET  = JBIT(WORD(1),FILTERMASK).gt.0
      ELSEIF(FILTERMASK.LE.32)THEN
        FILTSET  = JBIT(WORD(2),FILTERMASK-16).gt.0
      ELSEIF(FILTERMASK.LE.48)THEN
        FILTSET  = JBIT(WORD(3),FILTERMASK-32).gt.0
      ELSEIF(FILTERMASK.LE.64)THEN
        FILTSET  = JBIT(WORD(4),FILTERMASK-48).gt.0
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
