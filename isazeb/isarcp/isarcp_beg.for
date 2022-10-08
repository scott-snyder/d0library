      LOGICAL FUNCTION ISARCP_BEG()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Does Begin run for ISARCP
C-
C-   Created  11-NOV-1989   Rajendran Raja   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ITAPES.INC'
      INCLUDE 'D0$INC:PRIMAR.INC'
      INTEGER IER,IRUN
      LOGICAL DO_ONE_TRACK,DO_READ_SEED
      CHARACTER*80 SEED_FILE
      INTEGER GTUNIT_ID,SEED_UNIT
      COMMON/SEED/XSEED 
      CHARACTER*24 XSEED    
      REAL*8 SEED
C----------------------------------------------------------------------
C
      CALL EZGET_i('NUMBER_OF_EVENTS',NEVENT,IER)
      CALL EZGET_i('RUN_NUMBER',IRUN,IER)
      CALL EZGET_l('DO_ONE_TRACK_EVENTS',DO_ONE_TRACK,IER)
      CALL EZGET_l('DO_READ_SEED',DO_READ_SEED,IER)
C
      ISARCP_BEG=.TRUE.
      IF(.NOT.DO_ONE_TRACK)CALL ISABEG(IER)                  ! INIT ISAJET.
      CALL ISARCP_BFL(IRUN)                     ! fill ISAJET begin record
      IF( DO_READ_SEED ) THEN
        CALL EZGET('GTUNIT_ID',GTUNIT_ID,IER)
        CALL EZ_FILE_OPEN(GTUNIT_ID,'RANDOM_SEED_SAVE_FILE','IF',
     &  SEED_UNIT,SEED_FILE,IER)
        IF (IER.NE.0) THEN 
          CALL ERRMSG('ISARCP','ISARCP_BEG',
     &      'NO FILE TO READ RANDOM NUMBER SEED FROM ','W')
        ELSE
          READ(SEED_UNIT,302) XSEED    
302       FORMAT(//28X,A24)  
          READ(XSEED,'(E24.15)') SEED    
C
          WRITE(ITLIS,*) SEED 
          CALL RANFST(SEED)   
        END IF
      END IF
      ISARCP_BEG = IER.EQ.0
  999 RETURN
      END
