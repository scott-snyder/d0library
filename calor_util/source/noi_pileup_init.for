      SUBROUTINE NOI_PILEUP_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C
C        SETS UP THE PILEUP CALCULATION
C
C        Relevant RCP parameters:
C          Follwing, if true, add contributions cell by cell for
C          DO_PAST          pileup from past events
C          DO_PRESENT       multiple events this bucket
C          DO_FUTURE        signal from next bucket
C          DO_SIGNAL        include event in present bucket
C                              (F for pedestal studies)
C          DO_ELECTRONICS   elctronics noise
C          DO_URANIUM       uranium noise
C
C
C        Additional switches used by PILEUP_INIT
C          OCCUPY   average occupancy (number of interactions per bucket)
C          DOUBLE_VERTEX probability of keeping double vertices
C
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  7-AUG-1991   Peter Nemethy and Allen I. Mincer
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:NOISY.INC'
      LOGICAL OK
C
      REAL SUM_NEG,SUM_POS
      INTEGER JJ
      REAL T_SHIFT,AMP_MAX
      REAL SAMP_LOSS,AMPLIS,AMPLIB,NOI_SHAPE
      INTEGER IER,LENGTH,SPIUNI
      CHARACTER*80 FILNAM,FILE_NAME,FILE_OUT
      REAL PULSE_SCALE
      DATA PULSE_SCALE/1.0/
C
C######EXECUTION BEGINS HERE######
C
      IF(PRINT_OUTPUT)THEN
        WRITE(OUTUNI,124)
        WRITE(OUTUNI,124)
      ENDIF
  124 FORMAT(/)
C
      IF(.NOT.USE_EXPSIG)THEN         ! USE SPICE SIGNAL NOI_SHAPE
        CALL EZPICK('NOISY_RCP')              ! select NOISY bank
        CALL EZGETS('SPICE_FILE',1,FILNAM,LENGTH,IER)
        CALL EZ_STORE_NAME('SPICE_DAT',FILNAM(1:LENGTH),IER)
        CALL EZRSET
        FILNAM=FILNAM(1:LENGTH)
        FILE_NAME=FILNAM
        CALL GTUNIT(222,SPIUNI,IER)
        IF(IER.EQ.0)THEN
          CALL D0OPEN(SPIUNI,FILE_NAME,'IF',OK)
          IF(.NOT.OK)THEN
            CALL ERRMSG('NOISY','PILEUP_INIT',
     &              'ERROR OPENING SPICE_DAT','W')
          ENDIF
        ELSE
          CALL ERRMSG('NOISY','PILEUP_INIT',
     &                  'ERROR GETTING INPUT UNIT','W')
        ENDIF
C
        READ(SPIUNI,131)HEADLINE
  131   FORMAT(A60)
        READ(SPIUNI,132)T_SHIFT
        READ(SPIUNI,132)AMP_MAX
  132   FORMAT(F15.7)
        IF(PRINT_OUTPUT)THEN
          CALL TRNLNM(FILE_NAME,FILE_OUT,LENGTH)
          WRITE(OUTUNI,*)' SPICE DATA FILE: ',FILE_OUT(1:LENGTH)
          WRITE(OUTUNI,124)
          WRITE(OUTUNI,*)' SPICE:',HEADLINE
          WRITE(OUTUNI,*)' T_SHIFT,AMP_MAX',T_SHIFT,AMP_MAX
        ENDIF
        DO JJ=1,49
          READ(SPIUNI,*)TP(JJ),AP(JJ)
C##### 133          FORMAT(E10.3,1X,E10.3)
          TP(JJ)=TP(JJ)*1.0E6-T_SHIFT
          AP(JJ)=AP(JJ)/AMP_MAX
        ENDDO
        TP(50)=80.0*T_CROSS
        AP(50)=0.0
        CLOSE(SPIUNI)
        IF(PRINT_OUTPUT)THEN
          WRITE(OUTUNI,124)
          DO JJ=1,50
            WRITE(OUTUNI,*)' SPICE:TIME,AMPLITUDE',JJ,TP(JJ),AP(JJ)
          ENDDO
          WRITE(OUTUNI,124)
        ENDIF
      ENDIF                   !SPICE OUTPUT
C
      CALL NOI_FILL_BUCKET
C
      IF(PRINT_OUTPUT)THEN
        SUM_POS=0.0
        SUM_NEG=0.0
        DO JJ=1,100
          IF(ABS(FRACTION(JJ)).GE.VAL_MIN)THEN
            WRITE(OUTUNI,101) JJ-80,100.0*FRACTION(JJ)
          ENDIF
  101     FORMAT(' BUCKET=',I3,'    %=',F8.2)
          IF(FRACTION(JJ).LT.0.)SUM_NEG=SUM_NEG+FRACTION(JJ)
          IF(FRACTION(JJ).GT.0.)SUM_POS=SUM_POS+FRACTION(JJ)
        ENDDO
        WRITE(OUTUNI,124)
        WRITE(OUTUNI,*)' POS E SUM AND SUM*OCCUP:',
     &    SUM_POS,SUM_POS*OCCUPY
        WRITE(OUTUNI,*)' NEG E SUM AND SUM*OCCUP:',
     &    SUM_NEG,SUM_NEG*OCCUPY
        WRITE(OUTUNI,124)
        AMPLIS=NOI_SHAPE(0.0)
        AMPLIB=NOI_SHAPE(-T_SAMP)
        SAMP_LOSS=100.
        IF(AMPLIS.NE.0.0)SAMP_LOSS=100.0*AMPLIB/AMPLIS
        WRITE(OUTUNI,*)' BLS SIGNAL LOSS IN %:',SAMP_LOSS
        WRITE(OUTUNI,124)
      ENDIF
C
      CALL NOI_POISSON
C
      RETURN
      END
