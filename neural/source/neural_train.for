      SUBROUTINE NEURAL_TRAIN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Train a Neural Network using JETNET.
C-   All parameters read from the rcp bank NEURAL_RCP. See D0note 1606
C-   for mathematical details.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   7-JAN-1991   Chip Stewart, Harrison B. Prosper,
C-                          Pushpa Bhat
C-   Updated  28-FEB-1991   Harrison B. Prosper
C-      Added some more printout
C-   Updated  15-JUL-1991   Harrison B. Prosper
C-      Compute better cost (error) functions
C-   Updated  29-OCT-1991   Boaz Klima, Harrison B. Prosper
C-      Add periodic dumping of weights, reading weights from file
C-      and use NEURAL_READFILES
C-   Updated   8-MAY-1992   K. Wyatt Merritt, Harrison B. Prosper
C-   Updated  20-AUG-1992   Harrison B. Prosper
C-      Modify cost function printout
C-   Updated   1-SEP-1992   Harrison B. Prosper
C-      Fix cost function printout
C-   Updated  20-OCT-1992   Harrison B. Prosper
C-      Allow for networks trained in range (epsilon, 1-epsilon)
C-      rather than [0,1].
C-   Updated  25-JAN-1993   Harrison B. Prosper
C-      Change a few calculations; See D0note 1606
C-   Updated   9-JUL-1993   Hannu E. Miettinen, Harrison B. Prosper
C-      Allow for using ntuple as-is
C-   Updated   7-MAR-1995   Harrison B. Prosper
C-      Implement event weighting
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:JETNET.INC'
      INCLUDE 'D0$INC:JNDAT1.INC'
C----------------------------------------------------------------------
      INTEGER ICYCLE,IPAT,NREPORT,NDUMP, I
      INTEGER LUNIT,STATUS, NFILE, NCYCLE, NUPDATE
      INTEGER NPAT,NLAYERS,LUNOUT, IDN_OUT, INPUTS(MAXIN)
C
      REAL   OUTPUTS(10,MAXFIL), Q(MAXFIL)
C
      LOGICAL EZERROR, OK,OPENED_OUT_FILE,READ_WEIGHTS
      CHARACTER*80 FILE(MAXFIL)
C----------------------------------------------------------------------
      CALL EZPICK('NEURAL_RCP')
      IF ( EZERROR(STATUS) ) THEN
        CALL ERRMSG('NEURAL','NEURAL_TRAIN',
     &    'Cannot pick NEURAL_RCP bank','F')
      ENDIF
C
      CALL EZGET('NUMBER_OF_TRAINING_CYCLES',NCYCLE,STATUS)
      CALL EZGET('NUMBER_OF_TRAIN_PATTERNS',NPAT,STATUS)
      NPAT = MIN(MAXPAT,NPAT)
C
      CALL EZGET('INP_NTUPLE_ID',NTUPLE_ID,STATUS)
      IF ( NTUPLE_ID .LE. 0 ) NTUPLE_ID = 2
      CALL EZGET('OUT_NTUPLE_ID',IDN_OUT,STATUS)
      IF(IDN_OUT.EQ.0) IDN_OUT = NTUPLE_ID+1
C
      CALL EZGET('NREPORT',NREPORT,STATUS)
      IF ( NREPORT .LE. 0 ) THEN
        NREPORT = 100
      ENDIF
      CALL EZGET('NDUMP_WEIGHTS',NDUMP,STATUS)
      IF ( NDUMP.GT.0 ) THEN
        NDUMP = NCYCLE/NDUMP
      ENDIF
C
      CALL EZGET('READ_WEIGHTS',READ_WEIGHTS,STATUS)
C
C ****  Initialize JNDAT1 from NEURAL_RCP (After reading weights)
C
      CALL NEURAL_INIT_JNDAT1
C
C **** Initialize the net:
C
      IF ( READ_WEIGHTS ) THEN
        OK = .TRUE.
        CALL D0OPEN(LUNIT,'NEURAL_WEIGHTS','IF',OK)
        IF ( OK ) THEN
          WRITE(6,'(''  Reading from NEURAL_WEIGHTS'')')
          CALL JNREAD(LUNIT)
C ****  OVERWRITE EVOLVING NETWORK PARAMETERS WITH RCP VALUES
          CALL NEURAL_INIT_JNREAD
          CLOSE(LUNIT)
        ELSE
          CALL ERRMSG('NOWEIGHTS','NEURAL_TEST',
     &      'Unable to read file NEURAL_WEIGHTS','F')
        ENDIF
      ELSE
        WRITE(6,'(''  Starting with randomized weights'')')
        CALL JNINIT
      ENDIF
C
C ****  Get input and output selection; define NINPUTS and NOUTPUTS
C
      CALL NEURAL_SELECT(NFILE,FILE,INPUTS,OUTPUTS,Q,0)
C
      IF ( NINPUTS .LE. 0 ) CALL ERRMSG('ZERO INPUTS','NEURAL_TRAIN',
     &    'Check NEURAL.RCP','F')
      IF ( NOUTPUTS .LE. 0 ) CALL ERRMSG('ZERO OUTPUTS','NEURAL_TRAIN',
     &    'Check NEURAL.RCP','F')
C
      CALL EZRSET
C
C ****  Update various totals
C
      NLAYERS  = MSTJN(1)               ! Number of layers
      NUPDATE  = MSTJN(2)               ! Number of patterns/update
      MSTJN(10)       = NINPUTS         ! Number of input nodes
      MSTJN(9+NLAYERS)= NOUTPUTS        ! Number of output nodes
C
C ****  Read all patterns into common block /JETNET/
C
      CALL NEURAL_READFILES(NPAT,NFILE,FILE,INPUTS,OUTPUTS,Q)
C
      CALL GTUNIT (NETID,LUNIT,STATUS)
      IF ( STATUS .NE. 0 ) THEN
        CALL ERRMSG('NOUNIT','NEURAL_TRAIN',
     &      'Cannot get unit number for NEURAL_WEIGHT','W')
      ENDIF
C
C ****  Open file for error function log
C
      CALL GTUNIT (NETID,LUNOUT,STATUS)
      IF(STATUS.EQ.0) THEN
        CALL D0OPEN(LUNOUT,'TRAIN_OUT_FILE','OF',OPENED_OUT_FILE)
      ELSE
        CALL ERRMSG('NOUNIT','NEURAL_TRAIN',
     &      'Cannot get unit number for TRAIN_OUT_FILE','W')
      ENDIF
      MSTJN(6) = LUNOUT
C
C ****  SET NUMVER OF PATTERNS PER UPDATE / UPDATES PER EPOCH (CYCLES)
C
      IF(MSTJN(2).GT.NPAT) THEN
        MSTJN(2) = NPAT  ! more than npat does no concievable good
        MSTJN(9) = 1
      ELSE IF(MSTJN(2).LT.NPAT) THEN
        I = MAX(1,NPAT/MSTJN(2))
        MSTJN(2) = NPAT/I
        MSTJN(9) = I
      END IF
C
      WRITE(LUNOUT,'(''  '')')
      WRITE(6,'(''  '')')
      WRITE(6,'(''  --- Number of    CYCLES   : '',I10)') NCYCLE
      WRITE(6,'(''  --- Number of    PATTERNS : '',I10)') NPAT
      WRITE(6,'(''  --- Patterns per UPDATE   : '',I10)') MSTJN(2)
      WRITE(6,'(''  --- Updates per  EPOCH    : '',I10)') MSTJN(9)
C
C
C ****          LOOP OVER TRAINING CYCLES
C
      DO ICYCLE = 1, NCYCLE
C
C ****          LOOP OVER ALL PATTERNS
C
        DO IPAT = 1, NPAT
C
C ****  put pattern IPAT into OIN using selected inputs
C
          DO I=1,NINPUTS
            OIN(I)=PATTERN_IN(PATTERN_SELECT(I),IPAT)
          ENDDO
C
C ****  put desired output pattern in OUT
C
          DO I=1,NOUTPUTS
            OUT(I)=PATTERN_OUT(I,IPAT)
          ENDDO
C
C ****  Set weight for current pattern (LOG base 10 for format)
C
          PARJN(40) = LOG10(PATTERN_WT(IPAT))
C
C ****  Train net and update every NUPDATE calls
C
          CALL JNTRAL
C
        ENDDO
C
C ****  Dump result of training
C
        IF ( NDUMP .GT. 0 ) THEN
          IF ( MOD(ICYCLE,NDUMP) .EQ. 0 ) THEN
            CALL D0OPEN(LUNIT,'NEURAL_WEIGHTS','OF',OK)
            IF ( OK ) THEN
              WRITE(6,1010) ICYCLE
              CALL JNDUMP(LUNIT)
              CLOSE(LUNIT)
            ENDIF
          ENDIF
        ENDIF
C
C ****  Compute Global Error
C
        IF ( (ICYCLE.EQ.1).OR.MOD(ICYCLE,NREPORT) .EQ. 0 )
     &    CALL NEURAL_REPORT(LUNOUT,ICYCLE,NCYCLE)
      ENDDO
C
C ****  Dump result of training
C
      CALL D0OPEN(LUNIT,'NEURAL_WEIGHTS','OF',OK)
      IF ( OK ) THEN
        CALL JNDUMP(LUNIT)
        CLOSE(LUNIT)
      ENDIF
C
C ****  WRITE NTUPLE WITH patterns and outputs
C
      CALL NEURAL_BOOK(IDN_OUT,' ','TRAIN_HSAVE_FILE',STATUS)
C
      DO IPAT = 1, NPAT
        DO I = 1, NINPUTS
          OIN(I)=PATTERN_IN(PATTERN_SELECT(I),IPAT)
        ENDDO
C
        CALl JNTEST
C
        CALL NEURAL_FILL(IDN_OUT,IPAT)
      ENDDO
      CALL NEURAL_BOOK_END
C
      CALL HOUTPU(LUNOUT)
      CALL HPRINT(0)
      CALL HOUTPU(6)
C
C ****  Close TRAIN_OUT FILE
C
      IF ( OPENED_OUT_FILE ) CLOSE(LUNOUT)
      CALL HDELET(0)
C
  999 RETURN
 1010 FORMAT(1X,1X,I10,1X,'Dumping Weights')
      END
