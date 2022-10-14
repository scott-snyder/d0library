      SUBROUTINE NEURAL_TEST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Test the results of a neural network.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   9-JAN-1991   Chip Stewart
C-   Modified  5-Feb-1991   Pushpa Bhat
C-   Updated  22-FEB-1991   Pushpa C. Bhat, Harrison B. Prosper
C-   Updated  29-SEP-1991   Harrison B. Prosper
C-   Updated   8-FEB-1993   Pushpa C. Bhat
C-   Updated  10-MAR-1993   Harrison B. Prosper
C-   Updated   9-JUL-1993   Hannu E. Miettinen, Harrison B. Prosper
C-      Allow the use of ntuple as-is
C-   Updated  10-MAR-1995   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:JETNET.INC'
      INCLUDE 'D0$INC:JNDAT1.INC'
C----------------------------------------------------------------------
      INTEGER NRIGHT,IPAT,I
      INTEGER NLAYERS, NUPDATE,IDN_OUT
      INTEGER LUNIT,STATUS,NFILE, IFILE,NPAT,LUNOUT
      INTEGER INPUTS(MAXIN), OFFSET, NPAT1
      REAL    CUT
      LOGICAL OK, SUCCESS
      CHARACTER*80 FILE(MAXFIL)
C
      REAL   OUTPUTS(10,MAXFIL), Q(MAXFIL)
C----------------------------------------------------------------------
C
      CALL EZPICK('NEURAL_RCP')
C
      CALL EZGET_i('NUMBER_OF_TEST_PATTERNS',NPAT,STATUS)
      NPAT1 = MIN(MAXPAT,NPAT)
      WRITE(6,'(/,''  --- Number of Patterns Requested: '',I10)') NPAT
C
      CALL EZGET('OUTPUT_CUT',CUT,STATUS)
      IF ( CUT .LE. 0.0 ) THEN
        CUT = 0.50               ! Default cut
      ENDIF
C
      CALL EZGET_i('INP_NTUPLE_ID',NTUPLE_ID,STATUS)
      IF ( NTUPLE_ID .LE. 0 ) NTUPLE_ID = 2
      CALL EZGET_i('OUT_NTUPLE_ID',IDN_OUT,STATUS)
      IF(IDN_OUT.EQ.0) IDN_OUT = NTUPLE_ID+1
C
      OFFSET = 10
C
C ****  No setting of parameters or switches is needed.
C ****  All information is present in the file NEURAL_WEIGHTS
C
      CALL GTUNIT (NETID,LUNIT,STATUS)
      IF(STATUS.EQ.0) THEN
        OK = .TRUE.
        CALL D0OPEN(LUNIT,'NEURAL_WEIGHTS','IF',OK)
        IF ( OK ) THEN
          CALL JNREAD(LUNIT)
          CLOSE(LUNIT)
        ELSE
          CALL ERRMSG('NOWEIGHTS','NEURAL_TEST',
     &      'Unable to read file NEURAL_WEIGHTS','F')
        ENDIF
      ENDIF
C
C ****  Get input and output selection; define NINPUTS and NOUTPUTS
C
      CALL NEURAL_SELECT(NFILE,FILE,INPUTS,OUTPUTS,Q,1)
C
      IF ( NINPUTS .LE. 0 ) CALL ERRMSG('ZERO INPUTS','NEURAL_TEST',
     &    'Check NEURAL.RCP','F')
      IF ( NOUTPUTS .LE. 0 ) CALL ERRMSG('ZERO OUTPUTS','NEURAL_TEST',
     &    'Check NEURAL.RCP','F')
C
      CALL EZRSET
C
C ****  Update various totals
C
      NLAYERS  = MSTJN(1)               ! Number of layers
      NUPDATE  = MSTJN(2)               ! Number of patterns/update
      MSTJN(10)       = NINPUTS
      MSTJN(9+NLAYERS)= NOUTPUTS
C
C ****  Open file for error function log
C
      CALL GTUNIT (NETID,LUNOUT,STATUS)
      IF(STATUS.EQ.0) THEN
        CALL D0OPEN(LUNOUT,'TEST_OUT_FILE','OF',OK)
      ELSE
        CALL ERRMSG('NOUNIT','NEURAL_TEST',
     &      'Cannot get unit number for TEST_OUT_FILE','W')
      ENDIF
      MSTJN(6) = LUNOUT
C
C ****  Loop over test data sets
C
      DO IFILE = 1, NFILE
C
C ****  Read all patterns into common block /JETNET/ and then
C ****  book ntuple. Must call readfiles before book.
C
        NPAT = NPAT1
        CALL NEURAL_READFILES(NPAT,1,FILE(IFILE),INPUTS,OUTPUTS,Q)
        WRITE(6,'(/,''  --- Number of Patterns: '',I10)') NPATTERNS
C
C ****  BOOK HISTOS & NTUPLE
C
        CALL NEURAL_BOOK(IDN_OUT,FILE(IFILE),' ',STATUS)
C
        NRIGHT = 0

C...loop over all patterns:
        DO IPAT=1, NPATTERNS

C...put pattern IPAT in OIN
          DO I=1, NINPUTS
            OIN(I) = PATTERN_IN(PATTERN_SELECT(I),IPAT)
          ENDDO
C
C...feed it through the net
          CALL JNTEST
C
C ****  Fill output ntuple
C
          CALL NEURAL_FILL(IDN_OUT,IPAT)
C
C ****  Check for success
C
          SUCCESS = .TRUE.
          DO I=1, NOUTPUTS
            IF ( OUT(I) .LT. CUT ) THEN
              SUCCESS = .FALSE.
            ENDIF
          ENDDO
C
C ****  Count successes
C
          IF ( SUCCESS ) THEN
            NRIGHT = NRIGHT + 1
          ENDIF
        ENDDO
C
C...write out success rate
C
        WRITE(*,*) IFILE, NRIGHT,' passed cut of ',CUT, ' out of ',NPAT
C
        CALL NEURAL_BOOK_END
      ENDDO
C
      CALL HOUTPU(LUNOUT)
      CALL HPRINT(0)
      CALL HOUTPU(6)
C
  999 RETURN
      END
