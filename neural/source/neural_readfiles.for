      SUBROUTINE NEURAL_READFILES(NPAT,NFILE,FILE,INPUTS,OUTPUTS,Q)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C       Read NPAT input patterns from  NFILE files together
C       with their corresponding weights and desired output patterns
C       and load the arrays PATTERN_IN, PATTERN_OUT and PATTERN_WT
C       in common block /JETNET/. If NFILE > 1 the patterns are mixed
C       in a "round-robin" fashion.
C-
C-   Inputs  : NPAT             [I]     Total number of patterns requested
C-             NFILE            [I]     Number of files
C-             FILE(*)          [C*]    File names
C-             INPUTS(*)        [I]     Inputs to select
C-             OUTPUTS(10,*)    [F]     Desired outputs
C-             Q(*)             [F]     Weight/file; if -1 then use weights
C-                                      supplied in ntuples
C-   Outputs : NPAT             [I]     Actual number of patterns read
C-   Controls:
C-
C-   Created   7-JAN-1991   Chip Stewart
C-   Updated  25-JAN-1991   Harrison B. Prosper
C-   Updated  15-FEB-1991   Pushpa C. Bhat
C-   Updated  22-FEB-1991   Pushpa C. Bhat, Harrison B. Prosper
C-   Updated  16-JUL-1991   Harrison B. Prosper
C-      Update NPAT
C-   Updated  27-AUG-1991   Boaz Klima  - Different format for more inputs
C-   Updated  29-SEP-1991   Harrison B. Prosper
C-    Use Ntuples
C-   Updated   8-OCT-1991   Harrison B. Prosper
C-   Updated   6-NOV-1991   Boaz Klima
C-    Remove user-defined input mixing
C-   Updated   9-DEC-1991   Harrison B. Prosper
C-      Allow training on one file only.
C-      Make compatible with DHDIR
C-   Updated  15-JUN-1992   Harrison B. Prosper
C-      Split up
C-   Updated  27-JAN-1993   Harrison B. Prosper
C-   Updated   9-MAR-1993   Harrison B. Prosper
C-      Do not use BUFFER for inputs
C-   Updated   9-JUL-1993   Hannu E. Miettinen, Harrison B. Prosper
C-      Allow reading of ntuple as-is (that is, do NOT skip the first
C-      two fields)
C-   Updated  17-DEC-1993   Chip Stewart
C-   Updated   7-MAR-1995   Harrison B. Prosper
C-      Implement event weighting
C-   Updated  29-MAR-1995   Chip Stewart  - MORE EVT WEIGHTING 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IEVENT, NPAT, NFILE
      CHARACTER*(*) FILE(*)
      INTEGER INPUTS(*)
      REAL    OUTPUTS(10,*), Q(*)
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:JETNET.INC'
      INCLUDE 'D0$INC:JNDAT1.INC'
C----------------------------------------------------------------------
      LOGICAL NEWFILE,HEXIST
      INTEGER NF,N,II,JJ,III,JJJ,EVENT
      INTEGER STATUS,I,J,IFILE,IPAT
      INTEGER LREC,IDN,ICYCLE,NCALL,NP,IWTAG
      CHARACTER*80 REMARK,TITLE,TITLE_NEW
      CHARACTER*4 CHOPT
      INTEGER FILE_OFFSET,OK_OFFSET
      REAL    XOUT,XT(MAXIN),XLO(MAXIN),XHI(MAXIN)
C----------------------------------------------------------------------
      DATA NCALL/0/
C----------------------------------------------------------------------
      SAVE NF,NCALL,FILE_OFFSET,OK_OFFSET
C----------------------------------------------------------------------
      NCALL = NCALL + 1
      NF = NFILE
C
C ****  Get number of events per file, check tags, define inputs to
C ****  use.
C
      NP = NPAT/NFILE           ! Requested number of patterns/file
C
      DO IFILE = 1, NFILE
        NEWFILE = .FALSE.
C
        IF ( HEXIST(NTUPLE_ID) ) THEN
          CALL HDELET(NTUPLE_ID)
        ENDIF
C
C ****  OPEN FILE
C
        CALL GTUNIT(NETID,IUNIT,STATUS)
        CHOPT = ' '
        LREC = 0
        CALL HROPEN(IUNIT,'INPUT',FILE(IFILE),CHOPT,LREC,STATUS)
        IF ( STATUS .NE. 0 ) THEN
          REMARK = 'Unable to open ntuple file '//FILE(IFILE)
          CALL ERRMSG('OPENERR','NEURAL_READFILES',REMARK,'F')
        ENDIF
C
C ****  Get number of entries
C
        CALL HRIN(NTUPLE_ID,9999999,0)
        CALL HNOENT(NTUPLE_ID,N)
C
C ****  CLOSE FILE
C
        CALL HREND('INPUT')    ! Close the direct access file
        CALL RLUNIT(NETID,IUNIT,STATUS)    ! Return the logical unit
        CLOSE(UNIT=IUNIT)
C
C ****  Update number of patterns/file
C
        NP = MIN(NP,N)
      ENDDO
C
C ****  Now compute total number of patterns
C
      NPAT = NP*NFILE
      NPATTERNS = NPAT
C
C ****  If training then mix files
C
      DO IFILE = 1, NFILE
        NEWFILE = .FALSE.
C
        IF ( HEXIST(NTUPLE_ID) ) THEN
          CALL HDELET(NTUPLE_ID)
        ENDIF
C
C ****  OPEN FILE
C
        CALL GTUNIT(NETID,IUNIT,STATUS)
        CHOPT = ' '
        LREC = 0
        CALL HROPEN(IUNIT,'INPUT',FILE(IFILE),CHOPT,LREC,STATUS)
        IF ( STATUS .NE. 0 ) THEN
          REMARK = 'Unable to open ntuple file '//FILE(IFILE)
          CALL ERRMSG('OPENERR','NEURAL_READFILES',REMARK,'F')
        ENDIF
C
C ****  DEFINE FIELDS TO USE FOR TRAINING/TESTING
C
        NTAG = MAXIN
        CALL HRIN(NTUPLE_ID,9999999,0)
        CALL HGIVEN(NTUPLE_ID,TITLE,NTAG,TAGS,XLO,XHI)
        CALL DGN_BEGIN('NEURAL_RCP','PATTERNS_INPUTS',NTAG,TAGS,STATUS)
        CALL NEURAL_EVENT_WEIGHT_TAG(NTAG,TAGS,IWTAG,STATUS)
C
C ****  Get number of fields. NOTE: not all may be used for training
C
        CALL DGN_GET_TAGS(MAXIN,NFIELDS,FIELDS)
C
        EVENT = 0
        DO IPAT = IFILE, NPATTERNS, NFILE
          EVENT = EVENT + 1
C
C ****  Get fields listed in PATTERNS_INPUTS, including those NOT to be
C ****  used as inputs to the network
C
          CALL DGN(NTUPLE_ID,EVENT,XT,STATUS)
          IF ( STATUS .NE. 0 ) GOTO 10
C
C ****  Copy patterns
C
          CALL UCOPY(XT(1),PATTERN_IN(1,IPAT),NFIELDS)
C
C ****  Copy desired outputs
C
          CALL UCOPY(OUTPUTS(1,IFILE),PATTERN_OUT(1,IPAT),NOUTPUTS)
C
C ****  Copy weights; weight/file for now
C
          IF(IWTAG.GT.0) THEN
            PATTERN_WT(IPAT) = Q(IFILE)*XT(IWTAG)
          ELSE
            PATTERN_WT(IPAT) = Q(IFILE)
          END IF
        ENDDO
   10   CONTINUE
C
C ****  CLOSE FILE
C
        CALL HREND('INPUT')               ! Close the direct access file
        CALL RLUNIT(NETID,IUNIT,STATUS)   ! Return the logical unit
        CLOSE(UNIT=IUNIT)
      ENDDO
C
C ****  Copy input selector
C
      CALL UCOPY(INPUTS(1),PATTERN_SELECT(1),NINPUTS)
  999 RETURN
      END
