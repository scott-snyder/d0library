      SUBROUTINE L1C_LOAD_BLS_FILE(LUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Load the BLS Gain Correction File
C-
C-   Inputs  : FILENAME The name of the file to load
C-             LUN      The IO unit number
C-   Outputs : filled common block L1C_BLS_GAIN_CORRECTION
C-   Controls: none
C-
C-   Created  24-SEP-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:L1C_BLS_GAIN_CORRECTION.PARAMS'
      INCLUDE 'D0$INC:L1C_BLS_GAIN_CORRECTION.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:QUEST.INC'
C
      INTEGER LUN, LLBLS, CURLOC, ILYRC, IPHIC, IETAC, FLEN, IER
      LOGICAL OK, SUCCESS
      CHARACTER*132 FILENAME
C
      CALL INZCOM(0)
C
      CALL L1UTIL_PICK_L1SIM_RCP()
      FILENAME = ' '
      CALL EZGETS('BLS_GAIN_CORRECTION_FILE',1,FILENAME,FLEN,IER)
      IF (IER .NE. 0) THEN
        CALL EZGET_ERROR_TEXT(IER, FILENAME)
        CALL ERRMSG('EZGETS','L1C_LOAD_BLS_FILE', FILENAME, 'F')
      ENDIF
      CALL EZRSET
C
      OK = .TRUE.
      CALL D0OPEN(LUN,FILENAME(1:FLEN),'IU',OK)
      IF (OK .EQV. .FALSE.) THEN
        CALL ERRMSG('L1SIM BAD BLS FILE', 'L1C_LOAD_BLS_FILE',
     &    'Error opening BLS Gain file','F')
        GOTO 3000
      ENDIF
C
      CALL FZFILE(LUN,0,'I')
C
      CALL FZIN(LUN,IXMAIN,LLBLS,2,' ',0,0)
      IF (IQUEST(1) .NE. 0) THEN
        CALL ERRMSG('L1SIM BAD BLS FILE', 'L1C_LOAD_BLS_FILE', 
     &    'Error reading BLS Gain file','F')
        GOTO 3000
      ENDIF
C
      CALL FZENDI(LUN,'T')
C
C Check the header on the LBLS bank
C
      IF (IQ(LLBLS +LLBLS_VERSION) .NE. 1) THEN
        CALL ERRMSG('L1SIM BAD BLS FILE', 'L1C_LOAD_BLS_FILE', 
     &    'BLS Gain file has wrong version number in header.', 'F')
        GOTO 3000
      ENDIF
C
      IF (IQ(LLBLS+LLBLS_LENGTH) 
     &  .NE. ((2 * NETAL + 1) * NPHIL * NLYRL) ) THEN
        CALL ERRMSG('L1SIM BAD BLS FILE', 'L1C_LOAD_BLS_FILE', 
     &    'BLS Gain file has wrong length in header.', 'F')
        GOTO 3000
      ENDIF
C
C Copy LBLS bank into array
C
      CURLOC = LLBLS + LLBLS_DATA
      DO ILYRC =1, NLYRL
        DO IPHIC = 1, NPHIL
          DO IETAC = -NETAL, NETAL
            BLS_GAIN_CORRECTION(IETAC, IPHIC, ILYRC) = Q(CURLOC)
            CURLOC = CURLOC + 1
          END DO                   
        END DO
      END DO
C
      CALL MZDROP(IXMAIN,LLBLS,' ')
      CLOSE (LUN)
      GOTO 999
C
 3000 CONTINUE
C----------------------------------------------------------------------
  999 RETURN
      END
