      SUBROUTINE L1UTIL_WRITE_BLS_FILE(FNAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create a BLS Gain Correction File from the
C-   L1C_BLS_GAIN_CORRECTION common block.
C-
C-   Inputs  : FNAME    The name of the file to create.
C-   Outputs : file output
C-   Controls: none
C-
C-   Created  20-SEP-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-   Updated  20-DEC-1991   Philippe Laurens, Steven Klocek   
C-                      The file name is now an argument rather than a
C-                      parameter.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:L1C_BLS_GAIN_CORRECTION.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$PARAMS:LSM_ZEB.PARAMS'
C
      INTEGER IETAC, IPHIC, ILYRC
      INTEGER LLBLS, CURLOC
      INTEGER IUSER
      PARAMETER (IUSER = 527)
      INTEGER UNIT_NUM, IER
      CHARACTER*(*) FNAME
      CHARACTER*80  BUFFER
      LOGICAL OK
C
      INTEGER TRULEN
      EXTERNAL TRULEN
C
      CALL INZCOM(0)
C
      CALL GTUNIT(IUSER,UNIT_NUM,IER)
      IF (IER .NE. 0) THEN
        CALL ERRMSG('GTUNIT','L1UTIL_WRITE_BLS_FILE',
     &    'Error calling GTUNIT', 'F')
        GOTO 999
      ENDIF
C
      CALL FZFILE(UNIT_NUM, 0, 'O')
      IF (IQUEST(1) .NE. 0) THEN
        BUFFER = 'Could not FZFILE:' // FNAME
        CALL ERRMSG( 'FZFILE', 'L1UTIL_WRITE_BLS_FILE', 
     &                BUFFER(1:TRULEN(BUFFER)), 'F')
      ENDIF
C
      CALL BKLBLS(LLBLS)
C
      CURLOC = LLBLS + 1
      IQ(CURLOC) =   1 ! VERSION
      IQ(CURLOC+1) =  (2 * NETAL + 1) * NPHIL * NLYRL ! SIZE OF DATA IN FIXED
                                                      ! PART
      IQ(CURLOC+2) = 0 ! SIZE OF REPEATING PART
      IQ(CURLOC+3) = 0 ! NUMBER OF REPETITIONS OF REPEATING PART

      CURLOC = CURLOC + 4

      DO ILYRC = 1, NLYRL
        DO IPHIC = 1, NPHIL
          DO IETAC = -NETAL, NETAL
            Q(CURLOC) = BLS_GAIN_CORRECTION( IETAC, IPHIC, ILYRC) 
            CURLOC = CURLOC + 1
          END DO
        END DO
      END DO
C
C       Write to FZ file
C
      CALL D0OPEN(UNIT_NUM, FNAME, 'OU', OK)
      OK = .TRUE.
C
      IF (OK .NEQV. .TRUE.) THEN
        BUFFER = 'Could not open:' // FNAME
        CALL ERRMSG( 'D0OPEN', 'L1UTIL_WRITE_BLS_FILE',
     &                BUFFER(1:TRULEN(BUFFER)), 'F')
      ENDIF
C
C
      CALL FZOUT(UNIT_NUM, IXMAIN, LLBLS, NEW_EVENT, 'S', 1, 0, 0)
      IF (IQUEST(1) .NE. 0) THEN
        BUFFER = 'Could not FZOUT:' // FNAME
        CALL ERRMSG( 'FZOUT', 'L1UTIL_WRITE_BLS_FILE', 
     &               BUFFER(1:TRULEN(BUFFER)), 'F')
      ENDIF
C
      CALL FZENDO(UNIT_NUM, 'T')
      CLOSE(UNIT_NUM)
C
      CALL RLUNIT(IUSER,UNIT_NUM,IER)
      IF (IER .NE. 0) THEN
        CALL ERRMSG('RLUNIT','L1UTIL_WRITE_BLS_FILE',
     &    'Error calling RLUNIT', 'F')
        GOTO 999
      ENDIF
C
  999 RETURN
C----------------------------------------------------------------------
      END
C
