      SUBROUTINE L0EXPD(DL0TYP, NDATA, DATA_WORD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Decode routine for the L0 subdetector.
C-              It reads from TRGR raw data bank and returns the unpacked
C-              data in the DATA_WORD array, in the following format :
C-
C-   Inputs  : DL0TYP [I] : decode type (1: ADC data block,
C-                                       2: Scaler data block,
C-                                       3: Vertex data block)
C-   Outputs : NDATA      : number of output data words, if DL0TYP found.
C-             DATA_WORD [I*] : array containing packed data words.
C-
C-   Created   1-JUN-1992   Jeffrey Bantly  from CDEXPD to L0EXPD
C-   Updated  26-JAN-1993   Jeffrey Bantly  remove various INCLUDEs no longer
C-                                          needed, add extra protections 
C-   Updated  26-JAN-1994   Jeffrey Bantly  make Monte Carlo compatible, more
C-                                          RCP and data-driven.  Run 1a and 
C-                                          Run 1b compatible. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZTRGR.LINK'
C
      INTEGER DL0TYP
      INTEGER NDATA
      INTEGER DATA_WORD(1000)
      INTEGER IOFFSET
      INTEGER LCRATE, LTRGR, LCRATE0
      INTEGER NEVOLD
      INTEGER I
      INTEGER ISTART
      INTEGER HEAD_LEN
      INTEGER BLOCK_WORD
      INTEGER BLOCK_TYPE
      INTEGER BLOCK_LENGTH
      INTEGER VERSION, L1VERSION
      INTEGER DETAILS(5,7)
      INTEGER NTDATA,TDETAILS(4,4)
      INTEGER L1DATA,L1DETAILS(5,7),L1TDETAILS(4,4)
      INTEGER GZFIND_CRATE, GZFIND_CRATE_TRAILER
      EXTERNAL GZFIND_CRATE, GZFIND_CRATE_TRAILER
      INTEGER L0_CRATE_DATA_LENGTH, L1_CRATE_DATA_LENGTH
      INTEGER L0_CRATE_1ADATA_LENGTH, L0_CRATE_1BDATA_LENGTH
      INTEGER ERR
      INTEGER RUNNUM,EVTNUM
      INTEGER RUNSAV, IDSAV
C
      LOGICAL PRODUC, PRODFL
      LOGICAL  BLOCK_EXIST(3), FIRST
      LOGICAL EZERROR, MCDATA
      EXTERNAL PRODUC, EZERROR
C
      DATA NEVOLD/ -1 /
      DATA FIRST/.TRUE./
      DATA RUNSAV, IDSAV/-1,-1/
C
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        PRODFL = PRODUC()
        MCDATA = IQ(LHEAD+1) .GT. 1000
        FIRST=.FALSE.
        CALL EZPICK('LEVEL0_RCP')
        IF ( EZERROR(ERR) ) THEN
          CALL ERRMSG('LEVEL0-no-rcp','L0EXPD',
     &                                 'LEVEL0_RCP not found.','W')
        ELSE
          CALL EZGET('L1_CRATE_DATA_LENGTH', L1_CRATE_DATA_LENGTH, ERR)
          IF ( ERR.NE.0 ) L1_CRATE_DATA_LENGTH=2847
          CALL EZGET('L0_CRATE_1ADATA_LENGTH', L0_CRATE_1ADATA_LENGTH, 
     &        ERR)
          IF ( ERR.NE.0 ) L0_CRATE_1ADATA_LENGTH=791
          CALL EZGET('L0_CRATE_1BDATA_LENGTH', L0_CRATE_1BDATA_LENGTH, 
     &        ERR)
          IF ( ERR.NE.0 ) L0_CRATE_1BDATA_LENGTH=847
          IF ( MCDATA ) THEN
            CALL L0_GET_VERSION(VERSION)
            IF ( VERSION.EQ.1 ) THEN
              CALL EZGET('L0_CRATE_MC1ADATA_LENGTH', 
     &          L0_CRATE_DATA_LENGTH,ERR)
              IF (ERR.NE.0) L0_CRATE_DATA_LENGTH=107
            ELSEIF ( VERSION.EQ.2 ) THEN
              CALL EZGET('L0_CRATE_MC1BDATA_LENGTH', 
     &          L0_CRATE_DATA_LENGTH,ERR)
              IF (ERR.NE.0) L0_CRATE_DATA_LENGTH=107
            ELSE
              L0_CRATE_DATA_LENGTH=107
            ENDIF
          ENDIF
          CALL EZRSET
        ENDIF
      ENDIF
C
C Should only need to be called once per event:
C
      CALL EVNTID(RUNNUM,EVTNUM)
      IF (RUNNUM .NE. RUNSAV .OR. EVTNUM .NE. IDSAV) THEN
        RUNSAV = RUNNUM
        IDSAV = EVTNUM
        IF ( RUNNUM.LE.73290 ) THEN
          L0_CRATE_DATA_LENGTH = L0_CRATE_1ADATA_LENGTH
        ELSE
          L0_CRATE_DATA_LENGTH = L0_CRATE_1BDATA_LENGTH 
        ENDIF
      ENDIF
C
      NDATA = 0
      IF (DL0TYP .LT. 1 .OR. DL0TYP .GT. 3) THEN
        IF (.NOT.PRODFL) CALL ERRMSG('LEVEL0-bad-block-request',
     &                      'L0EXPD','Bad Data Block type','I')
        GOTO 999
      ENDIF
C
      IF (LHEAD .EQ. 0) THEN
        IF (.NOT.PRODFL) CALL ERRMSG('LEVEL0-no-HEAD','L0EXPD',
     &                      'HEAD bank not found','W')
        GOTO 999
      ENDIF
      LTRGR = LQ(LHEAD-IZTRGR)
      IF (LTRGR .EQ. 0) THEN
        IF (.NOT.PRODFL) CALL ERRMSG('LEVEL0-no-TRGR','L0EXPD',
     &                      'TRGR bank not found','W')
        GOTO 999
      ENDIF
C
C  fetch location in TRGR bank of L0 crate, crate 01
C
      LCRATE = GZFIND_CRATE('TRGR',LTRGR,1)
      IF (LCRATE .LE. 0) THEN
        IF (.NOT.PRODFL) CALL ERRMSG('LEVEL0-no-crt01','L0EXPD',
     &                                    'Crate 01 data not found','W')
        GOTO 999
      ENDIF
      HEAD_LEN = IQ(LCRATE)
      IF (HEAD_LEN .NE. 4) THEN
        IF (.NOT.PRODFL) CALL ERRMSG('LEVEL0-bad-crate-header','L0EXPD',
     &              'Crate 01 header was corrupted','W')
        NDATA = 0
        GOTO 999
C        HEAD_LEN=4
      ENDIF
      VERSION = 1
C
C   If same event, skip mapping.
C
      IF (IQ(LHEAD+9).EQ.NEVOLD) GOTO 100
      NEVOLD = IQ(LHEAD+9)
C
C   Decode the header words.
C
      LCRATE0 = LCRATE-1
      CALL L0_DECODE_HEADER(LCRATE0,VERSION,NDATA,DETAILS)
C
C   Map the Level 0 data words.  Purpose is to find the last data word.
C
      BLOCK_EXIST(1) = .FALSE.
      BLOCK_EXIST(2) = .FALSE.
      BLOCK_EXIST(3) = .FALSE.
      ISTART = LCRATE + HEAD_LEN
      IOFFSET = ISTART
C
C   Loop over Level 0 data blocks.
C
   10 CONTINUE
      BLOCK_WORD   = IQ(IOFFSET+1)
      BLOCK_TYPE   = IBITS(BLOCK_WORD,16,16)
      BLOCK_LENGTH = IBITS(BLOCK_WORD, 0,16)
C
C   Check for end of Level 0 crate data.
C
      IF (BLOCK_WORD.EQ.(IOFFSET-LCRATE+1+4)) GOTO 100
C
      IF (BLOCK_TYPE .LT. 1 .OR. BLOCK_TYPE .GT. 3) GOTO 100
      BLOCK_EXIST(BLOCK_TYPE) = .TRUE.
C
C   If not end of Level 0 crate data, then loop through next data block.
C
      IOFFSET = IOFFSET + BLOCK_LENGTH
      IF ( IOFFSET-ISTART .GT. 1000 ) THEN
        IF (.NOT.PRODFL) CALL ERRMSG('LEVEL0-bad-data-length','L0EXPD',
     &         'LEVEL0 data length too long - corrupted data','W')
        NDATA=0
        GOTO 999
      ENDIF
      GOTO 10
C
C   Set the number of data words.
C
  100 CONTINUE
      IF (.NOT.BLOCK_EXIST(DL0TYP)) GOTO 999
      NDATA = IOFFSET - ISTART
      IF ( NDATA .GT. 1000 ) THEN
        IF (.NOT.PRODFL) CALL ERRMSG('LEVEL0-bad-data-length','L0EXPD',
     &         'LEVEL0 data length too long - corrupted data','W')
        NDATA=0
        GOTO 999
      ENDIF
C
C   Copy data words to output array.
C
      DO I=1,NDATA
        DATA_WORD(I)=IQ(ISTART+I)
      ENDDO
C
C   Decode the trailer words.
C
      LCRATE0 = LCRATE+HEAD_LEN+NDATA
      CALL L0_DECODE_TRAILER(LCRATE0,VERSION,NTDATA,TDETAILS)
C
C   Check integrity of header and trailer words.
C
      IF ( TDETAILS(1,1).NE.L0_CRATE_DATA_LENGTH ) THEN
        IF (.NOT.PRODFL) CALL ERRMSG('LEVEL0-trlr-bad-datalen','L0EXPD',
     &               'L0 trailer total wordcount wrong','W')
        NDATA=0
        GOTO 999
      ENDIF
      IF ( DETAILS(2,1).NE.TDETAILS(2,1) ) THEN
        IF (.NOT.PRODFL) CALL ERRMSG('LEVEL0-hdr-trlr-trigno-mis',
     &       'L0EXPD','Header trailer trigger number mismatch','W')
        NDATA=0
        GOTO 999
      ENDIF
      IF ( DETAILS(3,1).NE.TDETAILS(2,2) ) THEN
        IF (.NOT.PRODFL) CALL ERRMSG('LEVEL0-hdr-trlr-crateid-mis',
     &      'L0EXPD','Header trailer crate id mismatch','W')
        NDATA=0
        GOTO 999
      ENDIF
      IF ( MCDATA ) GOTO 999
      IF ( .NOT.PRODFL ) THEN
        LCRATE = GZFIND_CRATE('TRGR',LTRGR,11)
        IF (LCRATE .LE. 0) THEN
          IF (.NOT.PRODFL) CALL ERRMSG('LEVEL0-no-crt11','L0EXPD',
     &               'Crate 11 data not found','W')
          GOTO 999
        ENDIF
        LCRATE0 = LCRATE - 1
        CALL L0_DECODE_HEADER(LCRATE0,L1VERSION,L1DATA,L1DETAILS)
        IF ( DETAILS(2,1).NE.L1DETAILS(2,1) ) THEN
          CALL ERRMSG('LEVEL0-crt01-11-sync-mismatch','L0EXPD',
     &               'Crate 01 and crate 11 sync words mismatch','W')
        ENDIF
        IF ( DETAILS(3,1).NE.1 ) THEN
          CALL ERRMSG('LEVEL0-crt01-crateid-mismatch','L0EXPD',
     &               'Crate 01 crate id wrong','W')
        ENDIF
        IF (IBITS(DETAILS(4,1),30,1).NE.IBITS(L1DETAILS(4,1),30,1)) THEN
          CALL ERRMSG('LEVEL0-crt01-11-sys30-mismatch','L0EXPD',
     &               'Crates 01 and 11 data orig bit mismatch','W')
        ENDIF
        IF (IBITS(DETAILS(4,1),29,1).NE.IBITS(L1DETAILS(4,1),29,1)) THEN
          CALL ERRMSG('LEVEL0-crt01-11-sys29-mismatch','L0EXPD',
     &               'Crates 01 and 11 data type bit mismatch','W')
        ENDIF
        LCRATE0 = GZFIND_CRATE_TRAILER('TRGR',LTRGR,11) - 1
        CALL L0_DECODE_TRAILER(LCRATE0,VERSION,NTDATA,L1TDETAILS)
        IF ( L1TDETAILS(1,1).NE.L1_CRATE_DATA_LENGTH ) THEN
          CALL ERRMSG('LEVEL0-trlr-bad-L1datalen','L0EXPD',
     &               'L0 - L1 trailer total wordcount wrong','W')
        ENDIF
        IF ( TDETAILS(2,1).NE.L1TDETAILS(2,1) ) THEN
          CALL ERRMSG('LEVEL0-trlr-trigno-L1mis','L0EXPD',
     &               'L0 trailer trigger number mismatch with L1','W')
        ENDIF
        IF ( L1DETAILS(3,1).NE.L1TDETAILS(2,2) ) THEN
          CALL ERRMSG('LEVEL0-L1-hdr-trlr-crateid-mis','L0EXPD',
     &               'L1 header trailer crate id mismatch','W')
        ENDIF
C        IF ( TDETAILS(3,1).NE.L1TDETAILS(3,1) ) THEN
C          CALL ERRMSG('LEVEL0-trlr-tknps-L1mis','L0EXPD',
C     &          'L0 trailer token pass status mismatch with L1','W')
C        ENDIF
      ENDIF
C
      GOTO 999
C
C  Done.
C
C----------------------------------------------------------------------------
  999 RETURN
      END
