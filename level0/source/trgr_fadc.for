      SUBROUTINE TRGR_FADC(CHANNEL,DATA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpack TRGR block FADC information.  Dumb
C-    routine - can't deal with zero suppressed data.
C-
C-   Inputs  : CHANNEL = channel number 0-15
C-   Outputs : DATA = array of unpacked FADC data
C-   Controls:
C-
C-   Created   1-DEC-1992   Susan K. Blessing
C-   Updated  13-JAN-1994   Jeffrey Bantly    new Crate 71 data format upgrade
C-   Updated   2-FEB-1994   Chip Stewart:  functions with zero-suppressed data
C-   Updated  26-JUL-1995   Jeffrey Bantly  fix data length error bug, ncards=3 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER CHANNEL, DATA(0:511)
      INTEGER I,LTRGR,GZTRGR
      INTEGER L71,L71_TRAILER,GZFIND_CRATE,GZFIND_CRATE_TRAILER_WAS
      INTEGER NHEAD,NLUMIN,NHV,NFADC,NMUON, LFADC,LWORD,NWORDS,WORD
      INTEGER LABEL,LENGTH, LCHAN,CHAN, BIN, LOC_CLUS,LENGTH_CLUS,OFFSET
      INTEGER VERSION, MIN_CHAN,MAX_CHAN,NCHAN, NCARDS,PROPER_LENGTH
      INTEGER LEN_CARD, NWCHAN,LCLUS
      CHARACTER*80 MESSG
      LOGICAL FIRST
      SAVE FIRST,MIN_CHAN,MAX_CHAN,LEN_CARD,PROPER_LENGTH,NCHAN,NMUON
      DATA FIRST/.TRUE./,MIN_CHAN/0/,NCHAN/16/,NMUON/80/
C----------------------------------------------------------------------
      DO I=0,255
        DATA(I)=-1
      ENDDO
      LTRGR = GZTRGR()
      IF(LTRGR.LE.0) GOTO 999
      L71 = GZFIND_CRATE('TRGR',LTRGR,71)
C      IF (FIRST) THEN       ! set nominal accounting for 3 FADC cards
        NCARDS = 3
        MAX_CHAN = MIN_CHAN + NCHAN*NCARDS - 1
        LEN_CARD = 4*(256*NCARDS+8)+1 !bogus length (indicates how many cards)
        PROPER_LENGTH=66              ! non  zero suppressed
C
C ****  TOP DOWN  (to check number of cards -  shouldn't change in data)
C
        NHEAD = IQ(L71) + 1
        VERSION = IQ(L71+3)
        NLUMIN = IQ(L71+NHEAD)
        IF (VERSION.EQ.3) NLUMIN = NLUMIN + 1
        IF (NLUMIN.LE.1) NLUMIN = 1
        NHV = IQ(L71+NHEAD+NLUMIN)
        IF (VERSION.EQ.3) NHV = NHV + 1
        IF (NHV.LE.1) NHV = 1
        NFADC = IQ(L71+NHEAD+NLUMIN+NHV)
        IF (VERSION.EQ.3) NFADC = NFADC + 1
        IF (NFADC.NE.LEN_CARD) THEN
          DO NCARDS=1,3
            LEN_CARD = 4*(256*NCARDS+8)+1
            IF (LEN_CARD.EQ.NFADC) GOTO 700
          END DO
          WRITE(MESSG,*) 'TRGR FADC data length =',NFADC, LEN_CARD
          CALL ERRMSG('LEVEL0-bad-71-length','TRGR_FADC',MESSG,'W')
          GO TO 999
        END IF
        FIRST=.FALSE.
C      ENDIF
 
      IF ( CHANNEL.LT.MIN_CHAN .OR. CHANNEL.GT.MAX_CHAN ) GOTO 999
C
C ****  BOTTOM UP UNPACKING to handle zero-supression
C
  700 L71_TRAILER = GZFIND_CRATE_TRAILER_WAS ()
      IF((L71.GT.L71_TRAILER).OR.(L71.EQ.0) ) THEN
        CALL ERRMSG('TRGR_CRATE_71','TRGR_FADC','NO 71','W')
        GOTO 999
      END IF
C
C ****  Skip past MUON words
C
      LFADC = L71_TRAILER -NMUON-1  ! begining of MUON block (80 words)
      IF(IQ(LFADC).NE.NMUON) THEN   ! missed MUON words
        LFADC = LFADC-1               ! end of FADC block
        IF(IQ(LFADC).EQ.NMUON) GOTO 10
        CALL ERRMSG('TRGR_CRATE_71','TRGR_FADC','NMUON not 80','W')
        GOTO 999
      END IF
   10 LFADC = LFADC-1               ! end of FADC block
      LCHAN = LFADC                 ! last channel label & length
      DO CHAN = 0, MAX_CHAN
        LABEL = IBITS(IQ(LCHAN),16,15)  ! Channel label (0-47)
C
C **** Total length for channel in bytes (divide by 4 for number of words)
C
        LENGTH = IBITS(IQ(LCHAN),0,16)/4
        IF ( LABEL.LT.MIN_CHAN .OR. LABEL.GT.MAX_CHAN ) THEN
          CALL ERRMSG('TRGR_CRATE_71','TRGR_FADC','CHANNEL LABEL','W')
          GOTO 999
        ELSE IF ( LENGTH.GT.PROPER_LENGTH )  THEN
          CALL ERRMSG('TRGR_CRATE_71','TRGR_FADC','CHANNEL LENGTH','W')
          GOTO 999
        ELSE IF ( LABEL.NE.CHANNEL ) THEN
          GOTO 990
        ELSE IF ( LENGTH.LE.1 )  THEN
          GOTO 999
        END IF
C
C ****  Loop through local cluster
C
        NWCHAN = 0
        LCLUS = LCHAN-1
  890   LOC_CLUS = IBITS(IQ(LCLUS),16,15)
        LENGTH_CLUS = IBITS(IQ(LCLUS),0,16)
        NWORDS = (LENGTH_CLUS)/4
        IF ( NWORDS.LT.0 .OR. NWORDS.GT.LENGTH ) THEN
          CALL ERRMSG('TRGR_CRATE_71','TRGR_FADC','LOCAL LENGTH','W')
          GOTO 999
        END IF
        OFFSET = LOC_CLUS - LENGTH_CLUS + 5! slice of start cluster
        IF ( OFFSET.LT.0 .OR. OFFSET.GT.255 ) THEN
          CALL ERRMSG('TRGR_CRATE_71','TRGR_FADC','LOCAL CLUSTER','W')
          GOTO 999
        END IF
        DO WORD = 0, NWORDS - 2
          LWORD = LCLUS - NWORDS + WORD + 1
          DO I = 0, 3
            BIN = WORD*4 + ABS(I-3) + OFFSET
            DATA(BIN) = IBITS(IQ(LWORD),I*8,8)
          END DO
        END DO
        LCLUS = LCLUS - NWORDS
        NWCHAN = NWCHAN+NWORDS
        IF((NWCHAN+2).LT.LENGTH) GOTO 890  ! another local cluster?
        GOTO 999                           ! That's all for this channel
C
  990   CONTINUE
        IF (LENGTH.LT.1 .OR. LENGTH.GT.NFADC) GOTO 999
        LCHAN = LCHAN - LENGTH
      END DO
C
C-----------------------------------------------------------------------------
  999 RETURN
      END
