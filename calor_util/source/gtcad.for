      SUBROUTINE GTCAD(NCAD,FIRST,CRATE,IWORD,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : For given CAD BANK (NCAD = 1 or 2) return the
C-   ADC CRATE, and CAD bank packed DATA/ADDRESS word for next channel from CAD
C-   Upon first call to GTCAD with given NCAD, the first channel in the bank
C-   is used.
C-
C-   Inputs  : NCAD     [I]     CAD bank number 1 for CAD1 or 2 for CAD2
C-             FIRST    [L]     TRUE if starting over to unpack this CAD bank
C-                              FALSE if getting subsequent data words.
C-
C-   Outputs : CRATE          [I]     CRATE number 0-11
C-             IWORD          [I]     CAD bank DATA/ADDR packed word
C-             IER            [I]     Error code; 0 --- OK
C-                              -1 --- Reached end of CAD bank
C-                              -2 --- Bad total word count in trailer
C-                              -3 --- NCAD wrong
C-                              -4 --- No CAD bank.
C-                              -5 --- Number of channels in card too big
C-                              -6 --- REACHED CAD TRAILER
C-   Controls: NONE
C-
C-   RELATED ROUTINES:
C-
C-      GTCAD_HEADER (NCAD,CRATE,HEADER_LEN,SYNCH,
C-     &  CONTROL_WORD,VERSION,STATUS,PULSER,IER)
C-      to retrive CRATE CAD header info
C-
C-      GTCAD_TOTAL ( NCAD,NCH,IER)
C-      to count ADC channels in CAD BANK
C-
C-      GTCAD_CHANNEL(NCAD,IADDR,IWORD,IER)
C-      to get IWORD for a given packed IADDR (most sig 16 bits)
C-      (SLOW ROUTINE- SEARCH FOR ADDRESS)
C-
C-   Created 19-FEB-1990   Chip Stewart, W.G.D.Dharmaratna.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER LCAD,GZCAD1,GZCAD2,POINT,IWORD,IADDR
      INTEGER NCAD,CRATE,CHANNEL,MASK,NCAD_PREV,NWORDS
      INTEGER IER

      INTEGER ICHANNEL,ICARD,ICRATE
      INTEGER NCHANNEL,JCARD,NCRATE
      INTEGER I,J,K
      INTEGER CONTROL_WORD,HEADER_LEN,STATUS,VERSION,PULSER,SYNCH
      INTEGER CHECKSUM,NWORD,NWORD_PREV
      INTEGER NCH,IADDR1,IADDR2,L1,L2,LMEAN
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_HEX_NO.PARAMS'
      INCLUDE 'D0$LINKS:IZCAD1.LINK'
      INCLUDE 'D0$LINKS:IZCAD2.LINK'
      INCLUDE 'D0$INC:CUNFLG.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      LOGICAL FIRST,BAD,BTEST
      SAVE POINT,ICRATE,CHANNEL,ICHANNEL,NCHANNEL,JCARD
      SAVE NCAD_PREV
      BYTE CONTROL_BYTE(4)
      EQUIVALENCE(CONTROL_WORD,CONTROL_BYTE)
      DATA NCAD_PREV/0/,NCHANNEL/0/,JCARD/0/,HEADER_LEN/0/
C----------------------------------------------------------------------
C
      IER = 0
      IF ( NCAD .LT. 1 .OR. NCAD .GT. 2 ) THEN
        IER = -3
        GOTO 999
      ENDIF
C
      IF (FIRST .OR. NCAD.NE.NCAD_PREV) THEN
C
C ****  READ IN FIRST CRATE CAD BANK HEADER
C
        NWORD_PREV = 0
        NCAD_PREV = NCAD
        ICHANNEL = 0
        ICARD = -1
        POINT = 0
        NCRATE = NADCRC
        IF ( IAND(D0VSN,64).GT.0 .AND. CALVSN.EQ.2) NCRATE = 87
      END IF
      IF ( NCAD.EQ.1 ) LCAD = LQ(LHEAD-IZCAD1)
      IF ( NCAD.EQ.2 ) LCAD = LQ(LHEAD-IZCAD2)
      IF ( LCAD .LE. 0 ) THEN
        IER = - 4
        GOTO 999
      ENDIF
C
C ****  LOOK FOR NEXT CHANNEL
C
      ICHANNEL = ICHANNEL + 1
      IF ( POINT.EQ.0 .OR. ICHANNEL .GT. NCHANNEL) THEN
C
C ****  LOOK FOR NEXT CARD IF ALREADY FINISHED PREVIOUS CARD
C
  100   ICARD = ICARD + 1
        IF ( POINT.EQ.0 .OR. ICARD.GT. JCARD ) THEN
C
C ****  LOOK FOR NEXT CRATE TRAILER/HEADER IF FINISHED PREVIOUS CRATE
C
          IF ( POINT.GT.0  .AND. HEADER_LEN.GT.3) THEN    ! CRATE TRAILER
C
C ****  CHECK TOTAL WORD COUNT MATCH
C
            NWORD = IQ(LCAD+POINT+1)
            IF (BTEST(D0VSN,5) .AND. SFTVSN.LT.2) THEN
              BAD = NWORD.NE.(POINT+4)
            ELSE 
              BAD = NWORD.NE.(POINT+4-NWORD_PREV)
              NWORD_PREV = POINT+ 4
            END IF
            IWORD = 1
            IF (BAD) THEN
              CALL ERRMSG('GTCAD','GTCAD',
     &            'TRAILER WORD COUNT MISMATCH','W')
              IER = -2
              GOTO 999
            END IF
C
C ****  DON'T CHECK CHECKSUM UNTIL WE KNOW WHAT IT IS
C
            CHECKSUM = IQ(LCAD+POINT+4)
            POINT = POINT + 4
          END IF
          IF( IQ(LCAD+POINT+1) .NE. HEADER_LEN .AND. POINT.NE.0) THEN
C
C ****  REACHED THE END OF THE CAD BANK
C
            IER = -1
            GOTO 999
          END IF
          NWORDS = IQ(LCAD-1)
          IF(POINT.GT.(NWORDS-NTRAILER)) THEN
            IER = -6
            GOTO 999
          END IF
          HEADER_LEN = IQ(LCAD+POINT+1)
          CONTROL_WORD=IQ(LCAD+POINT+3)
          CRATE =  CONTROL_BYTE(BYTE4)
          JCARD  = CONTROL_BYTE(BYTE3)
          ICRATE = CRATE/10 + 1
          ICARD = 0
          IF( (ICRATE.GT.NCRATE).AND.(CRATE.NE.87).AND.(CRATE.NE.77))
     &       CALL ERRMSG('GTCAD','GTCAD','TOO MANY CRATES','W')
          IF( JCARD .GT. NADCC - 1 ) 
     &       CALL ERRMSG('GTCAD','GTCAD','TOO MANY CARDS','W')
          POINT = POINT + HEADER_LEN + 1
        ENDIF
        POINT = POINT + 1 
        NCHANNEL = IQ(LCAD+POINT)
        IF( NCHANNEL.GT.768) THEN
          CALL ERRMSG('GTCAD','GTCAD',
     &      'NUMBER OF CHANNELS IN CARD TOO BIG','W')
          IER = -5
          GOTO 999
        ELSE IF( NCHANNEL.EQ.0) THEN
          GOTO 100
        END IF
        ICHANNEL = 1
      END IF
      POINT =  POINT + 1
      IWORD = IQ(LCAD+POINT)
C
  999 RETURN
      END
