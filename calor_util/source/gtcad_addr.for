      SUBROUTINE GTCAD_ADDR(NCAD,IADDR,IWORD,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns IWORD for a given packed IADDR
C-   (most sig 16 bits) given which CAD bank (NCAD=1,2).
C-   CAUTION : SLOW ROUTINE- SEARCH FOR ADDRESS
C-
C-   Inputs  : NCAD         [I]  1 for CAD1, 2 for CAD2
C-             IADDR        [I]  32 bit word with most significant 16 bits
C-                               Set to desired packed CAD bank address.
C-                               Ignoring NEGLIM and SCALE bits in IADDR.
C-
C-   Outputs : IWORD        [I]  CAD bank data word with address IADDR
C-             IER          [I]  Error code - 0=ok,
C-                              -1 --- Channel with IADDR not found
C-                              -2 --- Bad total word count in trailer
C-                              -3 --- NCAD wrong
C-                              -4 --- No CAD bank.
C-                              -5 --- Expecting header length but not found
C-   Controls: NONE
C-
C-   Created 19-FEB-1990   Chip Stewart, W.G.D.Dharmaratna.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER LCAD,GZCAD1,GZCAD2,POINT,IWORD,IADDR
      INTEGER NCAD,CRATE,CHANNEL,MASK,NCAD_PREV
      INTEGER IER

      INTEGER ICHANNEL,ICARD,ICRATE
      INTEGER NCHANNEL,NCARD,NCRATE
      INTEGER I,J,K
      INTEGER CONTROL_WORD,HEADER_LEN,STATUS,VERSION,PULSER,SYNCH
      INTEGER CHECKSUM,NWORD
      INTEGER NCH,IADDR1,IADDR2,L1,L2,LMEAN,IAND
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_HEX_NO.PARAMS'
      INCLUDE 'D0$LINKS:IZCAD1.LINK'
      INCLUDE 'D0$LINKS:IZCAD2.LINK'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      LOGICAL FIRST
      SAVE POINT,ICRATE,CHANNEL,ICHANNEL,NCHANNEL,NCARD
      SAVE NCAD_PREV
      BYTE CONTROL_BYTE(4)
      EQUIVALENCE(CONTROL_WORD,CONTROL_BYTE)
C------------------------------------------------------------------------

      IF ( MASK .EQ. 0) THEN ! MAKE MASK FOR SEEING ADDRESS PART OF CAD WORD
        DO I = 16 + IDEPTB, 16 + IADCB + NADCB   !BIT POSTIONS FROM ADC_HEX_NO
          MASK = MASK + 2**I
        END DO
      END IF
C
C ****  Do a binary search for PARAM
C
      IER = 0
      NCH = 0
      IF ( NCAD .LT. 1 .OR. NCAD .GT. 2 ) THEN
        IER = -3
        GOTO 499
      ENDIF
C
      IF ( NCAD.EQ.1 ) LCAD = GZCAD1()
      IF ( NCAD.EQ.2 ) LCAD = GZCAD2()
      IF ( LCAD .LE. 0 ) THEN
        IER = - 4
        GOTO 499
      ENDIF
      POINT = 0
  114 HEADER_LEN   = IQ(LCAD+1+POINT)
      CONTROL_WORD = IQ(LCAD+3+POINT)
      ICRATE =  CONTROL_BYTE(BYTE4)
      NCARD  =  CONTROL_BYTE(BYTE3)
      POINT = POINT + HEADER_LEN + 2
      DO 14 J=0,NCARD
        NCH = IQ(LCAD+POINT)
        IADDR1 = IAND(IQ(LCAD+POINT+1),MASK)
        IADDR2 = IAND(IQ(LCAD+POINT+NCH),MASK)
        IF ( IADDR1.GT.IADDR) THEN
          IER = -1
          GOTO 499
        ELSE IF ( IADDR1.EQ.IADDR) THEN
          IWORD = IQ(LCAD+POINT+1)
          GOTO 499
        ELSE IF ( IADDR2.EQ.IADDR) THEN
          IWORD = IQ(LCAD+POINT+NCH)
          GOTO 499
        ELSE IF ( IADDR1.LT.IADDR .AND. IADDR2.GT.IADDR) THEN
          L1 = POINT + 2
          L2 = POINT + NCH - 1
          GOTO 410
        END IF
        POINT = POINT + NCH + 1
   14 CONTINUE
C
C ****  Check for another crate
C
      IF ( HEADER_LEN.GT.3) THEN    ! CRATE TRAILER
C
C ****  CHECK TOTAL WORD COUNT MATCH
C
        NWORD = IQ(LCAD+POINT+1)
        IF (NWORD.NE.POINT+4) THEN
          CALL ERRMSG('GTCAD','GTCAD_ADDR',
     &       'TRAILER WORD COUNT MISMATCH','W')
          IER = -2
          GOTO 999
        END IF
C
C ****  DON'T CHECK CHECKSUM UNTIL WE KNOW WHAT IT IS
C
        POINT = POINT + 3
      END IF
      IF(IQ(LCAD+POINT+1).EQ.HEADER_LEN) GOTO 114
      IER = -1
      GOTO 499
C
C ****  BINARY SEARCH IN THIS CARD
C
  410 IF((L2-L1).LE.1) GOTO 460
C
C ****  FIND A CHANNEL HALF WAY BETWEEN  LUP AND LDOWN
C
      LMEAN = (L1+L2)/2
C
C ****  Get record at median position
C
      IADDR1 = IAND(IQ(LCAD+LMEAN),MASK)
      IF( IADDR1 .LT. IADDR )THEN
        L2=LMEAN
        GOTO 410
C
      ELSEIF ( IADDR1 .EQ. IADDR ) THEN
C
C ***** A MATCH has been FOUND
C
        IWORD = IQ(LCAD+LMEAN)
        GOTO 499
C
      ELSEIF( IADDR1 .GT. IADDR ) THEN
        L1 = LMEAN
        GOTO 410
      ENDIF
C
C **** No match found return error IER = -1
C
  460 IER = -1
  499 RETURN
  999 RETURN
      END
