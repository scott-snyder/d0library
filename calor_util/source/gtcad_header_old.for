      SUBROUTINE GTCAD_HEADER_OLD (ICAD,CRATE,HEADER_LEN,SYNCH,
     &  CONTROL_WORD,VERSION,STATUS,PULSER,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns CAD bank header words given which CAD bank
C-   CRATE(7,17,27,37,47,57,8,18,28,38,48,58). If CRATE=0 on input, the first 
C-   CRATE header
C-   data is returned by default.
C-
C-   Inputs  : ICAD         [I]  1 for CAD1, 2 for CAD2, 
C-                                Zero if determined by crate
C-             CRATE        [I]  If non zero - search for CRATE header of CRATE
C-                               IF CRATE .LT. 0 THEN  READ HEADER FOR THE
C-                               (N=-CRATE) Nth SEQUENCIAL CRATE HEADER.
C-   Outputs : HEADER_LEN   [I]  Header length 3,4 or 5
C-             SYNCH        [I]  SYNCH word - least sig 16 bit turned on
C-             CONTROL_WORD [I]  Control word - contains crate, ADC CARD
C-                               information and BLS and ADC mode control data.
C-             VERSION      [I]  Packed version software version - including
C-                               TB, MC, or D0 data switch. 0 if HEADER_LEN < 4
C-             STATUS       [I]  Status error codes, Vertex in Z from LV0
C-             PULSER       [I]  Pulser pattern code. 0 if HEADER_LEN < 5
C-             IER          [I]  Error code - 0=ok,
C-                              -2 --- Bad total word count in trailer
C-                              -3 --- ICAD wrong
C-                              -4 --- No CAD bank.
C-                              -5 --- Expecting header length but not found
C-                              -6 --- CRATE not found in this bank
C-                              -7 --- TRAILER ONLY
C-   Controls:none
C-
C-   Created  19-FEB-1990   Chip Stewart, W.G.D.Dharmaratna.
C-   Updated   6-DEC-1990   Jan Guida  Modifications to read crates 8, 18, ...
C-                                         on CAD1 instead of CAD2  (CR run) 
C-   Updated  30-NOV-1992   Jan Guida - made GTCAD_HEADER_OLD 
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER LCAD,GZCAD1,GZCAD2,POINT,IWORD,IADDR
      INTEGER ICAD,KCAD,NCAD,CRATE,CHANNEL,MASK,NCAD_PREV,NWORD_PREV
      INTEGER IER

      INTEGER ICHANNEL,ICARD,ICRATE
      INTEGER NCHANNEL,NCRATE
      INTEGER I,J,K
      INTEGER CONTROL_WORD,HEADER_LEN,STATUS,VERSION,PULSER,SYNCH
      INTEGER VSN
      INTEGER CHECKSUM,NWORD
      INTEGER NCH,MCRATE,LCRATE,NWORDS
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CUNFLG.INC'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_HEX_NO.PARAMS'
      INCLUDE 'D0$LINKS:IZCAD1.LINK'
      INCLUDE 'D0$LINKS:IZCAD2.LINK'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      LOGICAL FIRST,BAD
      SAVE POINT,ICRATE,CHANNEL,ICHANNEL,NCHANNEL
      SAVE NCAD_PREV
      BYTE CONTROL_BYTE(4)
      EQUIVALENCE(CNTRLWRD,CONTROL_BYTE)
      BYTE VERSION_BYTE(4)
      EQUIVALENCE(VSN,VERSION_BYTE)
      INTEGER*2 VERSION_WORD(2)
      EQUIVALENCE(VSN,VERSION_WORD)

C
C ****  To retrive CRATE CAD header info
C
      IER = 0
      LCRATE = 0
      IF(CRATE.LT.0 .AND. CRATE.GE. -6) LCRATE = - CRATE
      KCAD = MOD(CRATE,10) - 6
      IF(ICAD.EQ.0)THEN
        IF(CRATE.EQ.0) THEN
          NCAD = 1
        ELSE
          NCAD = KCAD
        ENDIF
      ELSE
        NCAD = ICAD
      ENDIF
      IF(CRATE/10.LT.0 .OR. CRATE/10.GT.8)THEN
        IER = -6
        GOTO 999
      ENDIF
      IF (CRATE.GT.0) THEN
        IF ( ICAD.EQ.0 .AND. KCAD.NE.NCAD ) THEN
          IER = -6        ! Incorrect data cable when chosen by crate id
          GOTO 999
        ENDIF
      ENDIF
      IF ( NCAD .LT. 1 .OR. NCAD .GT. 2 ) THEN
        IER = -3
        GOTO 999
      ENDIF
C
      IF ( NCAD.EQ.1 ) THEN
        LCAD = GZCAD1()
        IF ( LCAD .LE. 0 ) THEN
          IER = - 4
          GOTO 999
        ENDIF
      ENDIF
      IF ( NCAD.EQ.2 ) THEN
        LCAD = GZCAD2()
        IF ( LCAD .LE. 0 .AND. ICAD.EQ.0) THEN
          LCAD = GZCAD1()
          IF (LCAD .GT. 0) THEN         ! Check for D0 CR running (1 data cable)
            VSN = IQ(LCAD+4)            ! Crates 8, 18, ... on CAD1
            D0VSN  =  VERSION_BYTE(BYTE4)
            CALVSN =  VERSION_BYTE(BYTE3)
            IF (D0VSN.NE.0 .OR. CALVSN.LE.3) THEN
              IER = - 4
              GOTO 999
            ENDIF
          ELSE
            IER = - 4
            GOTO 999
          ENDIF
        ELSE IF(LCAD.EQ.0) THEN
          IER = - 4
          GOTO 999
        ENDIF
      ENDIF
C
C ****  LOOK FOR CRATE IF CRATE>0
C
      POINT = 0
      MCRATE = 0
      NWORD_PREV = 0
      NWORDS = IQ(LCAD-1)
      IF (NWORDS.LE.NTRAILER) THEN  ! TRAILER ONLY 
        IER = -7
        GOTO 999
      END IF
  111 IF( (POINT+1).GT.NWORDS )THEN
        IER = -6
        GOTO 999
      END IF
      HEADER_LEN   = IQ(LCAD+1+POINT)
      IF( HEADER_LENGTH.GT.0 .AND. (HEADER_LEN.NE.HEADER_LENGTH))THEN
        IER = -5
        GOTO 999
      END IF
      HEADER_LENGTH = HEADER_LEN
      SYNCH        = IQ(LCAD+2+POINT)
      CONTROL_WORD = IQ(LCAD+3+POINT)
      CNTRLWRD     = CONTROL_WORD
      VERSION      = IQ(LCAD+4+POINT)
      VSN          = VERSION
      STATUS       =  IQ(LCAD+5+POINT)
      STATUS_VERTEX = STATUS
      MCRATE = MCRATE + 1
      IF( HEADER_LEN.LT.4) THEN
        VERSION    =  0
        VSN        =  0
        STATUS     =  IQ(LCAD+4+POINT)
      ELSE IF( HEADER_LEN.GT.4) THEN
        PULSER     = IQ(LCAD+6+POINT)
        PLSWRD     = PULSER
      ELSE
        PULSER = 0
        PLSWRD = 0
      END IF
        ICRATE =  CONTROL_BYTE(BYTE4)
        CRT    =  ICRATE
        NCARD  =  CONTROL_BYTE(BYTE3)
        D0VSN  =  VERSION_BYTE(BYTE4)
        CALVSN =  VERSION_BYTE(BYTE3)
        SFTVSN =  VERSION_WORD(WORD1)
        IF(VSN.EQ.-1 .OR. VSN.EQ.0) THEN
          IF(HEADER_LEN.LT.4)THEN       ! MONTE CARLO
            D0VSN  = 32                 ! '20'X
            CALVSN = 0
            SFTVSN = 0
          ELSE                          ! 5000 CHANNEL TEST
            D0VSN  = 0
            CALVSN = 2
            SFTVSN = 0
          ENDIF
        ENDIF
C
C-                              -5 --- Number of channels in card too big
C ****  IF CRATE INPUT THEN LOOK FOR THAT CRATE
C
      IF(CRATE.NE.0) THEN
        POINT = POINT + HEADER_LEN + 2
        IF (ICRATE.EQ.CRATE) THEN
          GOTO 999
        ELSE IF ( LCRATE.EQ.MCRATE) THEN  ! CRATE .LT.0 - SEQ CRATE NUMBER
          GOTO 999
        ELSE                            ! READ TRAILER FOR NEXT CRATE
          DO 12 J=0,NCARD
            POINT = POINT +IQ(LCAD + POINT) + 1
   12     CONTINUE
C
C ****  Check for another crate
C
          POINT = POINT - 1
          IF (HEADER_LEN.GT.3) THEN    ! CRATE TRAILER
C
C ****  CHECK TOTAL WORD COUNT MATCH
C
            NWORD = IQ(LCAD+POINT+1)
            IF (BTEST(D0VSN,5) .AND. SFTVSN.LT.2) THEN  !very old MC format 
              BAD = NWORD.NE.(POINT+4)
            ELSE 
              BAD = NWORD.NE.(POINT+4-NWORD_PREV)
              NWORD_PREV = POINT+ 4
            END IF
            IWORD = 1
            IF (BAD) THEN
              CALL ERRMSG('GTCAD','GTCAD_HEADER',
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
          IF (MCRATE.LT.LCRATE) GOTO 111
          IF( (POINT.LT.(NWORDS-NTRAILER)).AND.
     &      (IQ(LCAD+POINT+1).EQ.HEADER_LEN) ) GOTO 111
C
C ****  No more Crates in this bank
C
          IER = -5
          GOTO 999
        END IF
        CALL ERRMSG('GTCAD','GTCAD_HEADER',
     &          'CRATE NOT FOUND','W')
        IER = -6
      END IF
  999 CAD_POINTER = POINT
      RETURN
      END
