      SUBROUTINE GTCAD_TOTAL ( NCAD,NCH,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns total number of ADC channels in CAD bank
C-   given which CAD bank (NCAD=1,2)
C-
C-   Inputs  : NCAD         [I]  1 for CAD1, 2 for CAD2
C-   Outputs : NCH          [I]  number of ADC channels in this bank
C-             IER          [I]  Error code - 0=ok,
C-                              -2 --- Bad total word count in trailer
C-                              -3 --- NCAD wrong
C-                              -4 --- No CAD bank.
C-                              -5 --- Expecting header length but not found
C-                              -6 --- CRATE not found in this bank
C-                              -7 --- Bad total word count in CARD HEADER
C-                              -8 --- Bad controller word in CARD HEADER
C-                              -9 --- ZEBRA pointer out of range
C-   Controls: NONE
C-
C-   Created 19-FEB-1990   Chip Stewart, W.G.D.Dharmaratna.
C-   Updated  8-APR-1991   Joan Guida, C. Stewart - Added IER = -8 for 
C-                         bad controller word and modified bad word 
C-                         count error.
C-   Updated   2-MAY-1992   Joan Guida   Modify error -5 added -9 and modified
C-                                       ERRMSG calls
C-   Updated   3-NOV-1992   Chip Stewart ERRMSG for zero length CARDS
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER MSG_STRING*80
      INTEGER LCAD,GZCAD1,GZCAD2,POINT,IWORD,IADDR
      INTEGER NCAD,CRATE,CHANNEL,MASK,NCAD_PREV,NWORD_PREV
      INTEGER IER
      INTEGER ICHANNEL,ICARD,ICRATE,IDTYPE
      INTEGER NCHANNEL,NCARD,NCRATE,NV,IBITS
      INTEGER I,J,K
      INTEGER CONTROL_WORD,HEADER_LEN,BANKLEN
      INTEGER CHECKSUM,NWORD,NCHAN,MCHAN,LCHAN
      INTEGER NCH
      INTEGER VSN,D0VSN,CALVSN
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_HEX_NO.PARAMS'
      INCLUDE 'D0$LINKS:IZCAD1.LINK'
      INCLUDE 'D0$LINKS:IZCAD2.LINK'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      LOGICAL FIRST,BAD,BTEST,MC
      SAVE POINT,ICRATE,CHANNEL,ICHANNEL,NCHANNEL,NCARD
      SAVE NCAD_PREV
      BYTE CONTROL_BYTE(4)
      EQUIVALENCE(CONTROL_WORD,CONTROL_BYTE)
C------------------------------------------------------------------------
      IER = 0
      NCH = 0
      NWORD_PREV = 0
      IF ( NCAD .LT. 1 .OR. NCAD .GT. 2 ) THEN
        IER = -3
        GOTO 999
      ENDIF
C
      IF ( NCAD.EQ.1 ) LCAD = GZCAD1()
      IF ( NCAD.EQ.2 ) LCAD = GZCAD2()
      IF ( LCAD .LE. 0 ) THEN
        IER = - 4
        GOTO 999
      ENDIF
C
      POINT = 0
      BANKLEN = IQ(LCAD-1)
      IF (BANKLEN.EQ.16) THEN                   !TRAILER ONLY
        WRITE(MSG_STRING,110)NCAD
  110   FORMAT('TRAILER ONLY ON CAD',I1)
        CALL ERRMSG('GTCAD','GTCAD_TOTAL',MSG_STRING,'W')
        IER = -4
        GOTO 999
      ENDIF
C
  112 HEADER_LEN   = IQ(LCAD+1+POINT)
      IF (HEADER_LEN.NE.4) THEN
        WRITE(MSG_STRING,115) HEADER_LEN,NCAD
  115   FORMAT('HEADER LENGTH BAD ',I9,' ON CAD',I1)
        CALL ERRMSG('GTCAD','GTCAD_TOTAL',MSG_STRING,'W')
        IER = -5
        GOTO 999
      ENDIF
C
      CONTROL_WORD = IQ(LCAD+3+POINT)
      NV           = IQ(LCAD+4+POINT)
      ICRATE =  CONTROL_BYTE(BYTE4)
      NCARD  =  CONTROL_BYTE(BYTE3)
      IDTYPE =  CONTROL_BYTE(BYTE2)
      MC = BTEST(NV,29)      ! Monte Carlo generated CAD bank - 0 CARD LEN OK
      IF (IDTYPE.LT.0) THEN
        WRITE(MSG_STRING,116) CONTROL_WORD,ICRATE
  116   FORMAT('CONTROLLER WORD BAD ',Z8,' FOR CRATE ',I2)
        CALL ERRMSG('GTCAD','GTCAD_TOTAL',MSG_STRING,'W')
        IER = -8
        GOTO 999
      ENDIF
C
      POINT = POINT + HEADER_LEN + 2
      LCHAN = 1
      MCHAN = 0
      DO 13 J=0,NCARD
        NCHAN = IQ(LCAD+POINT)
        IF( NCHAN.LT.0) THEN
        WRITE(MSG_STRING,117) NCHAN,J,ICRATE
  117   FORMAT('INCORRECT NUMBER OF CHANNELS = ',I9,' IN CARD ',I2,
     &    ' FOR CRATE ',I2)
          CALL ERRMSG('GTCAD','GTCAD_TOTAL',MSG_STRING,'W')
          IER = -7
          GOTO 999
        ELSE IF(.NOT.MC.AND.(NCHAN.EQ.0)) THEN
          WRITE(MSG_STRING,118) J,ICRATE
  118     FORMAT('NUMBER OF CHANNELS = 0 IN CARD ',I2,' FOR CRATE ',I2)
          CALL ERRMSG('GTCAD','GTCAD_TOTAL',MSG_STRING,'W')
          IF(LCHAN.EQ.0) MCHAN = MCHAN + 1
        ENDIF
        LCHAN = NCHAN
        IF( BTEST(IDTYPE,2)) THEN                 !DUAL READOUT
          IF( NCHAN.GT.768) THEN
            WRITE(MSG_STRING,117) NCHAN,J,ICRATE
            CALL ERRMSG('GTCAD','GTCAD_TOTAL',MSG_STRING,'W')
            IER = -7
            GOTO 999
          ENDIF
        ELSE
          IF( NCHAN.GT.384) THEN
            WRITE(MSG_STRING,117) NCHAN,J,ICRATE
            CALL ERRMSG('GTCAD','GTCAD_TOTAL',MSG_STRING,'W')
            IER = -7
            GOTO 999
          END IF
        END IF
        POINT = POINT + NCHAN + 1               !GO TO NEXT ADC CARD
        NCH = NCH + NCHAN
        IF (POINT.LT.0 .OR. POINT.GT.IQ(LCAD-1)) THEN
          WRITE(MSG_STRING,129) ICRATE,J
  129     FORMAT('BAD ZEBRA POINTER FOR CRATE',I2,' AT CARD ',I2)
          CALL ERRMSG('GTCAD','GTCAD_TOTAL',MSG_STRING,'W')
          IER=-9
          GOTO 999
        ENDIF
   13 CONTINUE
      IF((.NOT.MC).and.(MCHAN.GT.0)) THEN
        WRITE(MSG_STRING,139)MCHAN+1, ICRATE
  139   FORMAT(I5,' CARDS = 0 IN CRATE ',I2)
        CALL ERRMSG('GTCAD','GTCAD_TOTAL',MSG_STRING,'W')
      END IF
C
C ****  Check for another crate
C
      IF ( HEADER_LEN.GT.3) THEN    ! CRATE TRAILER
C
C ****  CHECK CRATE TRAILER TOTAL WORD COUNT MATCH
C
        NWORD = IQ(LCAD+POINT)
        IF (BTEST(NV,29) .AND. IBITS(NV,0,16).LT.2) THEN
          BAD = NWORD.NE.(POINT+3)
        ELSE 
          BAD = NWORD.NE.(POINT+3-NWORD_PREV)
          NWORD_PREV = POINT+3
        END IF
        IWORD = 1
        IF (BAD) THEN
          WRITE(MSG_STRING,130) ICRATE
  130   FORMAT('TRAILER WORD COUNT MISMATCH FOR CRATE ',I2)
          CALL ERRMSG('GTCAD','GTCAD_TOTAL',MSG_STRING,'W')
          IER = -2
          GOTO 999
        END IF
C
C ****  DON'T CHECK CHECKSUM UNTIL WE KNOW WHAT IT IS
C
        POINT = POINT + 3
      ELSE
        POINT = POINT - 1
      END IF

      IF (POINT.LT.0 .OR. POINT.GT.IQ(LCAD-1)) THEN
        WRITE(MSG_STRING,119) ICRATE
  119   FORMAT('BAD ZEBRA POINTER AFTER CRATE',I2)
        CALL ERRMSG('GTCAD','GTCAD_TOTAL',MSG_STRING,'W')
        IER=-9
        GOTO 999
      ENDIF

      IF( (POINT.LT.(BANKLEN-NTRAILER)).AND.
     &  (IQ(LCAD+POINT+1).EQ.HEADER_LEN) )GOTO 112
  999 RETURN
      END
