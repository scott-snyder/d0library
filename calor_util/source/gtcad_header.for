      SUBROUTINE GTCAD_HEADER (ICAD,CRATE,HEADER_LEN,SYNCH,
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
C-                              -8 --- CRATE ID - header does not match trailer
C-   Controls:none
C-
C-   Created  19-FEB-1990   Chip Stewart, W.G.D.Dharmaratna.
C-   Updated  12-APR-1990   Jan Guida
C-   Updated   6-DEC-1990   Jan Guida  Modifications to read crates 8, 18, ...
C-                                         on CAD1 instead of CAD2  (CR run)
C-   Updated  15-APR-1992   Chip Stewart   - no ERRMSG at end of CAD
C-   Updated   6-NOV-1992   Jan Guida   Completely rewritten to search from
C-                                      bottom to top.  Add CADEND_POINTER.
C-                                      Put all old version stuff in
C-                                      GTCAD_HEADER_OLD
C-   Updated  18-DEC-1992   Jan Guida   If CRATE=0, use first crate
C-   Updated  11-MAY-1993   Jan Guida   Fix for TB - call old version
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER LCAD,GZCAD1,GZCAD2,POINT,IWORD,IADDR,IPOINT
      INTEGER ICAD,KCAD,NCAD,CRATE
      INTEGER IER

      INTEGER ICRATE
      INTEGER NCRATE,KRATE
      INTEGER CONTROL_WORD,HEADER_LEN,STATUS,VERSION,PULSER,SYNCH
      INTEGER VSN
      INTEGER NWORDS,CTRAILER
      INTEGER MCRATE,LCRATE,BANKLEN,FFFF
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CUNFLG.INC'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_HEX_NO.PARAMS'
      INCLUDE 'D0$LINKS:IZCAD1.LINK'
      INCLUDE 'D0$LINKS:IZCAD2.LINK'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      PARAMETER (CTRAILER = 4)       !number of words in crate trailer
      PARAMETER (FFFF     = 2**16-1) ! 'FFFF' 
      LOGICAL FIRST,BAD
      SAVE POINT,ICRATE
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
        IF ( LCAD .LE. 0 ) THEN
          IER = - 4
          CALL GTCAD_HEADER_OLD(ICAD,CRATE,HEADER_LEN,SYNCH,
     &      CONTROL_WORD,VERSION,STATUS,PULSER,IER)
C          GOTO 999
        ENDIF
      ENDIF
C
      HEADER_LEN   = IQ(LCAD+1)
      IF( HEADER_LEN.LE.0) THEN
        IER = -5
        GOTO 999
      END IF
C
      IF (HEADER_LEN.NE.4) THEN
        CALL GTCAD_HEADER_OLD(ICAD,CRATE,HEADER_LEN,SYNCH,
     &    CONTROL_WORD,VERSION,STATUS,PULSER,IER)
        GOTO 999
      ENDIF
      VSN = IQ(LCAD+4)
      D0VSN = VERSION_BYTE(BYTE4)
      CALVSN = VERSION_BYTE(BYTE2)
      SFTVSN = VERSION_BYTE(WORD1)
      IF (VSN.LE.0) THEN
        CALL GTCAD_HEADER_OLD(ICAD,CRATE,HEADER_LEN,SYNCH,
     &    CONTROL_WORD,VERSION,STATUS,PULSER,IER)
        GOTO 999
      ELSEIF (BTEST(D0VSN,6)) THEN  ! test beam
        CALL GTCAD_HEADER_OLD(ICAD,CRATE,HEADER_LEN,SYNCH,
     &    CONTROL_WORD,VERSION,STATUS,PULSER,IER)
        GOTO 999
      ELSEIF (BTEST(D0VSN,5) .AND. SFTVSN.LT.4) THEN  !very old MC format 
        CALL GTCAD_HEADER_OLD(ICAD,CRATE,HEADER_LEN,SYNCH,
     &    CONTROL_WORD,VERSION,STATUS,PULSER,IER)
        GOTO 999
      ENDIF
C
C ****  LOOK FOR CRATE IF CRATE>0
C
      POINT = 0
      MCRATE = 0
      LCRATE = -1
      CAD_POINTER = 0
      CADEND_POINTER = 0
      BANKLEN = IQ(LCAD-1)
      IF (BANKLEN.LE.NTRAILER) THEN  ! TRAILER ONLY
        IER = -7
        GOTO 999
      END IF
      IF(CRATE/10.LT.0 .OR. CRATE/10.GT.5)THEN
        IER = -6
        GOTO 999
      ENDIF
      IF(CRATE.LT.0 .AND. CRATE.GE. -6) LCRATE = - CRATE
      IPOINT = 0
      POINT = BANKLEN - NTRAILER + 1
      DO WHILE(POINT.LE.BANKLEN .AND. POINT.GT.(HEADER_LEN+CTRAILER))
        NWORDS = IQ(LCAD+POINT-CTRAILER)
        KRATE = IAND(IQ(LCAD+POINT-3),FFFF)
        MCRATE = MCRATE+1
        IF (CRATE.EQ.KRATE .OR. CRATE.EQ.0 .OR. LCRATE.EQ.MCRATE) THEN
          IPOINT = POINT - NWORDS - 1
          HEADER_LEN   = IQ(LCAD+IPOINT+1)
          CAD_POINTER = (IPOINT + 1) + HEADER_LEN + 1
          CADEND_POINTER = POINT - 1
          POINT = HEADER_LEN+CTRAILER
        ELSE
          POINT = POINT - NWORDS
        ENDIF
      ENDDO
      IF (POINT .LT.(HEADER_LEN+CTRAILER) .OR. IPOINT.LT.0) THEN
        IER = -6
        GO TO 999
      ENDIF
C
      HEADER_LENGTH = HEADER_LEN
      SYNCH        = IQ(LCAD+2+IPOINT)
      CONTROL_WORD = IQ(LCAD+3+IPOINT)
      CNTRLWRD     = CONTROL_WORD
      VERSION      = IQ(LCAD+4+IPOINT)
      VSN          = VERSION
      STATUS       =  IQ(LCAD+5+IPOINT)
      STATUS_VERTEX = STATUS
      PULSER = 0
      PLSWRD = 0
      ICRATE =  CONTROL_BYTE(BYTE4)
      IF (ICRATE.NE.CRATE .AND. CRATE.GT.0) THEN
        CALL ERRMSG('CADUPK','GTCAD_HEADER',
     &    ' Crate number in header does not match trailer','W')
        IER = -8
      ENDIF
      CRT    =  ICRATE
      NCARD  =  CONTROL_BYTE(BYTE3)
      D0VSN  =  VERSION_BYTE(BYTE4)
      CALVSN =  VERSION_BYTE(BYTE3)
      SFTVSN =  VERSION_WORD(WORD1)
C
  999 CONTINUE
      RETURN
      END
