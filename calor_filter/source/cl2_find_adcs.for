      SUBROUTINE CL2_FIND_ADCS( NCAD,NCH,L2CAD,CALVSN,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : build a list of pointers to each
C-      CAL ADC in a CAD bank and do crude format checks
C-              based on GTCAD_TOTAL
C-      If an error is found, the flag for bad cal data will be set
C-
C-   Inputs  : NCAD         [I]  1 for CAD1, 2 for CAD2
C-              uses /L2CRATE/
C-   Outputs : NCH          [I]  Number of Nonzero'd Channels in this CAD bank
C-             L2CAD        [I]  Link to requested CAD bank
C-             CALVSN       [I]  CAL version number of CAD bank
C-             IER          [I]  Error code - 0=ok,
C-                              (-2 --- Bad total word count in trailer)
C-                              -3 --- NCAD wrong
C-                              -4 --- No CAD bank (and should be one)
C-                              -5 --- found only a trailer in this CAD
C-                              (-6 --- CRATE not found in this bank)
C-                              -7 --- Bad total word count in CARD HEADER
C-                              -8 --- Illegal crate number
C-                              -9 --- Illegal number of ADC cards
C-                              -10 -- Bad Header Length
C-                              -11 -- Wandered out of CAD bank
C-                              -12 -- fatal Calorimeter controller error
C-                              -13 -- Illegal header length
C-                              ( ) means not currently checked for
C-            /ADC_LOCATION/ADC_POINT(I,J),CRATE_LOCATION(J),CRATE_CAD(J) where
C-                      I = 0...11      ADC card number
C-                      J = 1...12      CL2 ADC crate number
C-                                      (see /CL2CRATE/)
C- The pointer points to the 0th data word of the object
C- CRATE_CAD = 0 if crate never found, 1 if on CAD1, 2 if on CAD2
C-
C-   Controls: ALLOW_MISSING_CAD  if .TRUE. IER=-4 is not produced.
C-             version word       
C-
C-   Created   6-JAN-1991   James T. Linnemann
C-   Updated  23-APR-1991   James T. Linnemann
C-   Updated   6-MAY-1992   James T. Linnemann  protection against bad data
C-   Updated   1-AUG-1992   James T. Linnemann  set _BAD flag; new trailers 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER NCAD,NCH,IER
      INTEGER L2CAD,GZCAD1,GZCAD2,POINT
      INTEGER ICRATE,MAXCARD,CRATE
      INTEGER J,NCHAN
      INTEGER CONTROL_WORD,HEADER_LEN,VERSION
      INTEGER CALVSN,D0VSN,SFTVSN       ! version numbers
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$INC:ADC_LOCATION.INC'
      INCLUDE 'D0$INC:CL2CRATE.INC'
      CHARACTER*80 MSG_STRING
      CHARACTER*20 KILLED
      INTEGER BANKLEN,TRAILER
      INTEGER SIXBITS
      PARAMETER( SIXBITS = 63 )
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      BYTE CONTROL_BYTE(4)
      EQUIVALENCE(CONTROL_WORD,CONTROL_BYTE)
      BYTE VERSION_BYTE(4)
      EQUIVALENCE(VERSION,VERSION_BYTE)
      INTEGER STATUS_VTX,ERROR_BIT
      integer*2 kill_mask
      INTEGER*2 STATUS_2(2),STATUS
      EQUIVALENCE(STATUS_2,STATUS_VTX)
      PARAMETER( ERROR_BIT  = 15 )
      PARAMETER( KILL_MASK = (2**14-1)-(2**7-1) ) !bits 7-13
      LOGICAL ALLOW_MISSING_CAD,OK,SET_BAD_CAL_FLAG
      SAVE ALLOW_MISSING_CAD
      DATA ALLOW_MISSING_CAD/.TRUE./
C------------------------------------------------------------------------
      IER = 0
      NCH = 0
      IF ( NCAD .LT. 1 .OR. NCAD .GT. 2 ) THEN
        IER = -3
        CALL ERRMSG('CAL_BAD_CAD','CL2_FIND_ADCS',
     &      ' UNEXPECTED CAD BANK NUMBER REQUESTED','E')
        GOTO 999
      ENDIF
C
      L2CAD = 0
      IF ( NCAD.EQ.1 ) L2CAD = GZCAD1()
      IF ( NCAD.EQ.2 ) L2CAD = GZCAD2()
      IF ( L2CAD .LE. 0 ) THEN
        IF (ALLOW_MISSING_CAD) THEN
          IER = 0
        ELSE
          IER = - 4
          CALL ERRMSG('CAL_NO_CAD','CL2_FIND_ADCS',
     &      ' CAD BANK NOT FOUND','E')
        ENDIF
        GOTO 999
      ENDIF
      POINT = 0
C
C...examine first header for validity
C
C...find out data version to see if special handling needed
      VERSION = IQ(L2CAD+4+POINT)
      D0VSN  =  VERSION_BYTE(BYTE4)       ! Most significant 8 bits
C      CALVSN =  VERSION_BYTE(BYTE3)       ! 2nd most significant 8 bits
      SFTVSN =  VERSION_BYTE(BYTE1)       ! Least significant 16 bits
C    D0VSN is equal to        CALVSN is
C        0 for D0 data          1 for CC quadrant test
C       64 for TB data          2 for 5000 channel test
C       32 for MC data          3 for cosmic ray data
C       96 for MC-TB data
C   (MC has bit 5 on in D0VSN)
C
C...check for missing data
C
C...if this is MC and format good, trailer is shorter
      TRAILER = 16              !data trailer length
      IF ( BTEST(D0VSN,5) ) THEN      !check for MC
        IF ( SFTVSN.LT.4 ) TRAILER = 1 ! old MC trailer short
      ENDIF
      BANKLEN = IQ(L2CAD-1)
      IF (BANKLEN.LE.TRAILER) THEN                   !TRAILER ONLY
        WRITE(MSG_STRING,110)NCAD
  110   FORMAT('TRAILER ONLY ON CAD',I1)
        CALL ERRMSG('CAL_TRAILER_ONLY','CL2_FIND_ADCS',MSG_STRING,'I')
        IER = -5
        GOTO 999
      ENDIF
C
C...there is probably really data here; is the header length valid?
      HEADER_LEN   = IQ(L2CAD+1+POINT)
      IF (HEADER_LEN.NE.4) THEN
        WRITE(MSG_STRING,115) HEADER_LEN,NCAD
  115   FORMAT('HEADER LEN ',I11,' on CAD',I1,' 1st crate')
        CALL ERRMSG('CAL_HDR_BAD','CL2_FIND_ADCS', MSG_STRING,'W')
        IER = -10
        GO TO 999
      ENDIF
C
C...at this point claim header length, trailer length, and version known + good
C
C...loop over crates
  112 CONTROL_WORD = IQ(L2CAD+3+POINT)
      ICRATE =  CONTROL_BYTE(BYTE4)
      MAXCARD  =  CONTROL_BYTE(BYTE3)
      CRATE = IAND(ICRATE,SIXBITS)      ! do in 2 steps to help debug
      CRATE = L2CRATE(CRATE)
      IF (CRATE.LE.0) THEN
        WRITE(MSG_STRING,120) ICRATE,NCAD
  120   FORMAT('BAD CRATE # ',I6,' ON CAD',I1)
        CALL ERRMSG('CAL_CRATE_BAD','CL2_FIND_ADCS', MSG_STRING,'W')
        IER = -8
        GOTO 999
      ENDIF
      STATUS_VTX = IQ(L2CAD+5+POINT)
      STATUS = STATUS_2(WORD2)
      IF( BTEST(STATUS,ERROR_BIT)) THEN
C
C...this event will PROBABLY be rejected: I refuse to convert this CAD bank
        KILLED = ' '
        IF (IAND(STATUS,KILL_MASK).NE.0) KILLED = ': Reject event'
        WRITE(MSG_STRING,125)STATUS,ICRATE,KILLED
  125   FORMAT(' data error: status word = ',Z4.4,' Crate',I3,1X,A)
        CALL ERRMSG('CAL_STATUS_BAD','CL2_FIND_ADCS',MSG_STRING,'I')
        IF (KILLED(1:1).NE.' ') THEN
          IER = -12   !only kill for selected bits (per D. Schamberger)
          GOTO 999
        ENDIF
      ENDIF
      IF ((MAXCARD.LT.0).OR.(MAXCARD.GT.(NADCC-1))) THEN
        WRITE(MSG_STRING,130) MAXCARD,ICRATE
  130   FORMAT(' MAX # CHAN = ',I11, ' CRATE ',I2)
        CALL ERRMSG('CAL_CHAN_BAD','CL2_FIND_ADCS', MSG_STRING,'W')
        IER = -9
        GOTO 999
      ENDIF
      CRATE_POINT(CRATE) = POINT
      CRATE_CAD(CRATE) = NCAD
      POINT = POINT + HEADER_LEN + 2
      IF (POINT.GT.BANKLEN) THEN
        WRITE(MSG_STRING,135) ICRATE,-1
  135   FORMAT('WANDERED OUT OF CAD: CRATE',I2,' CARD ',I2)
        CALL ERRMSG('CAL_PTR_BAD','CL2_FIND_ADCS', MSG_STRING,'W')
        IER = -11
        GO TO 999
      ENDIF
      DO J=0,MAXCARD
        NCHAN = IQ(L2CAD+POINT)
        IF((NCHAN.LT.0).OR.(NCHAN.GT.384)) THEN
          WRITE(MSG_STRING,137) NCHAN,J,ICRATE
  137     FORMAT(' # CHAN = ',I11,' CARD ',I2, ' CRATE ',I2)
          CALL ERRMSG('CAL_NUM_CHAN_BAD','CL2_FIND_ADCS',MSG_STRING,'W')
          IER = -7
          GOTO 999
        END IF
        ADC_POINT(J,CRATE) = POINT
        POINT = POINT + NCHAN + 1
        IF (POINT.GT.BANKLEN) THEN
          WRITE(MSG_STRING,135) ICRATE,J
          CALL ERRMSG('CAL_PTR_BAD','CL2_FIND_ADCS', MSG_STRING,'W')
          IER = -11
          GO TO 999
        ENDIF
        NCH = NCH + NCHAN
      ENDDO
C
C ****  DON'T CHECK TOTAL WORD COUNT MATCH  (MC data had problems)
C
C
C ****  DON'T CHECK CHECKSUM UNTIL WE KNOW WHAT IT IS
C
      POINT = POINT + 3               ! skip over crate trailer
C
C...see if have hit the trailer.
      IF (POINT.GE.BANKLEN-TRAILER) THEN
        IF (POINT.EQ.BANKLEN-TRAILER) THEN
C
C...normal termination
          GO TO 999
        ELSE
          WRITE(MSG_STRING,145)
  145     FORMAT('POINTER WANDERED INTO TRAILER')
          CALL ERRMSG('CAL_PTR_BAD','CL2_FIND_ADCS', MSG_STRING,'W')
          IER = -11
          GO TO 999
        ENDIF
      ENDIF
C
C ****  Check for another crate
C
      IF(IQ(L2CAD+POINT+1).EQ.HEADER_LEN) THEN
        GOTO 112            !not in the trailer and looks like a real header
      ELSE
        WRITE(MSG_STRING,140) IQ(L2CAD+POINT+1),ICRATE
  140   FORMAT(' HEADER_LEN =',I10,' after Crate',I5)
        CALL ERRMSG('CAL_BAD_HDR','CL2_FIND_ADCS',MSG_STRING,'W')
        IER = -13
        GO TO 999
      ENDIF
  999 CONTINUE
      IF (IER.NE.0) OK = SET_BAD_CAL_FLAG()   !note that I am killing the event
      RETURN
      END
