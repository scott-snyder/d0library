      FUNCTION TB90_INSPILL_PEDS ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill an array for calculating the means and sigmas
C-                              of the inspill pedestals
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-OCT-1990   Jan Guida
C-   Updated   8-JAN-1991   Jan Guida  Skip event, if ped file hasn't been read
C-                                      from the data base yet. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL TB90_INSPILL_PEDS
      INCLUDE 'D0$PARAMS:PRTPDG.DEF'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAD1.LINK'
      INCLUDE 'D0$LINKS:IZCRCP.LINK'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      CHARACTER*32 SRCPNAME
      LOGICAL LFIRST,LFIRST2,LFIRST3
      REAL INPED(2,4608)
      REAL WINDOW
      REAL AVR1,SIG1,XEXTR,NSIG1
      REAL XAV,LOWEST_BIN,VAL(2*384)
      INTEGER TRG,GZHEAD,GZCPDH,LCPDH,HEAD(NHEAD)
      INTEGER IADDR,ADDR,PH,CHAN,NEVT(4608),IRUN
      INTEGER ICRATE,CRATE,ADC,BLS,TOW,DEP,SCL,NEGLIM
      INTEGER MINVAL(4608),MAXVAL(4608)
      INTEGER MAX_BIN,INTEGER IBIN
      INTEGER NCHAN,NCH,ICH,I,IER
      INTEGER NH,XL,XU,IBIN
      INTEGER LD,LOC,LDLOC,ICARD,CNTRL,NCARD
      INTEGER*2 HLFWRD(2,NNQ)
      EQUIVALENCE (HLFWRD(1,1),IQ(1))
      BYTE CONTROL_BYTE(4)
      EQUIVALENCE(CNTRL,CONTROL_BYTE)
C
      COMMON /INSPILL/INPED,NEVT,IRUN
      DATA ICRATE/7/
      DATA LFIRST,LFIRST2,LFIRST3/2*.TRUE.,.FALSE./
C----------------------------------------------------------------------
      TRG   = iand(IQ(LHEAD+11), 65535)
      TB90_INSPILL_PEDS =  .TRUE.
C
      IF (LFIRST) THEN
        LCPDH = GZCPDH()
        IF (LCPDH.LE.0) GO TO 999       ! Skip event if no ped banks yet
C
        LFIRST = .FALSE.
        CALL vZERO_i(MINVAL,4608)
        CALL vZERO_i(MAXVAL,4608)
        CALL UZERO(INPED,1,2*4608)
        CALL vZERO_i(NEVT,4608)
C
        IRUN = IQ(LHEAD+6)
        WRITE(SRCPNAME,15)ICRATE
   15   FORMAT('STPC/CAL',I2.2)
        CALL EZNAME(SRCPNAME,LCPDH,IZCRCP)
        CALL EZPICK(SRCPNAME)
        CALL EZGET_i('MAX_BIN',MAX_BIN,IER)
        CALL EZGET('LOWEST_BIN',LOWEST_BIN,IER)
        CALL EZGET('WINDOW',WINDOW,IER)
        CALL EZGET('NSIG1',NSIG1,IER)
        CALL EZGET('XEXTR',XEXTR,IER)
        CALL EZRSET
      ENDIF
C
      IF (TRG .NE. 4) GO TO 999         ! Not an inspill pedestal event
      LD = LQ(LHEAD-IZCAD1)
      LOC = IQ(LD+1) + 2                ! Header length
      CNTRL = IQ(LD+3)                        ! Controller word
      NCARD  =  CONTROL_BYTE(BYTE3)
      DO ICARD=0,NCARD
        IF (LFIRST2) CALL GT_PED_GNS(1,0,ICRATE,ICARD,HEAD,VAL)
        LDLOC=LD+LOC
        NCH=IQ(LDLOC)
        DO ICH = 1 , NCH
          IADDR =  HLFWRD(WORD2,LDLOC+ICH)
          PH = HLFWRD(WORD1,LDLOC+ICH)
          ADDR = ISHFT(IADDR,16)
          CALL CADUPK(ICRATE,ADDR,CRATE,ADC,BLS,TOW,DEP,SCL,NEGLIM)
          CHAN = ADC*384 + BLS*48 + TOW*12 + DEP + 1
C
          IF (LFIRST2 .AND. MAXVAL(CHAN).EQ.0) THEN
            LFIRST3 = .TRUE.
            IF (SCL.EQ.0) THEN
              I = BLS*48+TOW*12+DEP+1
              AVR1=VAL(2*I-1)
              SIG1=VAL(2*I)
            ELSE
              AVR1 = 0.
            ENDIF
            IF (AVR1.LE.0.) THEN
              AVR1 = 260.
              SIG1 = 15.
            ENDIF
            IF (SIG1.LE.0.) SIG1 = 1.
            XL=IFIX(AVR1-XEXTR-NSIG1*SIG1) - 0.5
            XU=IFIX(AVR1+XEXTR+NSIG1*SIG1) + 0.5
C
            IF(XL.LT.LOWEST_BIN) XL=LOWEST_BIN    ! lowest value allowed
            NH = XU - XL
            IF(NH.GT.MAX_BIN)THEN
C Too many bins
              IBIN = 1 + IFIX((XU-XL)/FLOAT(MAX_BIN))       !Integer bin size
              XAV = 0.5*(XU+XL)
              XL = IFIX(XAV-0.5*FLOAT(MAX_BIN*IBIN))-0.5
              IF(XL.LT.LOWEST_BIN) XL=LOWEST_BIN            ! lowest allowed
              XU = XL + MAX_BIN*IBIN
            ENDIF
            MINVAL(CHAN) = XL
            MAXVAL(CHAN) = XU
          ENDIF
C
          IF(SCL.EQ.0)THEN
            IF (PH.GE.MINVAL(CHAN) .AND. PH.LE.MAXVAL(CHAN)) THEN
              INPED(1,CHAN) = INPED(1,CHAN) + PH
              INPED(2,CHAN) = INPED(2,CHAN) + PH*PH
              NEVT(CHAN) = NEVT(CHAN) + 1
            ENDIF
          ENDIF
        ENDDO                             ! CHANNELS
        LOC=LOC+NCH+1
      ENDDO                             ! ADC CARDS
      IF (LFIRST3) LFIRST2 = .FALSE.
  999 RETURN
      END
