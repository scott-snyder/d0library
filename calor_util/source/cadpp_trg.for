      SUBROUTINE CADPP_TRG(IDET,ICRATE,IADC,IBLS,IROTOW,IDEPTH
     &,IBOX,IPPAT,ICOND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To find the pulser configuration that will
C-                              pulse a given TRIGGER channel
C-                              (using CALIB addressing convention)
C-
C-   Inputs  : IDET    detector config as defined in CAL_PULSE_LIST.PARAMS
C-             ICRATE  adc crate
C-             IADC    adc board
C-             IBLS    bls board
C-             IROTOW  read out tower in the BLS board      (CALIB notation)
C-             IDEPTH  depth in a read out tower            (CALIB notation)
C-   Outputs : IBOX  pulser number [also preamp box number] 0 to 11
C-             IPPAT pulser pattern number  0 to 31
C-   Controls: ICOND from the return of a call to CADPR
C-
C-   Created  19-OCT-1993   Jan Guida       Similar to CADPP
C-      A trigger channel can be pulsed by multiple pulser patterns.
C-      This routine will only return the first pulser pattern found for that
C-      channel.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDET, ICRATE, IADC, IBLS, IROTOW, IDEPTH  ! ADC SCHEME
      INTEGER IBOX, IPPAT                               ! PULSER DESCRIPTION
      INTEGER ICOND
      INTEGER DET,CRATE,ADC,BLS,TWR,DEP,BOX,PPAT,IER
      INTEGER IBRD,LM,JBRD
      LOGICAL LCRATE_EC
C----------------------------------------------------------------------
C
      ICOND = 0
      DET = IDET
      LCRATE_EC = ICRATE/10.GT.1
      CRATE = ICRATE
      ADC = IADC
      BLS = IBLS
      IBRD = IADC*8 + IBLS
      JBRD = MOD(IBRD,6)
      LM = MOD(IBRD,12)
      IF (LCRATE_EC .AND. LM.GE.10) THEN  ! LM card
        TWR = IROTOW
        IF (IDEPTH.EQ.12) THEN            ! EM
          DEP = 0
        ELSE IF (IDEPTH.EQ.13) THEN       ! HD
          DEP = 7
        ELSE                              ! ERROR
          ICOND = -1
          GO TO 999
        ENDIF
      ELSE                                ! Not LM card
        TWR = 0
        IF (IDEPTH.EQ.12) THEN            ! EM
          DEP = IROTOW
        ELSE IF (IDEPTH.EQ.13) THEN       ! HD
          IF (.NOT.LCRATE_EC .AND. JBRD.EQ.5) THEN   ! Merge region
            TWR = 2
            DEP = 4 + IROTOW
          ELSE
            DEP = 8 + IROTOW
            IF (IROTOW.EQ.3) DEP = 7
          ENDIF
        ELSE                              ! ERROR
          ICOND = -1
          GO TO 999
        ENDIF
      ENDIF
C
      CALL CADPP(DET,CRATE,ADC,BLS,TWR,DEP,BOX,PPAT,IER)
C
      IF (IER.EQ.0) THEN
        IBOX = BOX
        IPPAT = PPAT
      ELSE
C
C ****  Try again with another channel in that trigger tower
C
C
        DEP = DEP + 4
        IF (IDEPTH.EQ.12 .AND. DEP.GT.6) GO TO 999
        IF (IDEPTH.EQ.13 .AND. DEP.GT.11) GO TO 999
C
        IER = 0
        CALL CADPP(DET,CRATE,ADC,BLS,TWR,DEP,BOX,PPAT,IER)
C
        IF (IER.EQ.0) THEN
          IBOX = BOX
          IPPAT = PPAT
        ELSE
          ICOND = IER
        ENDIF
      ENDIF
C
  999 RETURN
      END
