      SUBROUTINE DIMUON_FAKE(NISAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : reconstruct decays X  ->mu+mu-
C-   using ISAL banks
C-
C-   Inputs  : ISAL banks,I1,I2
C-   Outputs : NISAL,FOUNDMU,OK,P1,P2,THE1,THE2
C-             PHI1,PHI2
C-   Controls:
C-
C-   Created  20-MAR-1992   Daria Zieminska,C.Murphy
C-   Updated  24-MAR-1992   c.r.murphy - was originally a function 
C-   subprogram,changed to subroutine.    
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:VEEKIN.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZISAL.LINK/LIST'
      INCLUDE 'D0$INC:PI.DEF/LIST'
      REAL P1,P2,E1,E2,P1COMP(3),P2COMP(3),INVMASS,MASS,
     +     THE1,THE2,PHI1,PHI2,PT1_ISAL,PT2_ISAL,PT,
     +     PT_ISALMAX,PT_ISALMIN,AMAX1,AMIN1
      INTEGER NISAL,NLEP,LISAE,GZISAE,LISAL,NZBANK
      INTEGER I1,I2
      LOGICAL FOUNDMUS,PTOK,THEOK,OK
      INTEGER ICALL,IER,RUN,ID,RUNSAV,IDSAV
      INTEGER DILBOSON_ID,LEPTONS
      REAL PT_MIN1,PT_MIN2,PT_MIN
      REAL MUON,PDG_MASS
      INTEGER PRUNIT,USUNIT
      SAVE RUNSAV,IDSAV
      DATA RUNSAV,IDSAV/-1,-1/
      SAVE ICALL
      DATA ICALL/0/
C------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('DILBOSON_RCP')
        CALL EZGET('DILBOSON_ID',DILBOSON_ID,IER)
        CALL EZGET('LEPTONS',LEPTONS,IER)
        CALL EZGET('PT_MIN1',PT_MIN1,IER)
        CALL EZGET('PT_MIN2',PT_MIN2,IER)
        CALL EZGET('PT_MIN',PT_MIN,IER)
        CALL EZRSET
        MUON=0.105658
        IF (DILBOSON_ID.EQ.441) PDG_MASS=3.09693
        IF (DILBOSON_ID.EQ.90)  PDG_MASS=91.17
        ICALL=1
        PRUNIT=USUNIT()
      END IF
C
      LISAE=GZISAE()
      IF (LISAE.EQ.0) GO TO 999
      LISAL=LQ(LISAE-IZISAL)
      NISAL=NZBANK(IXCOM,LISAL)
      RETURN
C
C****     LOOP OVER ISAL BANKS
C
      ENTRY FAKEMU1(I1,I2,FOUNDMUS,OK,P1,P2,THE1,THE2,PHI1,PHI2)
      CALL DILBOSON_ISA(I1,I2,P1,P1COMP,E1,P2,P2COMP,E2,
     +                 THE1,PHI1,THE2,PHI2,INVMASS,FOUNDMUS)
      RETURN
C
      ENTRY FAKEMU2(I1,I2,FOUNDMUS,OK,P1,P2,THE1,THE2,PHI1,PHI2)
      PTOK=.FALSE.
      THEOK=.FALSE.
      OK=.FALSE.
      PT1_ISAL=SQRT(P1COMP(1)**2+P1COMP(2)**2)
      PT2_ISAL=SQRT(P2COMP(1)**2+P2COMP(2)**2)
      PT=SQRT((P1COMP(1)+P2COMP(1))**2 +
     +                   (P1COMP(2)+P2COMP(2))**2)
      PT_ISALMAX=AMAX1(PT1_ISAL,PT2_ISAL)
      PT_ISALMIN=AMIN1(PT1_ISAL,PT2_ISAL)
      IF (PT_ISALMAX.GT.PT_MIN1.AND.PT_ISALMIN.GT.PT_MIN2) PTOK=.TRUE.
      IF (THE1.NE.0..AND.THE2.NE.0.) THEOK=.TRUE.
      IF (PTOK.AND.THEOK) OK=.TRUE.
      CALL VZERO(STR,40)
      CALL VZERO(ETR,40)
      IF (OK) THEN
        STR(1,1)=P1
        STR(1,2)=P2
        STR(4,1)=MUON
        STR(4,2)=MUON
        STR(4,3)=PDG_MASS
        STR(2,1)=THE1
        STR(3,1)=PHI1
        STR(2,2)=THE2
        STR(3,2)=PHI2
        ETR(1,1)=0.05*STR(1,1)
        ETR(1,2)=0.05*STR(1,2)
        ETR(2,1)=0.05*STR(2,1)
        ETR(3,1)=0.05*STR(3,1)
        ETR(2,2)=0.05*STR(2,2)
        ETR(3,2)=0.05*STR(3,2)
      ENDIF
C
  999 RETURN
      END
