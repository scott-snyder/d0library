      SUBROUTINE MUGLOP_FIT(LMUON,P,DP) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find momentum for a muon track
C-
C-   Inputs  : LMUON  muon track bank address
C-   MUON bank has track parameters from global fit and
C-   (through MUOT) the initial estimate of P.
C- 
C-   Outputs : P +- DP    momentum and error
C-   Controls: 
C-
C-   Created  12-JUN-1992   Daria Zieminska   
C----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL SATRGLO
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMUOT.LINK'
      INCLUDE 'D0$LINKS:IZSTTH.LINK'
      INCLUDE 'D0$LINKS:IZSACL.LINK'
C----------------------------------------------------------------------
      INTEGER  LMUON,LMUOT,I
      REAL     P,DP,FA,FB,ELFE,ELCAL,ELOSS 
      REAL     PSAVE,SATRGLO_SAVE,PT(3),VEC(3),DIF,DIFVEC,STEP
C
      LMUOT=LQ(LMUON-11)
      CALL SATRGLO_SET(LMUON)
C
C  Initial estimate of momentum  
C
      P    = Q(LMUOT+23)
      STEP=5.
C
C  Subtract energy loss
C
        IF(P.GT.0.) THEN
          P=P- 4.0     ! ENERGY LOSS
        ELSE
          P=P+ 4.0
        ENDIF
      P=-P
      PSAVE = P
      I    = 0
      IF (P.LT.0.0)                           THEN
        FA = -500.
        FB = -0.1
      ELSE
        FA =  0.1
        FB = 500.
      END IF
    2 CONTINUE
      I    = I+1
      CALL MINVAR(P,DIF,0.01*P,0.01,STEP,500,FA,FB,SATRGLO)
      IF (ABS(P-FA).LE.0.02*ABS(P).OR.
     +    ABS(P-FB).LE.0.02*ABS(P))           THEN
        IF (ABS(P-FA).LE.0.02*ABS(P)) FA = FA-0.5*ABS(P)
        IF (ABS(P-FB).LE.0.02*ABS(P)) FB = FB+0.5*ABS(P)
        IF (I.LE.4)                           GO TO 2
        P = PSAVE
      END IF
      DIFVEC=SATRGLO_SAVE(P,PT,VEC)
C
C  Add energy loss
C
      CALL MUELOS(IQ(LMUOT+3),ABS(P),Q(LMUOT+11),Q(LMUOT+12),
     &   Q(LMUOT+13),Q(LMUOT+14),Q(LMUOT+15),Q(LMUOT+16),
     &   Q(LMUOT+17),Q(LMUOT+18),Q(LMUOT+19),ELFE,ELCAL)
        IF (ELFE.GT.20.) ELFE=20.
        IF (ELCAL.GT.20.) ELCAL=20.
        ELOSS=ELFE/2.+ELCAL
        IF(P.GT.0.) THEN
          P=P+ ELOSS     ! ENERGY LOSS
        ELSE
          P=P- ELOSS
        ENDIF
        P=-P
        P=ABS(P)   
        DP=0.2*P ! to be changed
  999 RETURN
      END
