

      SUBROUTINE CALTH(IETA,CENTH,DELTH,ARGSOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the theta and width of a software
C-                         tower
C-
C-   Inputs  : IETA  Index of desired tower
C-   Outputs : CENTH Central theta of tower
C-             DELTH Width of tower
C-   Controls:
C-
C-   Created  13-NOV-1988   Michael W. Peters
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER IETA,I,ARGSOK,AROKIN
      REAL CENTH,DELTH,TH1,TH2,CENETA,DELETA
      REAL CENTHL(NETAL),DELTHL(NETAL)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF(.NOT.FIRST) GO TO 500
      FIRST=.FALSE.
      DO 100 I=1,NETAL
        CALL CALETA(I,CENETA,DELETA,AROKIN)
        IF(AROKIN.NE.0) THEN
          ARGSOK=100
          GO TO 999
        ENDIF
        TH1=2.*ATAN(EXP(-(CENETA-DELETA/2.)))
        TH2=2.*ATAN(EXP(-(CENETA+DELETA/2.)))
        CENTHL(I)=(TH1+TH2)/2.
        DELTHL(I)=ABS(TH1-TH2)
  100 CONTINUE
  500 CONTINUE
      IF(IETA.EQ.0..OR.ABS(IETA).GT.NETAL) THEN
        ARGSOK=1
      ELSE
        CENTH=CENTHL(ABS(IETA))
        DELTH=DELTHL(ABS(IETA))
        IF(IETA.LT.0) CENTH=PI-CENTH
        ARGSOK=0
      ENDIF
  999 RETURN
      END
