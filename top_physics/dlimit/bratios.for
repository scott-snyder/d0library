      SUBROUTINE BRATIOS(TMASS,HMASS,TB,H,TAU)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GIVEN THE TOP MASS, HIGGS MASS AND TAN BETA
C-   ROUTINE RETURNS THE TOP BRANCHING RATIO INTO CHARGED HIGGS
C-   AND THE HIGGS BRANCHING RATIO INTO TAU
C-
C-   Inputs  : TMASS= TOP MASS
C-             HMASS = HIGGS MASS
C-             TB = TANBETA
C-   Outputs : H = TOP BRANCHING RATIO INTO HIGGS
C-             TAU = HIGGS BRANCHING RATIO INTO TAU
C-   Controls:
C-
C-   Created   9-JUN-1995   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    TMASS,WMASS,HMASS,BMASS,TB,H,TAU,TAUMASS,CMASS,CABIBBO
      REAL    PTAU,PC,SMASS
      REAL    M,M1,M2,PW,MW2,MB2,MT2,MH,MH2,PH
      REAL    CB,NUM,DEN,MTAU2,MS2,MC2
      INTEGER IER
      LOGICAL first
      SAVE first
      DATA first / .true. /
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL EZPICK('DLIMIT_RCP')
        CALL EZGET('WMASS',WMASS,IER)
        CALL EZGET('BMASS',BMASS,IER)
        CALL EZGET('TAUMASS',TAUMASS,IER)
        CALL EZGET('CMASS',CMASS,IER)
        CALL EZGET('SMASS',SMASS,IER)
        CALL EZGET('CABIBBO',CABIBBO,IER)
        CALL EZRSET
      ENDIF
C
      M= TMASS
      M1=WMASS
      M2=BMASS
      PW = SQRT((M**2-(M1+M2)**2)*(M**2-(M1-M2)**2))/(2.0*M)
C
      MW2 = WMASS*WMASS
      MB2=BMASS*BMASS
      MT2=TMASS*TMASS
      MH = HMASS
C HIGGS MASS
      MH2=MH*MH
      M1=MH
      PH = SQRT((M**2-(M1+M2)**2)*(M**2-(M1-M2)**2))/(2.0*M)
C
      CB = 1/TB
C TAN AND COT BETA
      NUM = PH*((MB2+MT2-MH2)*(MB2*TB*TB + MT2*CB*CB)+
     &      4.0*MB2*MT2*TB*CB)
C NUM = BR T TO H+B, DEN=BR T TO W+B
      DEN = PW*(MW2*(MT2+MB2-2.0*MW2) + (MT2-MB2)**2)
      H = NUM/(NUM+DEN)
C
C ****  NOW THE HIGGS DECAY TO TAU AND CSBAR
C
      M= HMASS
      M1=TAUMASS
      M2= 0
      PTAU = SQRT((M**2-(M1+M2)**2)*(M**2-(M1-M2)**2))/(2.0*M)
C
      M= HMASS
      M1= CMASS
      M2= SMASS
      MTAU2= TAUMASS*TAUMASS
      MS2=SMASS*SMASS
      MC2=CMASS*CMASS
      PC = SQRT((M**2-(M1+M2)**2)*(M**2-(M1-M2)**2))/(2.0*M)
C
      NUM = PTAU*((TAUMASS*TB*CABIBBO)**2)*(MH2-MTAU2)
      DEN = 3*PC*(MS2*TB*TB+MC2*CB*CB)*(MH2-MC2-MS2)-4.0*MC2*MS2*TB*CB
      TAU = NUM/(NUM+DEN)
C
  999 RETURN
      END
