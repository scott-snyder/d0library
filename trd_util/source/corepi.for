      REAL FUNCTION COREPI (EIN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Analys of energy deposit in the T.R.D. TEC
C-
C-   Inputs  :EIN    =Energy of the particle (assumed to be a pion)
C-            THETA  =Theta angle
C-   Outputs :COR =Normalized  to 5 gev/c pions energy deposit
C-   Controls:
C-
C-   Created  28-JUN-1989   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL XG(24),YG(24)
      REAL COR,EIN,GAMMA,PREF,GAMREF,YREF,E0,THETA
      INTEGER I
      LOGICAL FIRST
C
C   Relative dE/dX versus gamma
C   XG=GAMMA OF THE PART.,YG=E/EMIN
C
      DATA XG/2.,3.,3.5,4.,5.,6.,7.,8.,9.,10.,20.,50.,100.,200.,300.,
     + 400.,500.,600.,700.,800.,900.,1000.,2000.,4000.         /
      DATA YG/1.073,1.011,1.,1.01,1.016,1.036,1.057,1.073,1.089,1.104,
     + 1.219,1.385,1.495,1.589,1.63,1.652,1.672,1.682,1.693,
     + 1.698,1.708,1.714,1.740,1.745                         /
      DATA PREF/5./,E0/0./
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST=.FALSE.
        GAMREF=PREF/.14
        GAMREF=AMAX1(2.,GAMREF)
        DO 6 I=1,23
          IF(GAMREF.LT.XG(I))GO TO 8
          IF(GAMREF.GT.XG(I+1))GO TO 6
          YREF=YG(I)+(GAMREF-XG(I))*(YG(I+1)-YG(I))/(XG(I+1)-XG(I))
          GO TO 8
    6   CONTINUE
    8   CONTINUE
C        PRINT*,' DANS COREPI,GAMREF',GAMREF,'YREF',YREF
      END IF
      COREPI=1.
      GAMMA=AMAX1(2.,EIN/.14)
      DO 16 I=1,23
        IF(GAMMA.LT.XG(I))GO TO 18
        IF(GAMMA.GT.XG(I+1))GO TO 16
        COR=YG(I)+(GAMMA-XG(I))*(YG(I+1)-YG(I))/(XG(I+1)-XG(I))
        GO TO 18
   16 CONTINUE
   18 CONTINUE
      COREPI=YREF/COR
C     PRINT*,'EIN',EIN,' GAMMA',GAMMA,'COR gamma',COREPI
  999 RETURN
      END
