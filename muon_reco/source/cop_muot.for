      SUBROUTINE COP_MUOT (MUVERT,ITRAK,HITA,HITW,HITS,NFITDA,NFITDBC,
     &                      QUAD,MAG,DIC,DOC,VMU,PCAL,DPFIT,DE_T,DE_C,
     &                       PX,PY,PZ,CONVERGE,CHI_TRAK,B_TOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-              MUVERT    vertex flag
C-              ITRAK     MUOT Track number
C-   Outputs : 
C-              HITA      Hit flag for A layer
C-              HITW      # of wide angle hits
C-              HITS      # of small angle hits
C-              NFITDA    # of fitted hits
C-              NFITDBC   # of fitted hits in B&C
C-              QUAD      quadrant
C-              MAG       Middle point in mag
C-              DIC       direction cosines in
C-              DOC       direction cosines out
C-              VMU       Point tarck defined
C-              PCAL      Momentum
C-              DPFIT     Error
C-              DE_T      Eloss in toroid
C-              DE_C      Eloss in cal
C-              PX        Momemtum components
C-              PY            :
C-              PZ            :
C-              CONVERGE  Flag of convergence
C-              CHI_TRACK Chisqur
C-              B_TOR     Mean B.dl
C-   Controls: 
C-
C-   Created   15-JAN-1992   SHAHRIAR ABACHI
C-   Modified  06-JAN-1992   SHAHRIAR ABACHI Flag check on ifw1 modified
C-   DH 4/93 DO A-LAYER CORRECTLY
C    DH 2/95 change DPFIT to a straight 20% error
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ITRAK,HITA,HITW,HITS,NFITDA,NFITDBC,QUAD,CONVERGE,MUVERT
      REAL MAG(3),DIC(3),DOC(3),VMU(3),PCAL,DPFIT,DE_T,DE_C
      REAL PX,PY,PZ,CHI_TRAK,B_TOR,PMU,RR
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LMUOT,GZMUOT,I
C
      LMUOT = GZMUOT(ITRAK)
C
      IF(LMUOT .GT. 0) THEN
        RR=SQRT(Q(LMUOT+8)**2+Q(LMUOT+9)**2+Q(LMUOT+10)**2)
CC   RR IS THE DISTANCE FROM 0,0,0 OF THE INSIDE POINT
CC   IF THIS IS SMALL, THAT MEANS THE A-LAYER WASN'T USED IN THE FIT
        HITA = 999          
        IF(RR.LE.150.) HITA=0
        HITW = IQ(LMUOT + 1)
        HITS = IQ(LMUOT + 2)
        QUAD = IQ(LMUOT + 3)
        DO I=1,3
          MAG(I) = Q(LMUOT + 10 + I)
          DIC(I) = Q(LMUOT + 13 + I)
          DOC(I) = Q(LMUOT + 16 + I)
          VMU(I) = Q(LMUOT + 7 + I)
        ENDDO
        PCAL = Q(LMUOT + 23)
        PMU = ABS(PCAL)
        DPFIT = .20
        DE_T = Q(LMUOT + 26)
        DE_C = Q(LMUOT + 25)
        PX = PMU * DIC(1)
        PY = PMU * DIC(2)
        PZ = PMU * DIC(3)
        CONVERGE = 999 + MUVERT
        CHI_TRAK = Q(LMUOT + 20)
        B_TOR = -999.
      ENDIF

C----------------------------------------------------------------------
  999 RETURN
      END
