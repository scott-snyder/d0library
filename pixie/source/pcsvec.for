      SUBROUTINE PCSVEC(Z1,Z2,TH1,TH2,NE,E1,E2,E3,COLORS,LABELS,NUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draws One Endcap Eta Module of the D0 Det
C-                         in the Side View
C-
C-   Inputs  : Z1    Min ABS(Z)
C-             Z2    Max ABS(Z)
C-             TH1   Lower Theta
C-             TH2   Upper Theta
C-             NE    Number of Energies to Plot in Module
C-             E1-E3 Scaled Energies to Plot in Module
C-   Outputs :
C-
C-   Created   7-JUN-1988   Michael Peters
C-   Updated  26-AUG-1988   Michael W. Peters  Use retained segments
C-   Updated  19-DEC-1989   Lupe Rosas   Using the color table
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      REAL Z1,Z2,TH1,TH2,E1,E2,E3
      INTEGER NE,I, NUM, J
      CHARACTER*3 IC
      CHARACTER*(*) COLORS(*)
      CHARACTER*(*) LABELS(*)
      REAL TTH1,TTH2,Z,SX,SY,X(4),Y(4)
      TTH1=TAN(ABS(TH1))
      TTH2=TAN(ABS(TH2))
      IF(TTH2.LT.0.) THEN
        SX=-1.
      ELSE
        SX=1.
      ENDIF
      TTH1=ABS(TTH1)
      TTH2=ABS(TTH2)
      IF(TH2.LT.0.) THEN
        SY=-1.
      ELSE
        SY=1.
      ENDIF
      X(1)=SX*Z1
      Y(1)=SY*Z1*TTH2
      X(2)=SX*Z1
      Y(2)=SY*Z1*TTH1
      X(3)=SX*Z2
      Y(3)=SY*Z2*TTH1
      X(4)=SX*Z2
      Y(4)=SY*Z2*TTH2
      CALL JPINTR(0)
      CALL JPOLGN(X,Y,4)
      IF(NE.LT.1.OR.NE.GT.3) RETURN
      CALL JPINTR(1)
      DO 500 I=1,NE
        IF(I.EQ.1) THEN
          Z=Z1+E1
          IC='RED'    
        ELSEIF(I.EQ.2) THEN
          Z=Z1+E1+E2
          IC='CYA'     
        ELSEIF(I.EQ.3) THEN
          Z=Z1+E1+E2+E3
          IC='YEL'    
        ENDIF
        X(3)=SX*Z
        Y(3)=SY*Z*TTH1
        X(4)=SX*Z
        Y(4)=SY*Z*TTH2
        CALL PXCOLFILL(IC)
        CALL JPOLGN(X,Y,4)
        X(1)=X(4)
        Y(1)=Y(4)
        X(2)=X(3)
        Y(2)=Y(3)
        DO 400 J=1, NUM
          IF (COLORS(J).EQ. IC) 
     &      GO TO 500 
  400   CONTINUE
        COLORS(NUM)  = IC
        IF (IC.EQ.'RED') THEN
          LABELS(NUM) = ' EM           '
        ELSEIF(IC.EQ.'CYA') THEN
          LABELS(NUM) = ' HAD          '
        ELSEIF(IC.EQ.'YEL') THEN
          LABELS(NUM) = '              '
        ENDIF
        NUM = NUM + 1
  500 CONTINUE
      RETURN
      END
