      SUBROUTINE PCSVCC(R1,R2,TH1,TH2,NE,E1,E2,E3,COLORS,LABELS,NUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draws One Central Eta Module Of The D0 Det
C-                         In The Side View
C-
C-   Inputs  : R1    Inner Radius
C-             R2    Outer Radius
C-             TH1   Lower Theta
C-             TH2   Upper Theta
C-             NE    Number Of Energies to Plot in Module
C-             E1-E3 Scaled Energies to Plot in Module
C-   Outputs :
C-
C-   Created   3-JUN-1988   Michael Peters
C-   Updated  26-AUG-1988   Michael W. Peters  Use retained segments
C-   Updated  19-DEC-1989   Lupe Rosas Using the Color table
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      REAL R1,R2,TH1,TH2,E1,E2,E3
      INTEGER NE,I, NUM, J
      CHARACTER*3 IC
      CHARACTER*(*) COLORS(*)
      CHARACTER*(*) LABELS(*)
      REAL TTH1,TTH2,R,SY,X(4),Y(4)
C----------------------------------------------------------------------
      IF(TH1.LT.0.) THEN
        SY=-1.
      ELSE
        SY=1.
      ENDIF
      TTH1=TAN(ABS(TH1))
      TTH2=TAN(ABS(TH2))
      X(1)=R1/TTH2
      Y(1)=SY*R1
      X(2)=R1/TTH1
      Y(2)=SY*R1
      X(3)=R2/TTH1
      Y(3)=SY*R2
      X(4)=R2/TTH2
      Y(4)=SY*R2
      CALL JPINTR(0)
      CALL JPOLGN(X,Y,4)
      IF(NE.LT.1.OR.NE.GT.3) RETURN
      CALL JPINTR(1)
      DO 500 I=1,NE
        IF(I.EQ.1) THEN
          R=R1+E1
          IC='RED'     
        ELSEIF(I.EQ.2) THEN
          R=R1+E1+E2
          IC='CYA'      
        ELSEIF(I.EQ.3) THEN
          R=R1+E1+E2+E3
          IC='YEL'     
        ENDIF
        X(3)=R/TTH1
        Y(3)=SY*R
        X(4)=R/TTH2
        Y(4)=SY*R
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
