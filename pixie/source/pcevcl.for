      SUBROUTINE PCEVCL(R1,R2,PHI1,PHI2,NE,E1,E2,E3,COLORS,LABELS,NUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DRAW ONE CAL PHI MODULE SEEN FROM THE END
C-
C-   Inputs  : R1     Min Radius
C-             R2     Max Radius
C-             PHI1   Min Phi
C-             PHI2   Max Phi
C-             NE     Number Of Energies To Plot
C-             E1-3   Energies To Plot
C-   Outputs :
C-
C-   Created  19-APR-1988   Michael Peters
C-   Updated  19-DEC-1989   Lupe Rosas Implementing color table
C-   Modified 18-AUG-1991   Nobu Oshima ( Gives colors for E&S )
C-   Modified 20-MAR-1992   Nobu Oshima ( Take care ICD+MG )
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      REAL R1,R2,PHI1,PHI2,E1,E2,E3
      REAL CPHI1,SPHI1,CPHI2,SPHI2,X(4),Y(4),R,REPS
      REAL RLEVEL
      INTEGER NE,I, NUM, J
      CHARACTER*3 CC
      CHARACTER*(*) COLORS(*)
      CHARACTER*(*) LABELS(*)
C-
C--- Fill COLORS and LABELS
C-
      IF (NUM .EQ. 1) THEN
        IF (NE .EQ. 3) THEN
          DO J=1, NE
            IF (J .EQ. 1) THEN
              LABELS(NUM) = ' EM           '
              COLORS(NUM) = 'RED'
            ELSEIF(J .EQ. 2) THEN
              LABELS(NUM) = ' ICD+MG       '
              COLORS(NUM) = 'YEL'
            ELSEIF(J .EQ. 3) THEN
              LABELS(NUM) = ' HAD          '
              COLORS(NUM) = 'CYA'
            ENDIF
            NUM = NUM + 1
          ENDDO
        ELSEIF (NE .EQ. 2) THEN
          DO J=1, NE
            IF (J .EQ. 1) THEN
              LABELS(NUM) = ' EM           '
              COLORS(NUM) = 'RED'
            ELSEIF(J .EQ. 2) THEN
              LABELS(NUM) = ' HAD          '
              COLORS(NUM) = 'CYA'
            ENDIF
            NUM = NUM + 1
          ENDDO
        ENDIF
      ENDIF
C---
      CALL JIQDIL(RLEVEL)
      CPHI1=COS(PHI1)
      SPHI1=SIN(PHI1)
      CPHI2=COS(PHI2)
      SPHI2=SIN(PHI2)
      X(1)=R1*CPHI1
      Y(1)=R1*SPHI1
      X(2)=R1*CPHI2
      Y(2)=R1*SPHI2
      X(3)=R2*CPHI2
      Y(3)=R2*SPHI2
      X(4)=R2*CPHI1
      Y(4)=R2*SPHI1
      IF(RLEVEL .EQ. -2.) THEN
        CALL PXCOLR('FOR')
      ELSE
        CALL JPINTR(0)
      ENDIF
      CALL JPOLGN(X,Y,4)
      IF(NE.LT.1.OR.NE.GT.3) RETURN
      IF(RLEVEL .EQ. -2.) THEN
        REPS=R1+0.5
        X(1)=REPS*COS(PHI1+0.01)
        Y(1)=REPS*SIN(PHI1+0.01)
        X(2)=REPS*COS(PHI2-0.01)
        Y(2)=REPS*SIN(PHI2-0.01)
      ENDIF
      DO 500 I=1,NE
        IF(I.EQ.1) THEN
          R=R1+E1
          CC='RED'     ! Cyan
        ELSEIF(I.EQ.2) THEN
          R=R1+E1+E2
          IF (NE .EQ. 3) THEN
            CC='YEL'     ! Yellow
          ELSE
            CC='CYA'     ! Red
          ENDIF        
        ELSEIF(I.EQ.3) THEN
          R=R1+E1+E2+E3
          CC='CYA'     ! Red
        ENDIF
        IF(RLEVEL .EQ. -2.) THEN
          CALL PXCOLR(CC)
        ELSE
          CALL PXCOLFILL(CC)
        ENDIF
        IF(RLEVEL .EQ. -2.) THEN
          REPS=R-0.5
          X(3)=REPS*COS(PHI2-0.01)
          Y(3)=REPS*SIN(PHI2-0.01)
          X(4)=REPS*COS(PHI1+0.01)
          Y(4)=REPS*SIN(PHI1+0.01)
        ELSE
          X(3)=R*CPHI2
          Y(3)=R*SPHI2
          X(4)=R*CPHI1
          Y(4)=R*SPHI1
        ENDIF
        CALL JPOLGN(X,Y,4)
        IF(RLEVEL .EQ. -2.) THEN
          REPS=R+1.0
          X(3)=REPS*COS(PHI2-0.01)
          Y(3)=REPS*SIN(PHI2-0.01)
          X(4)=REPS*COS(PHI1+0.01)
          Y(4)=REPS*SIN(PHI1+0.01)
        ENDIF
        X(1)=X(4)
        Y(1)=Y(4)
        X(2)=X(3)
        Y(2)=Y(3)
  500 CONTINUE
      RETURN
      END
