      REAL FUNCTION SADIPL(P,LINE,MODE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 3D or 2D distance between point and line
C-
C-   Returned value  : squared distance
C-   Inputs  :    P   - point coordinates
C-               LINE - line parameters ( point coor.& direc. cosines)
C-               mode = 1 3D distance
C-                    = 2 2D in XZ plane
C-                    = 3 2D in YZ plane
C-
C-   Outputs :
C-   Controls:
C-
C-   Created  21-AUG-1995   Andrei Mayorov
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    A,QQ,NORM,P(3),LINE(6)
      INTEGER MODE,J,JB,STEP
C----------------------------------------------------------------------
      GOTO (10,20,30) MODE
   10 JB=1
      STEP=1
      GOTO 35
   20 JB=1
      STEP=2
      GOTO 35
   30 JB=2
      STEP=1
   35 NORM=0
      QQ = 0.0
      DO J = JB, 3,STEP
        QQ = QQ + (P(J) - LINE(J)) * LINE(J+3)
        NORM=LINE(J+3)**2+NORM
      END DO
      SADIPL = 0.0
      DO J = JB, 3,STEP
        A = LINE(J) + QQ * LINE(J+3)/NORM - P(J)
        SADIPL = SADIPL + A * A
      END DO
  999 RETURN
      END
