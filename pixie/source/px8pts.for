

     
     
      SUBROUTINE PX8PTS(X,Y,Z)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Draw a 3D Figure Bounded by 6 Quadrilaterals
C-
C-   Inputs  :  X,Y,Z  Arrays of 8 coordinates of vertices in required order
C-   Outputs :
C-
C-   Created   21-JUL-1988   Michael Peters
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
     
      REAL X(8),Y(8),Z(8)
      REAL XV(4),YV(4),ZV(4)
      INTEGER I,J,L,LP(4,4)
      DATA LP/1,2,6,5, 2,3,7,6, 3,4,8,7, 4,1,5,8/
      CALL J3PLGN(X(1),Y(1),Z(1),4)
      CALL J3PLGN(X(5),Y(5),Z(5),4)
      DO 200 I=1,4
        DO 100 J=1,4
          L=LP(J,I)
          XV(J)=X(L)
          YV(J)=Y(L)
          ZV(J)=Z(L)
  100   CONTINUE
        CALL J3PLGN(XV,YV,ZV,4)
  200 CONTINUE
      RETURN
      END
