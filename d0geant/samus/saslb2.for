      SUBROUTINE SASLB2 ( COOR, TRAN, ROTM, ROT_FLAG, X, Y, Z )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  : COOR - coordinates of slab in local system of the magnet
C-             TRAN - coordinate of magnet in global system
C-             ROTM - rotation matrix
C-             ROT_FLAG - rotatiom flag (zero if no rotation)
C-   Outputs : X, Y, Z - coordinates of slab in global system
C-   Controls:
C-
C-   Created  23-APR-1991   Andrei Kiryunin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ROT_FLAG, I,J
      REAL    COOR(3),TRAN(3),ROTM(3,3), VECT(3),V, X,Y,Z
C----------------------------------------------------------------------
C
C ****  Make (if need) rotation
C
      IF (ROT_FLAG.NE.0) THEN
        DO 20 I=1,3
          V=0.
          DO 10 J=1,3
            V=V+ROTM(I,J)*COOR(J)
   10     CONTINUE
          VECT(I)=V
   20   CONTINUE
      ELSE
        CALL UCOPY (COOR(1),VECT(1),3)
      ENDIF
C
C ****  Make transformation to global system
C
      X = TRAN(1)+VECT(1)
      Y = TRAN(2)+VECT(2)
      Z = TRAN(3)+VECT(3)
C
  999 RETURN
      END
