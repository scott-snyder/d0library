      SUBROUTINE FRESID(X,Y,N,RESID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find residuals by removing a wire hit from
C-                         a segment
C-
C-   Inputs  : X,Y,N
C-   Outputs : RESID
C-   Controls:
C-
C-   Created  24-MAY-1990   Susan K. Blessing
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER N,I,J
      REAL X(16),Y(16),A,B
      REAL A1,B1,D
      REAL XSUM,YSUM,XYSUM,XXSUM
      REAL RESID(16),YOFX
C----------------------------------------------------------------------
C
C Loop over all points.
C
      DO 10 J = 1, N
C
        XSUM=0.
        YSUM=0.
        XYSUM=0.
        XXSUM=0.
C
C Do fit leaving each point out successively
C
        DO 20 I=1,N
          IF (I.NE.J) THEN
            XSUM = XSUM + X(I)
            YSUM = YSUM + Y(I)
            XYSUM = XYSUM + X(I)*Y(I)
            XXSUM = XXSUM + X(I)*X(I)
          END IF
   20   CONTINUE
C
        A1 = YSUM*XXSUM - XSUM*XYSUM
        B1 = FLOAT(N-1)*XYSUM - XSUM*YSUM
        D = FLOAT(N-1)*XXSUM - XSUM*XSUM

        A = 0.
        B = 0.
        IF (D.NE.0.) THEN
          A = A1/D
          B = B1/D
        END IF
C
        YOFX = A + B * X(J)
        RESID(J) = Y(J) - YOFX
   10 CONTINUE
C
C----------------------------------------------------------------------
  999 RETURN
      END
