      FUNCTION GAUSS_TAIL(X)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return value from a point along a Gaussian fit 
C-                         with a tail (Gaussian to  peak, exponential 
C-                         after peak)
C-
C-   Returned value  : value of function fit at X
C-   Inputs  : X = location along fit
C-   Outputs : none
C-   Controls: none
C-
C-   Created  11-SEP-1990   Susan K. Blessing   
C-   Updated  25-JAN-1991   Jeffrey Bantly  insert the real pulse shape
C-                                          fit values 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      DOUBLE PRECISION GAUSS_TAIL
      DOUBLE PRECISION P(4)
C       P(1) = SCALE 
C       P(2) = PEAK (MEAN FOR GAUSSIAN)
C       P(3) = SIGMA FOR GAUSSIAN
C       P(4) =  WIDTH FOR EXPONENTIAL 
C
      REAL X, Y
C
C      DATA P/1.000000000000000,2.062394033704220,2.177321384225985,
C     &       2.871605584344834/
      DATA P/1.000,2.650,2.670,4.080/
C
C------------------------------------------------------------------------
C
      Y = X - P(2)
C
      IF (Y.LE.0.0) THEN
        GAUSS_TAIL =  P(1) * EXP(-.5*( Y/P(3))**2)
      ELSE
        GAUSS_TAIL = P(1)/P(4) * (Y + P(4)) * EXP(-Y/P(4)) 
      ENDIF
C
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
