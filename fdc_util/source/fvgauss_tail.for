      FUNCTION FVGAUSS_TAIL(X,PAR2)
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
      DOUBLE PRECISION FVGAUSS_TAIL
      DOUBLE PRECISION P(4)
C       P(1) = SCALE 
C       P(2) = PEAK (MEAN FOR GAUSSIAN)
C       P(3) = SIGMA FOR GAUSSIAN
C       P(4) =  WIDTH FOR EXPONENTIAL 
C
      REAL X, Y, PAR2
C
      DATA P/1.00000,2.06239,2.17732,2.87161/
C
C------------------------------------------------------------------------
C
C  Check and adjust for faster than expected risetime.
C  Shift coordinate so that Y=0 is at the transition from Gaussian to Expon
C    when X=0 is at the leading edge of the pulse.
C
      IF(PAR2.LE. 0.0) PAR2=P(2)+1.0
      IF(P(2).LE.PAR2) THEN
        Y = X - P(2)
      ELSE
        Y = X - PAR2
      ENDIF
C
      IF (Y.LE.0.0) THEN
        FVGAUSS_TAIL =  P(1) * EXP(-.5*( Y/P(3))**2)          ! Gaussian
      ELSE
        FVGAUSS_TAIL = P(1)/P(4) * (Y + P(4)) * EXP(-Y/P(4))  ! Exponential
      ENDIF
C
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
