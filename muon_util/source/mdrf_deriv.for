      REAL FUNCTION MDRF_DERIV(TIME,ANGLE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate partial derivative of time-to-distance
C-                         function
C-
C-   Returned value  :    Derivative w.r.t. time
C-   Inputs  : TIME = drift time, ANGLE = incident angle
C-   Outputs : 
C-   Controls: 
C-
C-   Created  10-MAY-1994   R. Markeloff
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      INTEGER IFAST, I, J, K
      LOGICAL FIRST
      REAL TIME, ANGLE, T2D(42)
C
      DATA FIRST / .TRUE. /
C
      IF (FIRST) THEN
        CALL GTMDFT(10,IFAST,T2D)
        IF (IFAST .LT. 2) CALL ERRMSG('Obsolete time-to-distance',
     &     'MDRF_DERIV','Cannot tune T0s','W')
        FIRST = .FALSE.
      ENDIF
C
      MDRF_DERIV = 0.
C
      DO I = 1,16
        J = (I-1)/4 + 1
        K = I - (J-1)*4 -1
        IF (K .GT. 0) THEN
          MDRF_DERIV = MDRF_DERIV + J*T2D(I)*(TIME**(J-1))*(ANGLE**K)
        ELSE
          MDRF_DERIV = MDRF_DERIV + J*T2D(I)*(TIME**(J-1))
        ENDIF
      ENDDO
C
      RETURN
      END
