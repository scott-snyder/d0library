      FUNCTION CORRECT_OLD_GETLUM(OLDLUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Converts the old GETLUM result to a more accurate
C-    instantaneous luminosity value.
C-    Correction is flat ~+10% until ~12e30 where it starts to diverge
C-    Everything is in units of E30 (input & output)
C-    See http://d0wop.fnal.gov/~gian/www.html
C-    Returns -1 if error
C-
C-   Inputs  : OLDLUM  - old result from GETLUM.FOR
C-   Outputs : CORRECT_OLD_GETLUM - corrected to match best estimate 
C-
C-   Created  28-AUG-1995   Gian Di Loreto
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C EXT PARAMETER                                   STEP         FIRST
C NO.   NAME        VALUE          ERROR          SIZE      DERIVATIVE
C  1      A0       0.89912       0.27610E-01   0.22746E-02  -0.12838E-08
C  2      A1       0.64451E-02   0.63514E-02   0.31370E-04   0.66525E-08
C  3      A2      -0.60681E-03   0.33579E-03   0.28982E-05   0.22991E-06
C
C   CHISQUARE = 0.2190E+00  NPFIT =    16
C----------------------------------------------------------------------
      REAL A0,A1,A2,X,OLDLUM,CORRECT_OLD_GETLUM
C
      PARAMETER(A0= 0.89912)
      PARAMETER(A1= 0.64451E-02)
      PARAMETER(A2= -0.60681E-03)
C
      X=OLDLUM
C     ERROR HANDLING
      IF(X.LT.0.0.OR.X.GT.35.0) THEN
        CORRECT_OLD_GETLUM=-1.0
        RETURN
      ENDIF
C
C  ratio = oldlum/correct_old_getlum, 1/ratio = correct_old_getlum/oldlum
C  correct_old_getlum=ratio(oldlum)*oldlum
C
      CORRECT_OLD_GETLUM=(1.0/(A0+A1*X+A2*(X**2)))*OLDLUM
C
      RETURN
      END
