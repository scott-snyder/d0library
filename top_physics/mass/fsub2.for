      SUBROUTINE FSUB2(M,U2,F2,X)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Does the integral over X2 to get the
C-    cross section. X2= X_B = PARTON MOMENTUM FRACTION IN ANTI_PROTON
C-
C-   Inputs  : M= INTEGER SET BY CALLING FUNCTION DGMLT2
C-   U2 = ONE DIMENSIONAL ARRAY WITH DIMENSION M WITH CONTENTS SET BY
C-   DGMLT2
C-   Outputs : F2 = One dimensional array of dimension
C-   M whose contents are set by
C-   this function  are the values of the integrand at the values
C-   of X provided in array U2
C-   See Cernlib D110 for more info

C-   Controls:
C-
C-   Created  29-MAR-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TOP_SOLNSE.INC'
      INTEGER M
      DOUBLE PRECISION U2(M),F2(M),X(*),DGMLT1,FSUB1,A2,B2,
     &  SH,HSH,TMASSQ,TPQ
      REAL    ROOTS_TEV
      EXTERNAL FSUB1
      INTEGER L,NG2,NI2,IER
      LOGICAL first
      SAVE first
      DATA first / .true. /
C
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL EZPICK('TOP_MASS_RCP')
        CALL EZGET('NGAUSS',NG2,IER)
        CALL EZGET('NSUB_DIV',NI2,IER)
        CALL EZGET('ROOTS_TEV',ROOTS_TEV,IER)
      ENDIF
C
      DO 10 L = 1 , M
        X(2) = U2(L)
        SH = X(3)*X(2)*ROOTS_TEV**2  !S HAT
        SHATD = SH  !STORE AWAY
        HSH = 0.5*SH
        TMASSQ = TMASSE*TMASSE
        TPQ = HSH*SQRT(1.0-4.0*TMASSQ/SH)
        TMIN = -(HSH-TMASSQ+TPQ)
        TMAX = -(HSH-TMASSQ-TPQ)
        A2 = TMIN
        B2 = TMAX
        F2(L) = DGMLT1(FSUB1,A2,B2,NI2,NG2,X)
   10 CONTINUE
C
  999 RETURN
      END
