      SUBROUTINE FSUB3(M,U3,F3,X)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Does the integral over X3 to get the
C-    cross section. X3= X_A = PARTON MOMENTUM FRACTION IN PROTON
C-
C-   Inputs  : M= INTEGER SET BY CALLING FUNCTION DGMLT3
C-   U3 = ONE DIMENSIONAL ARRAY WITH DIMENSION M WITH CONTENTS SET BY
C-   DGMLT3
C-   Outputs : F3 = One dimensional array of dimension
C-   M whose contents are set by
C-   this function  are the values of the integrand at the values
C-   of X provided in array U3
C-   See Cernlib D110 for more info

C-   Controls:
C-
C-   Created  29-MAR-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TOP_SOLNSE.INC'
      INTEGER M
      DOUBLE PRECISION U3(M),F3(M),X(*),DGMLT2,FSUB2,A2,B2
      REAL    ROOTS_TEV
      EXTERNAL FSUB2
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
      ENDIF
C
      DO 10 L = 1 , M
        X(3) = U3(L)
        A2 = TAU_MIN/X(3)
        B2 = 1.0
        F3(L) = DGMLT2(FSUB2,A2,B2,NI2,NG2,X)
   10 CONTINUE
C
  999 RETURN
      END
