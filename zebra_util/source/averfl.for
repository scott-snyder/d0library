      SUBROUTINE AVERFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank AVER
C-
C-   Inputs  :
C-
C-   Outputs :
C-   Controls:
C-
C-   Created  20-DEC-1990 15:07:37.26  Rajendran Raja
C-   Updated   7-APR-1995   Alan M. Jonckheere  
C-      Change calls DGET -> DDGET and DSET -> DDSET 
C-        to avoid conflict with new intrinsic functions
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
      INCLUDE 'D0$INC:HMATRIX_PARS.INC'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER I,IND
      DOUBLE PRECISION DAVER
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
C do initialization here if necessary.
        FIRST = .FALSE.
      ENDIF
C
C ****  fill AVER bank
C
C
      DO 10 I = 1 ,TOT_DIM  
        IND = 2*I-1                     ! DOUBLE PRECISION
        CALL DDGET(LAVER+IND,DAVER)
        DAVER = DAVER + C(LQUAN + I)
        CALL DDSET(LAVER+IND,DAVER)
   10 CONTINUE
C
  999 RETURN
      END
