      SUBROUTINE EIGNFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank EIGN
C-
C-   Inputs  :
C-
C-   Outputs :
C-   Controls:
C-
C-   Created  20-DEC-1990 15:07:37.26  Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
C do initialization here if necessary.
        FIRST = .FALSE.
      ENDIF
C
C ****  fill EIGN bank
C
  999 RETURN
      END
