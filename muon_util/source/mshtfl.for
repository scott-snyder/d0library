      SUBROUTINE MSHTFL( ITRAK, NPTR, IPTR )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill MSHT bank
C-
C-   Inputs  : ITRAK : MUOT track ID
C-             NPTR  : number of valid scint hit
C-             IPTR  : scint hit ID
C-   Outputs : None
C-   Controls: None
C-
C-   Created  24-FEB-1994   Atsushi Taketani
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  ITRAK, NPTR, IPTR(4)
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER  LMSHT, GZMSHT, I
C----------------------------------------------------------------------
      LMSHT = GZMSHT(ITRAK)
      IF ( LMSHT.EQ.0 ) GOTO 999
C
      DO I=1,NPTR
        IQ(LMSHT+I) = IPTR(I)
      END DO
C
  999 RETURN
      END
