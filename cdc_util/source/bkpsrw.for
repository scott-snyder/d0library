      SUBROUTINE BKPSRW(ND,LPSRW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book a PSRW bank
C-
C-   Inputs  :
C-   Outputs : LPSRW - new bank address
C-   Controls:
C-
C-   Created  23-OCT-1995   Hailin Li
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPSRW.LINK'
      INTEGER LPSRW,MPPSRW(5),ND,LPSHT,GZPSHT,ISETVN
      DATA MPPSRW/4HPSRW,0,0,2,1/
C----------------------------------------------------------------------
      MPPSRW(4) = ND
C
      LPSHT = GZPSHT()
      IF (LPSHT .LE. 0) CALL BKPSHT( LPSHT )
      CALL MZLIFT( IXMAIN, LPSRW, LPSHT, -IZPSRW, MPPSRW, 0 )
C
      IQ(LPSRW) = ISETVN(IQ(LPSRW),1)
      RETURN
      END
