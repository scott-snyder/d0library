      SUBROUTINE CEDP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine creates the 'CEDP' bank.
C-          This bank contains header for the towerr addressing
C-          scheme.  The bank will dispatch to the constant eta
C-          banks CETA which will be booked in CETA.
C-
C-   Zebra Banks Created : CEDP
C-   Inputs  :             NONE
C-   Outputs :             NONE
C-   Controls:             NONE
C-
C-   Created  25-NOV-1988   Stephen Kahn, Esq.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$INC:CALGEO.INC'
      INCLUDE 'D0$PARAMS:CEDP.PARAMS'
      INCLUDE 'D0$LINKS:IZCEDP.LINK'
C
      INTEGER MCEDP(5)
C
      CHARACTER*4 CHAR4
      EQUIVALENCE (CHAR4,MCEDP(1))
      DATA MCEDP / 0, 37, 1, 11, 2 /
      DATA CHAR4 / 'CEDP'/
C----------------------------------------------------------------------
C
      CALL MZLIFT(IXCDV, LQCEDP, LCGEH, -IZCEDP, MCEDP, 0)
      CALL UCOPY(Q(LCGEH+1),Q(LQCEDP+1),8)    ! copy std header
      IQ(LQCEDP+IGNETA)  = 37
      IQ(LQCEDP+IGNDEP) = 17
      IQ(LQCEDP+IGNFI) = 64
C----------------------------------------------------------------------
  999 RETURN
      END
