      FUNCTION TOP_MASS_FIN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finish up Hmatrix
C-
C-   Returned value  : True if OK
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-DEC-1990   Rajendran Raja
C-   Updated  23-MAR-2004   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:KINEQ.INC'
      LOGICAL TOP_MASS_FIN
      INTEGER IER
      INTEGER STATUS
      INTEGER SSUNIT
C----------------------------------------------------------------------
      TOP_MASS_FIN = .TRUE.
      CALL NTUPLE_CLOSE('DILEPTON',STATUS)
      WRITE(SSUNIT(),1)ITOT_EV,IACC_EV,ISOL_EV
    1 FORMAT(' TOTAL NUMBER OF EVETNS READ ',I7,/,
     &       ' NUMBER OF EVENTS PASSING CUTS ',I7,/,
     &       ' NUMBER OF EVENTS WITH MASS SOLUTIONS ',I7)
  999 RETURN
      END
