      FUNCTION TOP_FIT_FIN()
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
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:EVENT_QUAN_2C.INC'
      LOGICAL TOP_FIT_FIN
      INTEGER IER
      INTEGER STATUS
      INTEGER SSUNIT
C----------------------------------------------------------------------
      TOP_FIT_FIN = .TRUE.
      CALL NTUPLE_CLOSE('FIT2C',STATUS)
      WRITE(SSUNIT(),1)ITOT_EV,IACC_EV,IFIT_EV
    1 FORMAT(' TOTAL NUMBER OF EVETNS READ ',I,/,
     &       ' NUMBER OF EVENTS PASSING CUTS ',I,/,
     &       ' NUMBER OF EVENTS WITH MASS FITS ',I)
  999 RETURN
      END
