      SUBROUTINE CGL2CRATE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-              generate table /CL2CRATE/ of level 2 calorimeter crate ID's
C-              which run from 1 to 12, mapping down from 0:63, the raw crate
C-              numbers
C-   Inputs  : none
C-   Outputs : /CL2CRATE/
C-   Controls:
C-
C-   Created  14-FEB-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$INC:CL2CRATE.INC'
      INTEGER ICC                       !Crate on cable
      INTEGER ICAD                      !bank number
      INTEGER CRATE_L2                  ! crate number in level 2 scheme
      INTEGER CRATE_CAD                 ! crate number in raw data
C----------------------------------------------------------------------
      CALL VZERO(L2CRATE,64)
      CRATE_L2 = 0
      DO ICAD = 7,8
        DO ICC = 0,NADCRC-1
          CRATE_CAD = 10*ICC + ICAD
          CRATE_L2 = CRATE_L2 + 1
          L2CRATE(CRATE_CAD) = CRATE_L2
        ENDDO
      ENDDO
  999 RETURN
      END
