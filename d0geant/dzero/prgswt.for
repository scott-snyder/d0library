      SUBROUTINE PRGSWT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : PRINT GEANT SWITCHES
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-JUL-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CALL PRT_FFID0                    ! Printing new values
      CALL PRT_FFICAL
      CALL PRT_FFIVTX
      CALL PRT_FFICDC
      CALL PRT_FFIFDC
      CALL PRT_FFITRD
      CALL PRT_FFIMUO
      CALL PRT_FFILV0
  999 RETURN
      END
