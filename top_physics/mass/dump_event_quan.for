      SUBROUTINE DUMP_MASS_EVENT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : PRINT OUT INTERESTING EVENT QUANTITIES
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   9-AUG-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:KINEQ.INC'
      INCLUDE 'D0$INC:EVENT_QUAN1.INC'
      INTEGER PRUNIT,SSUNIT
C
C----------------------------------------------------------------------
C
      PRUNIT=SSUNIT()
      CALL VEC_PRINT(PRUNIT,'ELECTRONS',P24_ELECTRON,NE,NELEC)
      CALL VEC_PRINT(PRUNIT,'PHOTONS',P18_PHOTON,NP,NPHO)
      CALL VEC_PRINT(PRUNIT,'MUONS',P23_MUON,NM,NMUO)
      CALL VEC_PRINT(PRUNIT,'JETS',P25_JETS,NJ,NJETS)
C
  999 RETURN
      END
