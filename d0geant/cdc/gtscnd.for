      SUBROUTINE GTSCND(NPRIM,ELKE,NSCND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate the number of secondary electrons
C-
C-   Inputs  : NPRIM = Number of primary electrons
C-             ELKE  = Kinetic energy of each primary electron
C-   Outputs : NSCND = Number of secondary electrons produced.
C-   Controls: none
C-
C-   Created   9-JAN-1992   Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IPRIM,NPRIM,NSCND
C
      REAL ELKE(*),SCND
      REAL K0AR,WAR
      REAL FANO,SMEAR
C
      DATA FANO /0.185/     ! Fano factor
      DATA K0AR /15.8/      ! First ionization potential of Argon
      DATA WAR /26.4/       ! Mean work to create an electron-ion pair
C
C----------------------------------------------------------------------
C
      NSCND = 0
      SCND = 0.
C
C  Generate secondary electrons if Kinetic energy greater than the first
C  ionization potential.  The mean work for CH4 is actually 28 eV, but has been
C  approximated to that of Argon below to simplify calculations
C
      DO IPRIM = 1,NPRIM
        IF (ELKE(IPRIM).GT.K0AR) THEN
          SCND = (ELKE(IPRIM) - K0AR)/WAR + SCND
        ENDIF
      ENDDO
C
C  NSCND is the mean number of secondaries produced. It is smeared with a
C  gaussian ditribution with a sigma equal to sqrt(FANO*MEAN) where FANO is the
C  FANO factor suggested by Lapique and Piuz.
C
      CALL NORRAN(SMEAR)
      SCND = SMEAR*SQRT(FANO*SCND) + SCND
      NSCND = NINT(SCND)
C
  999 RETURN
      END
