      SUBROUTINE GTPRIM(DIST,NPION,NPEXC,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate Number of collisions for charged particle
C-                         traversing through Mark-2 gas.
C-
C-   Inputs  : DIST = Distance covered by particle in gas volume.
C-   Outputs : NPION = Number of primary collisions due to ionization.
C-             NPEXC = Number of primary collisions due to excitation.
C-   Controls: none
C-
C-   Created   8-JAN-1992   Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCTRAK.INC'
      INCLUDE 'D0$INC:GCKINE.INC'
C
      INTEGER ISHELL,NCOLL,ERR
      INTEGER NPION(4),NPEXC(4)   ! Outputs
C
      REAL BETA,DIST
      REAL BETAS,EM,EX
      REAL NPRIM1,NPRIM2
      REAL COEFF(4)               ! Calculated coeff from PAI model
      REAL IPOT(4)                ! Ionization potential for the four levels
      REAL PFREQ                  ! Plasma frequency
C
      DATA COEFF /0.09115,0.15892,1.223,0.0804/
      DATA IPOT /538.41,81.92,24.89,27.44/
C
      LOGICAL STATUS
C----------------------------------------------------------------------
C
      STATUS = .FALSE.
      NCOLL = 0
      CALL VZERO(NPION,4)
      CALL VZERO(NPEXC,4)
C
      BETA=VECT(7)/GETOT
      BETAS = BETA*BETA               
      EM=VECT(7)*VECT(7)/AMASS/AMASS
C
C  For each atomic level, the number of primary collisions are calculated.
C  The atomic levels considered are Argon-L shell, Ar-M shell, Ar-Outer shell
C  and CH4 outer shell.  The effect of K-shell for all gases is negligible and
C  has been neglected. The effect of 3% CO2 has also been negelected.
C
      DO ISHELL = 1,4
C
C  Mean number of collisions from ionizations in each shell
C
        NPRIM1 = DIST*COEFF(ISHELL)/BETAS
C
        PFREQ = 0.6659                  ! Plasma frequency**2 for Argon
        IF (ISHELL.EQ.4) PFREQ = 0.371  ! for Methane
C
C  Mean number of collsions from excitations in each shell
C
        EX = IPOT(ISHELL) + PFREQ*EM/IPOT(ISHELL)
        NPRIM2 = NPRIM1 * (ALOG(1022000.*EM/EX) - BETAS)
C
C  Generate a poisson distribution about the mean number of collisions in each
C  shell and pick the actual number of collisions.
C
        CALL POISSN(NPRIM1,NPION(ISHELL),ERR)
        IF (ERR.NE.0) GO TO 999
        CALL POISSN(NPRIM2,NPEXC(ISHELL),ERR)
        IF (ERR.NE.0) GO TO 999
C
C  Reject if total number of primary collisions exceeds 500.
C
        NCOLL = NCOLL + NPION(ISHELL) + NPEXC(ISHELL)
        IF (NCOLL.GT.500) GO TO 999
      ENDDO
C
      STATUS = .TRUE.
C
  999 RETURN
      END
