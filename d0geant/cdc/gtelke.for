      SUBROUTINE GTELKE(NPION,NPEXC,NPRIM,ELKE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate the energy transferred (E) in each
C-   collision.On the basis of E, calulate the number of primary electrons and
C-   their Kinetic energies using the most probable decay modes of Ar and CH4.
C-   The table of the most probable decay modes is given by Lapique and Piuz.
C-
C-   Inputs  : NPION = # of prim. coll. due to ionization in each shell
C-             NPEXC = # of prim. coll. due to excitation in each shell
C-   Outputs : NPRIM = Total number of primary electrons produced
C-             ELKE  = Kinetic energy of each primary electron.
C-   Controls: none
C-
C-   Created   9-JAN-1992   Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER NPION(4),NPEXC(4)
      INTEGER NPRIM,NCOLL
      INTEGER ISHELL,ILEVEL,IPRIM
C
      REAL R,R1,R2,RNDM
      REAL E,EX,EION(4),S(4)
      REAL ELKE(1000)
C
      DATA S /2.29,3.2,3.2,2.15/      ! Slope of the photoabsorption spectra
      DATA EION /248.,52.,15.8,11.5/  ! Ionization potential for various shells
C
C  The above data statements are referenced in TPC internal note:
C  "Measurement of energy loss in TPC by Gerry Lynch"
C
C----------------------------------------------------------------------
C
C  Loop over all primary electrons from ionization(ILEVEL=1) and
C  excitation(ILEVEL=2)
C
      NPRIM = 0                   ! Number of primaries
C
      DO ILEVEL = 1,2
C
        DO ISHELL = 1,4           ! Loop over each atomic shell
C
          IF (ILEVEL.EQ.1) THEN
            NCOLL = NPION(ISHELL)
          ELSE
            NCOLL = NPEXC(ISHELL)
          ENDIF
C
          DO IPRIM = 1,NCOLL      ! Loop over all collisions
C
C  Calculate the energy transferred in each collision = E
C
            R = RNDM(0)
            IF (ILEVEL.EQ.1) THEN
              EX = EION(ISHELL)/R
              E = EX - EION(ISHELL)
            ELSE
              EX = EION(ISHELL) * (R**(-1./S(ISHELL)))
              E = EX
            ENDIF
C
C  Based on E determine NPRIM and KE based on Lapique-Piuz's table
C
            IF (ISHELL.LE.3) THEN
              IF (E.GE.15.8.AND.E.LT.248.) THEN
                NPRIM = NPRIM + 1
                ELKE(NPRIM) = E - 15.8
              ELSE IF (E.GE.248..AND.E.LT.280.) THEN
                NPRIM = NPRIM + 2
                ELKE(NPRIM-1) = 200.
                ELKE(NPRIM) = E - 248.
              ELSE IF (E.GE.280..AND.E.LT.326.) THEN
                R2 = RNDM(0)
                IF (R2.LE.0.8) THEN
                  NPRIM = NPRIM + 2
                  ELKE(NPRIM-1) = 200.
                  ELKE(NPRIM) = E - 248.
                ELSE
                  NPRIM = NPRIM + 3
                  ELKE(NPRIM-2) = 200.
                  ELKE(NPRIM-1) = E - 265.
                  ELKE(NPRIM) = E - 265. - ELKE(NPRIM-1)
                ENDIF
              ELSE IF (E.GE.326..AND.E.LT.3206.) THEN
                R2 = RNDM(0)
                IF (R2.LE.0.65) THEN
                  NPRIM = NPRIM + 2
                  ELKE(NPRIM-1) = 200.
                  ELKE(NPRIM) = E - 248.
                ELSE
                  NPRIM = NPRIM + 3
                  ELKE(NPRIM-2) = 50.
                  ELKE(NPRIM-1) = 200.
                  ELKE(NPRIM) = E - 326.
                ENDIF
              ELSE IF (E.GE.3206.) THEN
                R2 = RNDM(0)
                IF (R2.LE.0.88) THEN
                  NPRIM = NPRIM + 3
                  ELKE(NPRIM-2) = 200.
                  ELKE(NPRIM-1) = 2670.
                  ELKE(NPRIM) = E - 3206.
                ELSE
                  NPRIM = NPRIM + 2
                  ELKE(NPRIM-1) = 2870.
                  ELKE(NPRIM)   = E - 3206.
                ENDIF
              ENDIF
            ELSE
              IF (E.GE.11.5.AND.E.LE.283.) THEN
                NPRIM = NPRIM + 1
                ELKE(NPRIM) = E - 11.5
              ELSE IF (E.GT.283.) THEN
                NPRIM = NPRIM + 2
                ELKE(NPRIM-1) = E-283.
                ELKE(NPRIM) = 243.
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C      
  999 RETURN
      END
