      SUBROUTINE ISA_SMEAR(IPIDG3,EE,DE,IPFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CALCULATES THE CHANGE TO THE ENERGY
C-   OF A PARTICLE ACCORDING TO THE PARTICLE ID TYPE
C-
C-   Inputs  : IPIDG3 = GEANT PARTICLE ID
C-             EE     = Energy of particle
C-   Outputs : DE     = Change in energy of particle
C-             IPFL = 1 EM, 2= MUON 3= HADRON 4= NEUTRINO
C-   Controls:
C-
C-   Created   8-JAN-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IPIDG3
      REAL    EE,DE
      INTEGER IPFL
C
      REAL  R1,R2                 ! Value from gaussian random no.
      REAL  SAMPLE_EM,CON_EM,NOISE_EM
      REAL    SIG_MU
      REAL    SAMPLE_HAD,CON_HAD,NOISE_HAD    ! energy resolution.
      REAL SIG
      REAL    RESOL,E,C,S,N
      LOGICAL ISASM
      REAL    DEFACT
      INTEGER IER
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      RESOL(E,C,S,N) = SQRT(C*C*E*E + S*S*E  + N*N)     ! RESOLUTION FUNCTION
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('SHOWERLIBRARY_RCP')
        CALL EZGET('ISA_SMEAR',ISASM,IER)
        IF ( ISASM ) THEN
          DEFACT = 1.0
        ELSE
          DEFACT = 0.
        ENDIF
        CALL EZGET('SAMPLE_EM',SAMPLE_EM,IER)
        CALL EZGET('CON_EM',CON_EM,IER)
        CALL EZGET('NOISE_EM',NOISE_EM,IER)
        CALL EZGET('SIG_MU',SIG_MU,IER)
        CALL EZGET('SAMPLE_HAD',SAMPLE_HAD,IER)
        CALL EZGET('CON_HAD',CON_HAD,IER)
        CALL EZGET('NOISE_HAD',NOISE_HAD,IER)
        CALL EZRSET
      ENDIF
C
      CALL RANNOR(R1,R2)
C
      IF(IPIDG3.EQ.1.OR.IPIDG3.EQ.2
     &      .OR.IPIDG3.EQ.3.OR.IPIDG3.EQ.7.OR.IPIDG3.EQ.17) THEN
C  PHOTON, e+,e-, pi0, Eta
        SIG=RESOL(EE,CON_EM,SAMPLE_EM,NOISE_EM)
        SIG=SIG*DEFACT
        DE=R1*SIG
        IPFL = 1
      ELSE IF(IPIDG3.EQ.5.OR.IPIDG3.EQ.6) THEN
        SIG=SIG_MU*EE*R1*DEFACT                 ! MU+, MU-
        DE=R1*SIG
        IPFL = 2
      ELSE IF(IPIDG3.EQ.4)THEN           !A NEUTRINO
        EE = 0.0                    ! no energy detected
        IPFL = 4
      ELSE            ! HADRONS
        SIG = RESOL(EE,CON_HAD,SAMPLE_HAD,NOISE_HAD)
        SIG = SIG*DEFACT
        DE=R1*SIG
        IPFL =3
      ENDIF
C
  999 RETURN
      END
