      SUBROUTINE INVARIANT_MASS (N,P4,OPT,MASS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate invariant mass of N particles
C-
C-   Inputs  : N - Number of particles
C-             P4 - Their 4-momentum vector
C-             OPT - 0 = Assuming 0 masses and using only E
C-                   1 - Using all 4 P4 components
C-   Outputs : MASS - the invariant mass
C-   Controls:None
C-
C-   Created  17-OCT-1990   Chip Stewart
C-   Updated  23-DEC-1990   Boaz Klima  _ Add option with zero masses
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER N,I,J,K,OPT
      REAL    P4(4,N),P0,P,E,MASS
      REAL    P4D(4,100),PD
C----------------------------------------------------------------------
      P0 = 0
      DO I = 1, 3
        P =  0
        DO J = 1, N
          P =  P + P4(I,J)
        END DO
        P0 = P0 + P**2
      END DO
      P = SQRT (P0)
      E =  0
      DO J = 1, N
        E =  E + P4(4,J)
      END DO
      IF ( OPT.EQ.0 ) THEN
        P0 = 0
        DO J = 1, N
          PD =  0
          DO I = 1, 3
            P4D(I,J) = P4(I,J)
            PD =  PD + P4(I,J)**2
          END DO
          PD = SQRT(PD)
          P4D(4,J) = P4(4,J)
          DO K = 1, 3
            P4D(K,J) = P4D(K,J)*(P4D(4,J)/PD)
          END DO
        END DO
        DO I = 1, 3
          P =  0
          DO J = 1, N
            P =  P + P4D(I,J)
          END DO
          P0 = P0 + P**2
        END DO
        P = SQRT (P0)
        E =  0
        DO J = 1, N
          E =  E + P4D(4,J)
        END DO
      ENDIF
      MASS = SQRT (ABS( (E+P)*(E-P) ) )
  999 RETURN
      END
