      SUBROUTINE MULTSC(S, CRADL, GETOT, P, NSD, MSA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Retuerns average Mult. scattering angles in the
C-                         planes perpendicular to the velocity axis.
C-
C-   Inputs  : 
C-              S    : Total length travelled
C-              CRADL: Compound radiation legth
C-              GETOT: Total kinetic energy
C-              P    : Particle momentum
C-              NSD  : Number of standard deviations (REAL)
C-
C-   Outputs :   MSA  ; Average multiple scattering angle. (equal in both
C-                      planes)
C-   Controls: 
C-
C-   Created  20-APR-1990    SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL S, CRADL, GETOT, P, NSD, MSA
      REAL DA, A, DX, DY, DR
C
      A = SQRT( S / CRADL )
      DA = 0.015 * NSD * GETOT * A / ( P * P )
      DX = 0.5 * ( DA + NSD / SQRT(3.0) ) * S * DA
      DY = DX
      DR = SQRT(DX*DX + DY*DY)
      MSA = DR / S
C
  999 RETURN
      END
