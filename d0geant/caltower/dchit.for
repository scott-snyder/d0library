      SUBROUTINE DCHIT(ITRA,X,Y,Z,LAYER,DESTEP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate Tower indices given X,Y,Z for the dead
C-                              material in the calorimeter
C-
C-   Inputs  : ITRA    Geant track number
C-             X,Y,Z   Position of Energy Loss
C-             LAYER   Layer number from IDTYPE
C-             DESTEP  Energy lost in step
C-   Outputs :
C-   Controls:
C-
C-   Created  29-JAN-1987   Alan M. Jonckheere
C-   Updated  31-JAN-1989   Alan M. Jonckheere  Rewritten to use
C-                                      "Physics" indices
C-   Updated  27-SEP-1991   K. Wyatt Merritt   Add protection against
C-                           LOG(0) or LOG(negative) at very large eta
C-                           as suggegested by Andy Milder
C----------------------------------------------------------------------
      IMPLICIT NONE
C INPUT VARIABLES
      INTEGER ITRA
      REAL X,Y,Z,DESTEP
      INTEGER LAYER
C COORDINATES
      REAL    ETA,PHI
C CELL INDICES
      INTEGER IPHI,IETA
C
      INCLUDE 'D0$INC:GCONST.INC/LIST'
C
C  LOCAL VARIABLES
      REAL    RXYZS,RXYZ,SMALL,COSTH,C
      DATA SMALL/1.0E-6/
C
C----------------------------------------------------------------------
C
      RXYZS = X*X + Y*Y + Z*Z
      RXYZ = SQRT(RXYZS)
      IF(RXYZ.LT.SMALL) RXYZ = SMALL
      COSTH = ABS(Z)/RXYZ
      C = 1. - COSTH
      IF (C .LE. 0.) C = SMALL
      ETA = -LOG(C/(1.+COSTH))/2.
      ETA = SIGN(1.,Z)*ETA
      PHI = ATAN2(-Y,-X) + PI
      CALL CACMPR(ETA,PHI,IETA,IPHI)
C
C***********************************
C  STORE THE DATA
C
      CALL DHSTOR(ITRA,IETA,IPHI,LAYER,DESTEP)
C
C***********************************
C
  999 RETURN
      END
