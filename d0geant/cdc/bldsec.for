      SUBROUTINE BLDSEC ( HIT, NDIM, NHIT, IFADC )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create and fill the Zebra structure
C-                         CDCH -- DLYR -- DSEC from the hits HIT given
C-                         by GEANT ( JHITS )
C-
C-   Inputs  : HIT ( NDIM, NHIT ) = Array containing the data stored in
C-                                  JHITS for each hit
C-             IFADC(3)           = Layer, sector, wire
C-                             
C-   Outputs : none; The bank DSEC is filled
C-
C-   Created  27-JAN-1988   Ghita Rahal-Callot
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

C----------------------------------------------------------------------
      INTEGER NDIM, NHIT, IFADC(*)
      REAL HIT ( NDIM, * )
C
      INTEGER NWORD, NMAX
      PARAMETER (NWORD = 18, NMAX = 50)
      INTEGER IHIT
      REAL DHITS ( NWORD, NMAX )
      INTEGER IDHITS (NWORD, NMAX )
      EQUIVALENCE ( DHITS, IDHITS )
C
C
C======================================================================
      IF ( NHIT .LE. 0 ) GO TO 999
C
      DO 100 IHIT=1,NHIT
C
C ****  Get the array DHITS ( to fill the ZEBRA hits banks )
C
        CALL CDHITS( HIT(1,IHIT), DHITS(1,IHIT), IDHITS(1,IHIT), IFADC)
  100 CONTINUE
C
C ****  Put the hits in Zebra Structure CDCH -- DLYR -- DSEC
C
      CALL FIDSEC ( DHITS, NHIT, IFADC )
C
  999 CONTINUE
      RETURN
      END
