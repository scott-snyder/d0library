      SUBROUTINE TRDISA (RMIN,ZMIN,RMAX,ZMAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Define the ficucial volume for a "central" track
C-               Stollen from CDCISA.FOR written by Ghita
C-   Inputs  : RMIN,RMAX
C-             ZMIN,ZMAX
C-   Outputs : 
C-   Controls: 
C-
C-   Created  24-FEB-1989   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GEOMTR.INC/LIST'
      INCLUDE 'D0$INC:TRDVOL.INC/LIST'
C
      REAL RMIN, ZMIN, RMAX, ZMAX
      INTEGER NISA, LDRFT,IERR
C----------------------------------------------------------------------
C
C ****  Define the limits of the TRD RMIN, ZMIN, RMAX, ZMAX 
C
C ****  Creates a fiducial volume in the CDC
C
      RMIN = radwin(1)
      RMAX = radext(3)
      ZMIN =zact(1)
      ZMAX = ZMIN
C
  999 RETURN
      END
