      SUBROUTINE DVECDA ( HIT, NDIM, NHIT, TIME, ERTIME, 
     &                    ITIMES, ICELL, DATAS )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create and fill the Zebra structure
C-                         (CDCH -- DLYR -- DSEC)--DCDA from the hits HIT
C-                         given by GEANT ( JHITS ) For the Sense Wires
C-
C-   Inputs  : HIT ( NDIM, NHIT ) =  The NDIM informations stored for the NHIT
C-                                    hits in JHIT
C-             TIME  ( NHIT )     =   Drift time
C-             ERTIME( NHIT )     =   Error on the Drift time
C-             ITIMES (NHIT )     =   Time ordering of the hits
C-             ICELL              =   Sense wire or Delay Line end number
C-                                   
C-   Outputs : DATAS (18, NHIT)   =   Informations on each hit to be loaded
C-                                    in DCDA
C-
C-   Created  27-JAN-1988   Ghita Rahal-Callot
C-   Updated  24-FEB-1991   Qizhong Li-Demarteau  fix array size for DATAS 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

C----------------------------------------------------------------------
      INTEGER NDIM, NHIT, ICELL, ITIMES(*)
      REAL HIT ( NDIM, NHIT ), TIME(*), ERTIME(*)
C
      INTEGER NWORD, NMAX
      PARAMETER (NWORD = 18, NMAX = 50)
      INTEGER I, IHIT
      REAL DATAS ( NWORD, 2*NMAX )
      INTEGER IVARI
      REAL FIVARI
      EQUIVALENCE (IVARI, FIVARI)
C
C
C======================================================================
C
C  ****  Loop through hits in this cell
C
      IF ( NHIT .LE. 0 ) GO TO 999
      DO 100 IHIT=1,NHIT
        I = ITIMES ( IHIT )
        DATAS ( 1, IHIT )  = ICELL
        DATAS  ( 2, IHIT ) = TIME  ( I )
        DATAS  ( 3, IHIT ) = HIT   ( 7, I )
        DATAS  ( 4, IHIT ) = HIT   ( 8, I )
        DATAS  ( 5, IHIT ) = HIT   ( 7, I )
        DATAS  ( 6, IHIT ) = ERTIME  ( I )
        DATAS  ( 7, IHIT ) = SQRT  ( DATAS ( 5, IHIT ) )
        DATAS ( 8, IHIT ) = 0
        IVARI = IFIX (HIT   ( 9, I ))
        DATAS ( 9, IHIT ) = FIVARI
  100 CONTINUE
  999 CONTINUE
      RETURN
      END
