      SUBROUTINE MUOHER(IHMUOH,ERRT1,ERRT2,ERRDT1,ERRDT2,ERRPAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns the errors for a given MUOH
C-                         hit. note that an error of 999 means
C                          that either there wasn't any information
C                          (i.e. no 2 time) or that the hit was
C                          unphysical
C-   Inputs  : IHMUOH - which MUOH hit
C-   Outputs : ERRT1,ERRT2  errors on drift time 1 and 2
C-             ERRDT1.ERRDT2 errors on time division 1 and 2
C-             ERRPAD  error on vernier pad
C-   Created  25-JAN-1992   David Hedin. semi-dummy for now
C-   Modified 23-Sep-1993   Darien Wood, use MU**ER routines
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IHMUOH
      REAL ERRT1,ERRT2,ERRDT1,ERRDT2,ERRPAD
      REAL MUDRER,MUDTER,MUPDER
      EXTERNAL MUDRER,MUDTER,MUPDER
C----------------------------------------------------------------------
      ERRT1 = MUDRER(IHMUOH,1)
      ERRT2 = MUDRER(IHMUOH,2)
      ERRDT1 = MUDTER(IHMUOH,1)
      ERRDT2 = MUDTER(IHMUOH,2)
      ERRPAD = MUPDER(IHMUOH)
C----------------------------------------------------------------------
  999 RETURN
      END
