      INTEGER FUNCTION GZMSGH(IDUMMY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Pointer to MSGH bank
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   2-JAN-1992   Atsushi Taketani
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK/LIST'
      INCLUDE 'D0$LINKS:IZSMUO.LINK/LIST'
      INCLUDE 'D0$LINKS:IZMSGH.LINK/LIST'
      INTEGER LSTPC
C
      INTEGER IDUMMY
C
C----------------------------------------------------------------------
      GZMSGH = 0
C -- go through chain of banks in tree structure... (temporary)
      LSTPC=LC(LSTPH-IZSTPC)
         IF(LSTPC.EQ.0) GO TO 999
      LSMUO=LC(LSTPC-IZSMUO)
         IF(LSMUO.EQ.0) GO TO 999
      GZMSGH=LC(LSMUO-IZMSGH)
C
  999 RETURN
      END
