      INTEGER FUNCTION GZJTSH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GZJTSH returns the link to the JTSH bank under the
C-              first JETS bank.
C-
C-   Returned value  : Zebra link to JTSH banks
C-   Inputs  : None
C-   Outputs :
C-   Controls:
C-
C-   Created  22-OCT-1991   Chip Stewart
C-   Updated  20-NOV-1991   Boaz Klima  
C-      JTSH bank doesn't exist for JETS versions 2 and higher
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C--   ZEBRA
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZJTSH.LINK/LIST'
C
C--   GEANT
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
C
C--   INTERNAL VARIABLES
      INTEGER LJET0,GZJETS
      INTEGER LJPT0
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZJTSH=0
C
C--   GET LINK TO FIRST JETS BANK
      LJET0=GZJETS()
      IF(LJET0.LE.0)  GO TO 999
C
C--   FIND LINK TO JTSH BANK UNDER FIRST JETS BANK ( IF EXISTS )
      IF ( IQ(LJET0+1).GT.1 ) THEN
        GZJTSH = 0
        GOTO 999
      ELSE
        LJPT0=LQ(LJET0-IZJTSH)
        GZJTSH=LJPT0
      ENDIF
C
  999 RETURN
      END
