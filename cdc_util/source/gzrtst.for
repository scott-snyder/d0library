      FUNCTION GZRTST()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to RTST bank
C-
C-   Returned value  : Link to 1st element of RTST linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-OCT-1995 09:20:30.95  Jadwiga Warchol
C-   Modified 15-NOV-1995 Jadwiga Warchol, RTST now hangs from UPGD
C-            to accomodate preshower
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZRTST
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZRTST.LINK'
C----------------------------------------------------------------------
      INTEGER LUPGD,GZUPGD
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZRTST = 0
C
C--   GET LINK TO SUPPORTING UPGD BANK
      LUPGD = GZUPGD()
C
      IF ( LUPGD .LE. 0 )  GOTO 999
C
C--   FIND LINK TO RTST
      GZRTST = LQ(LUPGD-IZRTST)
C
  999 RETURN
      END
