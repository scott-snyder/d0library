      FUNCTION GZSVTX()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return pointer to VTX constants header bank SVTX
C-
C-   Returned value  : Pointer to SVTX bank; 0 if nonexistent
C-
C-   Created   4-OCT-1992   Peter M. Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSTPN.LINK'
      INCLUDE 'D0$LINKS:IZSTPO.LINK'
      INCLUDE 'D0$LINKS:IZSVTX.LINK'
C
      INTEGER GZSVTX
      INTEGER LSTP
      CHARACTER*4 PATH
C----------------------------------------------------------------------
C
      IF ( LSTPH .LE. 0 ) THEN
        GZSVTX = 0
        GO TO 999
      ENDIF
C
      CALL CPATHG(PATH)
      LSTP = 0
      IF ( PATH .EQ. 'STPO' ) THEN
        LSTP = LC(LSTPH - IZSTPO)
      ELSEIF ( PATH .EQ. 'STPC' ) THEN
        LSTP = LC(LSTPH - IZSTPC)
      ELSEIF ( PATH .EQ. 'STPN' ) THEN
        LSTP = LC(LSTPH - IZSTPN)
      ENDIF
C
      IF ( LSTP .LE. 0 ) THEN
        GZSVTX = 0
        GO TO 999
      ELSE
        GZSVTX = LC(LSTP - IZSVTX)
      ENDIF
C
  999 RETURN
      END
