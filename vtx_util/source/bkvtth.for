      SUBROUTINE BKVTTH(LVTTH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the bank VTTH, containing hit info for a VTX
C-                         track.  This bank hangs from the VTXT bank for the
C-                         relevant track.
C-
C-   Inputs  : BKVTTH - none 
C-             BKVTTH_2ARG_INI - none
C-             BKVTTH_2ARG -LLVTXT = address of supporting VTXT bank)
C-   Outputs : BKVTTH, BKVTTH_2ARG - LVTTH = address of newly created VTTH bank
C-             BKVTTH_2ARG_INI - NONE
C-
C-   Created  24-OCT-1991   Peter M. Grudberg
C-   Updated   9-APR-1992   Peter M. Grudberg  Now UNIX compatible 
C-   Updated  28-APR-1992   Peter M. Grudberg  Remove LVTXT input to fix bug
C-   Updated   5-OCT-1992   Peter M. Grudberg  Remove room for strip hits 
C-   Updated   1-MAY-1993   Ed Oltman  Add sector map and spare words to top 
C-   Updated  24-JUN-1994   Al Clark  Add new entry points for 
C-                            1) separate initialization, and 
C-                            2) 2-argument call. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVTXT.LINK'
      INCLUDE 'D0$LINKS:IZVTTH.LINK/LIST'
C
      INTEGER LVTXT, LVTTH, LVTRH, GZVTRH, LLVTXT
      INTEGER NL, NS, ND, NIO
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
      DATA NL, NS, ND / 0, 0, 101 /
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('VTTH','5I / 1B 1I 2F',NIO)
      ENDIF
      LVTRH = GZVTRH()
C
C ****  The supporting VTXT bank for the future VTTH bank is the most recently
C ****  booked VTXT, that is, the one right under VTRH
C
      IF ( LVTRH .LE. 0 ) GO TO 999
      LVTXT = LQ(LVTRH - IZVTXT)
      IF ( LVTXT .LE. 0 ) GO TO 999
      CALL MZBOOK(IXMAIN,LVTTH,LVTXT,-IZVTTH,'VTTH',NL,NS,ND,NIO,0)
  999 RETURN
C
C*************************
C
      ENTRY BKVTTH_2ARG_INI  ! Just call MZFORM if it hasn't been before.
C
C**   Need separate initialization call for 2-argument call, since the call
C**   to MZFORM can trigger a garbage collection, and invalidate the pointer
C**   LLVTXT.
C**   
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('VTTH','5I / 1B 1I 2F',NIO)
      ENDIF
  998 RETURN
C
C******************************
C
      ENTRY BKVTTH_2ARG( LLVTXT, LVTTH)
C
C**     THE SUPPORTING VTXT BANK is specified by LLVTXT
C
      IF ( LLVTXT .LE. 0 ) GO TO 997
      CALL MZBOOK(IXMAIN,LVTTH,LLVTXT,-IZVTTH,'VTTH',NL,NS,ND,NIO,0)
  997 RETURN
      END
