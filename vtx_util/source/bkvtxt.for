      SUBROUTINE BKVTXT(LVTXT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the VTX track bank VTXT.  The VTXT banks form a
C-                         linear chain hanging from VTRH; book the new bank
C-                         directly under VTRH.
C-
C-   Outputs : LVTXT = address of the newly created bank
C-
C-   Created  24-OCT-1991   Peter M. Grudberg
C-   Updated  14-JAN-1994   Ed Oltman Books VTXT with words 14 and 15 as REAL
C-                          rather then INTEGER 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVTXT.LINK/LIST'
C
      INTEGER LVTXT, LVTRH, GZVTRH
      INTEGER NL, NS, ND, NIO
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
      DATA NL, NS, ND / 2, 1, 21 /
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('VTXT','1B 4I -F', NIO)
      ENDIF
      LVTRH = GZVTRH()
      IF ( LVTRH .LE. 0 ) GO TO 999   ! VTRH booked by calling routine
      CALL MZBOOK(IXMAIN,LVTXT,LVTRH,-IZVTXT,'VTXT',NL,NS,ND,NIO,0)
  999 RETURN
      END
