c DEC/CMS REPLACEMENT HISTORY, Element MUTDV.FOR
c *2    30-AUG-1988 16:34:29 TAMI "Put in limits, unphysical=-99999."
c *1    19-DEC-1986 18:27:13 HEDIN "Muon Utilities initial versions"
c DEC/CMS REPLACEMENT HISTORY, Element MUTDV.FOR
      SUBROUTINE MUTDV(TDIFF,WLEN,XTDV)
CC   ==================================================
CC    returns time division XTDV in cm from center of wire
CC    given time differnce TDIFF and wire length WLEN
CC
CC    12-6-85  HEDIN
CC    DH 8/88   PUT IN LIMITS; UNPHYSICAL=99999.
CC    SI 11/91   For MC digitization version 2
CC    AT 05/92  Add jumper length to delta-t analysis
CC    DH 6/92 put those over the edge at the edge
CC    ===================================================
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER MC,IFIRST,L,GZMUD1
      REAL TDIFF,WLEN,XTDV,SPEED,XLIM
      REAL MRDELT
      INTEGER NUMVER
      REAL JUMPER, MUJUMP
      DATA NUMVER/536875808/              ! '20001320'X
      DATA SPEED/30./
      DATA XLIM/100./
C
      IF(IFIRST.EQ.0) THEN
        IFIRST=1
        L=GZMUD1(0)
        IF(IQ(L+4).EQ.1) MC=1      ! MONTE CARLO
        IF(IQ(L+4).EQ.NUMVER) MC=2 ! NEW MONTE CARLO
        JUMPER = MUJUMP()/2.0      ! Half length of jumper
      ENDIF
C
      IF(MC.EQ.2) THEN             ! NEW MONTE CARLO
        XTDV=WLEN/2.+ JUMPER - MRDELT(TDIFF)
      ELSE
        XTDV=WLEN/2.+ JUMPER -TDIFF/2.*SPEED
        IF(ABS(XTDV).GT.WLEN/2.+XLIM) XTDV=99999.
      ENDIF
      IF(XTDV.LT.9999.) THEN
        IF(XTDV.GT.WLEN/2.) XTDV=WLEN/2.
        IF(XTDV.LT.-WLEN/2.) XTDV=-WLEN/2.
      ENDIF
      RETURN
      END
