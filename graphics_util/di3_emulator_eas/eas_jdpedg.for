      SUBROUTINE JDPEDG(DVALUE)
C
C    Purpose:
CD   This module sets the default polygon edge style. The parameter
CD   passed is an integer value (0..32767) indicates the current edge
CD   for all subsequently open segments.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 20-Oct-1988
CH   History:
CH      20-OCT-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      SEGINF-R, PLGATT-W
C
C    Calls:
CC      ERROR.
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      INTEGER DVALUE
C
C    Then local declarations of variables (non-common variables).
C
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:PLGATT.INC/LIST'
C
C    Then executable code follows
C
      IF (SEGSOP) THEN
         CALL ERROR('JDPEDG: DEF PLY EDGE MAY NOT BE CHANGED')
      ELSE
         IF (DVALUE .LT. 0 .OR. DVALUE .GT. 32767) THEN
            CALL ERROR('JDPEDG: DVALUE OUT OF RANGE (0..32767)')
         ENDIF
         DPEDGE = DVALUE
      ENDIF
      RETURN
      END
