      SUBROUTINE JDPIDX(DCOL, DINT)
C
C    Purpose:
CD   This module sets the default polygon interior color and intensity.
CD   The parameters passed are both integers and designate the color and 
CD   intensity.  Valid values for both are (0..32767)
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
      INTEGER DCOL, DINT 
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
         CALL ERROR('JDPIDX: DEF PLY INT INDICES MAY NOT BE CHANGED')
      ELSE
         IF (DCOL .LT. 0 .OR. DCOL .GT. 32767) THEN
            CALL ERROR('JDPIDX: DCOLOR IS OUT OF RANGE (0..32767)')
         ENDIF
         IF (DINT .LT. 0 .OR. DINT .GT. 32767) THEN
            CALL ERROR('JDPIDX: DINTEN IS OUT OF RANGE (0..32767)')
         ENDIF
         DPIDCO = DCOL
         DPIDIN = DINT
      ENDIF
      RETURN
      END
