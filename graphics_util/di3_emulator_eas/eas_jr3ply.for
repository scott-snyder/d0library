      SUBROUTINE JR3PLY(XPTS, YPTS, ZPTS, NPTS)
C
C    Purpose:
CD   The function of this module is to display a connect sequence of 
CD   visible lines in relative world coordinates.  The parameters passed
CD   to this routine are: 
CD      XPTS --> an array of offsets defined along the X-axis.
CD      YPTS --> an array of offsets defined along the Y-axis.
CD      ZPTS --> an array of offsets defined along the Z-axis.
CD      NPTS --> number of offsets in the designated arrays.
CD   One might note that the first offset in the polyline array is drawn
CD   to from the CP.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 08-Nov-1988
CH   History:
CH      08-NOV-88  ATV  Add right handed stuff.
CH      26-JUL-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      GRFPAR-R, SEGINF-R, PRIMVR-R
C
C    Calls:
CC      ERROR, KQUEV
C
C    Next is the declaration of parameters passed to the subroutine/function.
      INTEGER NPTS
      REAL XPTS(NPTS), YPTS(NPTS), ZPTS(NPTS)
C
C    Then local declarations of variables (non-common variables).
C
      INTEGER I
      REAL XNEW, YNEW, ZNEW
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:PRIMVR.INC/LIST'
C
C    Then executable code follows
C
      IF (SEGOPN) THEN
         IF (NPTS .GT. 0) THEN
            DO I=1,NPTS
               XNEW = CPX + XPTS(I)
               YNEW = CPY + YPTS(I)
               ZNEW = CPZ + (ZPTS(I) * RIGHT)
               CALL KQUEV(XNEW, YNEW, ZNEW, 'DRAW')
            ENDDO
         ELSE
            CALL ERROR ('POLYLINE NPTS <= 0')
         ENDIF
      ELSE
         CALL ERROR('NO SEGMENT IS OPEN')
      ENDIF
      RETURN
      END
