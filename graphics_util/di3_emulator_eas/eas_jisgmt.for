      SUBROUTINE JISGMT(NAMSG, ARRAY)
C
C    Purpose:
CD   This routine returns information on a particular segment. The 
CD   parameter NAMSG is the segment of interest and ARRAY is an array
CD   that will contain information if the segment exists; otherwise the
CD   array will be all zeroes.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 10-Oct-1988
CH   History:
CH      10-OCT-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      SEGINF-R
C
C    Calls:
CC      NONE.
C
C    Next is the declaration of parameters passed to the subroutine/function.
      INTEGER NAMSG, ARRAY(*)
C
C    Then local declarations of variables (non-common variables).
C
      INTEGER I, LOCAT
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
C
C    Then executable code follows
C
      DO 10 I=1,19
         ARRAY(I) = 0
   10 CONTINUE
      LOCAT = 0
      DO 20 I=1,NSEGS
         IF (SEGINF(1,I) .EQ. NAMSG) LOCAT = I
   20 CONTINUE
      IF (LOCAT .NE. 0) THEN
         ARRAY(1) = 1
         I = SEGINF(2,LOCAT)
         ARRAY(5) = I .AND. 3
         I = I / 4
         ARRAY(4) = I .AND. 1
         I = I / 2
         ARRAY(2) = I .AND. 1
         ARRAY(3) = SEGINF(3,LOCAT) .AND. 65535
         ARRAY(6) = SEGINF(3,LOCAT) / 65536
      ENDIF
      RETURN
      END
