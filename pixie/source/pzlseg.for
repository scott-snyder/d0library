      SUBROUTINE PZLSEG( CLANUM, SEGNUM, OFSET )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Give the next segment of class CLANUM, and 
C-               increment the OFSET. Reset to 0 if no more segments
C-
C-   Inputs  : CLANUM [I] : Class number, 0 = curent one
C-             OFSET  [I] : Control variable, number of segments to skip
C-                          in this class. Incremented. Set to -1 if no more
C-                          valid segments
C-   Outputs : SEGNUM [I] : Requested segment number, 0 if none
C-
C-   Created  20-JAN-1988   Olivier Callot
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PZCOMN.INC'
      INTEGER CLANUM, SEGNUM, OFSET
      INTEGER I, LOCLA, NBPREV
C----------------------------------------------------------------------
      LOCLA = CLANUM
      IF( LOCLA .LE. 0 ) LOCLA = CURCLA
      SEGNUM = 0
      NBPREV = OFSET
      IF( LOCLA .LE.0 .OR. LOCLA .GT. MAXCLA ) GOTO 990
      DO 10 I = 1, NMXSEG
        IF ( SEGLIS(I) .EQ. LOCLA ) THEN
          NBPREV = NBPREV-1
          IF( NBPREV .LT. 0 ) THEN
            OFSET = OFSET + 1
            SEGNUM = I
            GOTO 999
          ENDIF
        ENDIF
   10 CONTINUE
  990 OFSET = -1
  999 RETURN
      END
