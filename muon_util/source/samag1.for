      SUBROUTINE SAMAG1 ( VECT, VECLOC, NMAG )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Invert coordinate of the point in the SAMUS
C-                         magnet in the global system to the local system 
C-                         of this magnet and define number of this magnet
C-
C-   Inputs  : VECT - coordinate of the point in the global system
C-   Outputs : VECLOC - coordinate of the point in the local system of
C-                      the SAMUS magnet
C-             NMAG - number of the magnet
C-   Controls: 
C-
C-   Created  23-APR-1991   Andrei Kiryunin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER NMAG
      REAL    VECT(3),VECLOC(3)
      INTEGER LSMAG,GZSMAG, I,J
      REAL    V
      CHARACTER*80 MSGSTR               ! Error message
C----------------------------------------------------------------------
C
C ****  Define number of the SAMUS magnet with this point
C
      DO 10 NMAG=1,2
        LSMAG=GZSMAG(NMAG)
        IF (LSMAG.GT.0) THEN
          IF (C(LSMAG+14)*VECT(3) .GT. 0.0) GOTO 20
        ENDIF
   10 CONTINUE
      GOTO 900
C
C ****  Make transformation from global system
C
   20 DO 30 I=1,3
        VECLOC(I)=VECT(I)-C(LSMAG+11+I)
   30 CONTINUE
C
C ****  Make (if need) rotation
C
      IF ( IC(LSMAG+25) .EQ. 0 ) GOTO 999
      DO 50 I=1,3
        V=0.
        DO 40 J=1,3
          V=V+VECLOC(J)*C(LSMAG+12+I*3+J)
   40   CONTINUE
        VECLOC(I)=V
   50 CONTINUE
      GOTO 999
C
  900 MSGSTR = ' *** SAMAG1: can not define number of SAMUS magnet '
      CALL INTMSG (MSGSTR)
C
  999 RETURN
      END
