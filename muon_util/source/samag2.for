      SUBROUTINE SAMAG2 ( NMAG, VECLOC, VECT )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Rotate vector from local system of the SAMUS
C-                         magnet to the global system
C-
C-   Inputs  : NMAG - number of the magnet
C-             VECLOC - vector in the local system of the SAMUS magnet
C-   Outputs : VECT - vector in the global system
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
C ****  Get address of the SMAG bank for the magnet #NMAG
C
      LSMAG=GZSMAG(NMAG)
      IF (LSMAG.EQ.0) THEN
        MSGSTR = ' *** SAMAG2: needed bank SMAG is absent '
        CALL INTMSG (MSGSTR)
        GOTO 999
      ENDIF
C
C ****  Make (if need) rotation
C
      IF ( IC(LSMAG+25) .EQ. 0 ) THEN
        CALL UCOPY (VECLOC(1),VECT(1),3)
      ELSE
        DO 20 J=1,3
          V=0.
          DO 10 I=1,3
            V=V+VECLOC(I)*C(LSMAG+12+I*3+J)
   10     CONTINUE
          VECT(J)=V
   20   CONTINUE
      ENDIF
C
  999 RETURN
      END
