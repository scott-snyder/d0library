      INTEGER FUNCTION GZSMAG ( NMAG )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get link to bank SMAG with information about
C-                         the SAMUS magnet #NMAG
C-
C-   Returned value  : Link to necessary bank SMAG (to the first bank if
C-                     NMAG equals zero)
C-   Inputs  : NMAG - the SAMUS magnet number
C-   Outputs : None
C-   Controls: 
C-
C-   Created  23-APR-1991   Andrei Kiryunin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSMAG.LINK'
      INTEGER NMAG,LSMAH,GZSMAH
      CHARACTER*80 MSGSTR               ! Error message
C----------------------------------------------------------------------
C
      GZSMAG = 0
C
      LSMAH = GZSMAH()
      IF ( LSMAH.GT.0 ) THEN
        GZSMAG=LC(LSMAH-IZSMAG)
        IF (NMAG.EQ.0) GOTO 999
  10    IF (GZSMAG.EQ.0) GOTO 999
        IF (IC(GZSMAG+2).EQ.NMAG) GOTO 999
        GZSMAG=LC(GZSMAG)
        GOTO 10
      ELSE
        MSGSTR = ' *** GZSMAG: bank SMAH is absent '
        CALL INTMSG (MSGSTR)
        GOTO 999
      ENDIF
C
  999 RETURN
      END
