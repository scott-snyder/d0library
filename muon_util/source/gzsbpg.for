      INTEGER FUNCTION GZSBPG ( NVOL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get link to bank SBPG number NVOL with information
C-                         about the corresponding GEANT volume for description
C-                         the collimators, quadrupoles or beam-pipe in
C-                         SAMUS region
C-
C-   Returned value  : Link to necessary bank SBPG (to the first bank if
C-                     NVOL equls zero)
C-   Inputs  : NVOL - the needed volume number
C-   Outputs : None
C-   Controls: 
C-
C-   Created  23-APR-1991   Andrei Kiryunin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSBPG.LINK'
      INTEGER NVOL,LSBPH,GZSBPH, I
      CHARACTER*80 MSGSTR               ! Error message
C----------------------------------------------------------------------
C
      GZSBPG = 0
C
      LSBPH = GZSBPH()
      IF ( LSBPH.GT.0 ) THEN
        GZSBPG=LC(LSBPH-IZSBPG)
        IF (GZSBPG.EQ.0) GOTO 999
        IF (NVOL.LT.2) GOTO 999
        DO 10 I=2,NVOL
          GZSBPG=LC(GZSBPG)
          IF (GZSBPG.EQ.0) GOTO 999
   10   CONTINUE
      ELSE
        MSGSTR = ' *** GZSBPG: bank SBPH is absent '
        CALL INTMSG (MSGSTR)
        GOTO 999
      ENDIF
C
  999 RETURN
      END
