      SUBROUTINE SAM_BP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read geometry constants for the collimators,
C-                         quadrupoles and beam-pipe in SAMUS region from
C-                         the banks SBPG and create the GEANT volumes
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls:
C-
C-   Created  21-APR-1991   Andrei Kiryunin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:SRCPR.INC'
      INCLUDE 'D0$LINKS:IZSBPG.LINK'
      INTEGER LSBPH,GZSBPH,LSBPG, NDATA,I
      CHARACTER*80 MSGSTR               ! Error message
C----------------------------------------------------------------------
C
C ****  Define address of the SBPG bank
C
      LSBPH = GZSBPH()
      IF (LSBPH.EQ.0) THEN
        MSGSTR = ' *** SAM_BP: header bank SBPH is absent '
        CALL INTMSG (MSGSTR)
        GOTO 999
      ENDIF
      LSBPG=LC(LSBPH-IZSBPG)
   10 IF (LSBPG.EQ.0) GOTO 999
C
C ****  Create GEANT volumes
C
      NDATA=IC(LSBPG-1)
      DO 20 I=2,NDATA
        ISRCPR(I-1)=IC(LSBPG+I)
   20 CONTINUE
      CALL VOLPOS1
      LSBPG=LC(LSBPG)
      GOTO 10
C
  999 RETURN
      END
