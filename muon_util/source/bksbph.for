      INTEGER FUNCTION BKSBPH ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create the Zebra bank SBPH and hanging under 
C-                         it banks SBPG with static parameters for the 
C-                         collimators, quadrupoles and beam-pipe in 
C-                         SAMUS region
C-
C-   Returned value  : bank SBPH address (or zero if something is bad)
C-   Inputs  : None
C-   Outputs : None
C-   Controls: 
C-
C-   Created  19-APR-1991   Andrei Kiryunin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSBPH.LINK'
      INTEGER LSBPH, GZSBPH, LSSAM, GZSSAM, NDATA, NFORM, 
     &IERR, IVAL, IAR(100), I, J, NCHAR
      PARAMETER( NDATA = 10 )
      CHARACTER*32 CHAR(100)
      CHARACTER*80 MSGSTR               ! Error message
C----------------------------------------------------------------------
C
      BKSBPH = 0
C
C ****  Check existence of the SSAM bank
C
      LSSAM = GZSSAM()
      IF (LSSAM.EQ.0) THEN
        MSGSTR = ' *** BKSBPH: supporting bank SSAM does not exist '
        CALL INTMSG (MSGSTR)
        GOTO 999
      ENDIF
C
C ****  Create bank SBPH
C
      CALL MZFORM ( 'SBPH','2I 1F 7I',NFORM )
      CALL MZBOOK ( IDVSTP, LSBPH, LSSAM, -IZSBPH, 'SBPH', 1, 1,
     &              NDATA, NFORM, 0 )
C
C ****  Read in RCP file with necessary static parameters
C
      CALL INRCP ('SAMUS_BEAM_RCP',IERR)
      IF (IERR.NE.0) THEN
        MSGSTR = ' *** BKSBPH: error openning of the RCP file '
        CALL INTMSG (MSGSTR)
        GOTO 999
      ENDIF
      CALL EZPICK ('SAMUS_BEAM_RCP')
C
C ****  Get the list with arrays' names
C
      CALL EZ_GET_CHARS ('SAMUS_BEAM_VOLUMES',NCHAR,CHAR,IERR)
      IF (IERR.NE.0) THEN
        MSGSTR = ' *** BKSBPH: error during EZ_GET_CHAR '
        CALL INTMSG (MSGSTR)
        GOTO 999
      ENDIF
      DO 10 I=1,NCHAR
C
C ****  Get values from the select array
C
        CALL EZGETA (CHAR(I),0,0,0,IVAL,IERR)
        IF (IERR.NE.0) THEN
          MSGSTR = ' *** BKSBPH: error during EZGETA '
          CALL INTMSG (MSGSTR)
          GOTO 999
        ENDIF
        CALL EZGET (CHAR(I),IAR,IERR)
        IF (IERR.NE.0) THEN
          MSGSTR = ' *** BKSBPH: error during EZGET '
          CALL INTMSG (MSGSTR)
          GOTO 999
        ENDIF
C
C ****  Create bank SBPG with data from IAR
C
        CALL BKSBPG (IVAL,IAR)
C
   10 CONTINUE
C
      CALL EZRSET
      LSBPH = GZSBPH()
      IC(LSBPH+9)=NCHAR                 ! Store number of GEANT volumes
      BKSBPH = LSBPH
C
  999 RETURN
      END
