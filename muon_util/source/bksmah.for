      INTEGER FUNCTION BKSMAH ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create the Zebra bank SMAH and hanging under 
C-                         it banks SMAG with static parameters for the 
C-                         SAMUS magnets
C-
C-   Returned value  : bank SMAH address (or zero if something is bad)
C-   Inputs  : None
C-   Outputs : None
C-   Controls: 
C-
C-   Created  22-APR-1991   Andrei Kiryunin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSMAH.LINK'
      INTEGER LSMAH, GZSMAH, LSSAM, GZSSAM, NDATA, NFORM, 
     &IERR, IVAL, IAR(100), I, J, NCHAR
      PARAMETER( NDATA = 10 )
      CHARACTER*32 CHAR(100)
      CHARACTER*80 MSGSTR               ! Error message
C----------------------------------------------------------------------
C
      BKSMAH = 0
C
C ****  Check existence of the SSAM bank
C
      LSSAM = GZSSAM()
      IF (LSSAM.EQ.0) THEN
        MSGSTR = ' *** BKSMAH: supporting bank SSAM does not exist '
        CALL INTMSG (MSGSTR)
        GOTO 999
      ENDIF
C
C ****  Create bank SMAH
C
      CALL MZFORM ( 'SMAH','2I 1F 7I',NFORM )
      CALL MZBOOK ( IDVSTP, LSMAH, LSSAM, -IZSMAH, 'SMAH', 2, 2,
     &              NDATA, NFORM, 0 )
C
C ****  Read in RCP file with necessary static parameters
C
      CALL INRCP ('SAMUS_MAGNET_RCP',IERR)
      IF (IERR.NE.0) THEN
        MSGSTR = ' *** BKSMAH: error openning of the RCP file '
        CALL INTMSG (MSGSTR)
        GOTO 999
      ENDIF
      CALL EZPICK ('SAMUS_MAGNET_RCP')
C
C ****  Get the list with arrays' names
C
      CALL EZ_GET_CHARS ('SAMUS_MAGNETS',NCHAR,CHAR,IERR)
      IF (IERR.NE.0) THEN
        MSGSTR = ' *** BKSMAH: error during EZ_GET_CHAR '
        CALL INTMSG (MSGSTR)
        GOTO 999
      ENDIF
      DO 10 I=1,NCHAR
C
C ****  Get values from the select array
C
        CALL EZGETA (CHAR(I),0,0,0,IVAL,IERR)
        IF (IERR.NE.0) THEN
          MSGSTR = ' *** BKSMAH: error during EZGETA '
          CALL INTMSG (MSGSTR)
          GOTO 999
        ENDIF
        CALL EZGET (CHAR(I),IAR,IERR)
        IF (IERR.NE.0) THEN
          MSGSTR = ' *** BKSMAH: error during EZGET '
          CALL INTMSG (MSGSTR)
          GOTO 999
        ENDIF
C
C ****  Create bank SMAG with data from IAR
C
        CALL BKSMAG (IVAL,IAR)
C
   10 CONTINUE
C
      CALL EZRSET
      LSMAH = GZSMAH()
      BKSMAH = LSMAH
C
  999 RETURN
      END
