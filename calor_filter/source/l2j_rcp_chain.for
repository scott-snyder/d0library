      SUBROUTINE L2J_RCP_CHAIN( NBANKS, BANK_NAMES, IER )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : In preparation for downloading to Level2 we
C-                         hang all SRCP banks under one link of SL2H.
C-                         The STP tree underneath SL2H is then written to
C-                         a file and downloaded to Level 2.
C-
C-                         This routine will add the specified RCP banks to
C-                         a chain of SRCP banks to is hanging under SL2H
C-
C-   Inputs  :  [I]     NBANKS        # of banks
C-              [C*]    BANK_NAMES(*)  Array of names of RCP banks
C-   Outputs :  [I]     IER           Status flag
C-                                    0 = OK
C-                                    -1= Unsuccessful: Cannot book SL2H
C-                                    -2= Could not do a EZCHAIN
C-                                    -3= Could not do a EZSHUNT
C-   Controls:
C-
C-   Created  25-JUL-1992   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$PARAMS:L2JETS.PARAMS'
      INCLUDE 'D0$LINKS:IZSRCP.LINK'
      INTEGER LSL2H, GZSL2H, NBANKS
      CHARACTER*(*) BANK_NAMES(*)
      INTEGER IER, LOFF, LLINK
      LOGICAL EZERR
C----------------------------------------------------------------------

C
C---Book SL2H if not already there
C
      LSL2H = GZSL2H()
      IF ( LSL2H .LE. 0 ) CALL BKSL2H( LSL2H )
      IF ( LSL2H .LE. 0 ) THEN
        IER = -1
        CALL ERRMSG('L2J_RCP_CHAIN','No SL2H',
     &    'Unable to book SL2H bank','F')
        RETURN
      ENDIF

      CALL EZCHAIN(BANK_NAMES,NBANKS)        ! Chain them together under #1
      IF (EZERR(IER)) THEN
        IER = -2
        CALL ERRMSG('L2J_RCP_CHAIN','Err EZCHAIN',
     &    'Unable to EZCHAIN banks','F')
        RETURN
      ENDIF
C
C---Now we must find out where to hang this chain. Either hang it below
C---SL2H (IZL2JETS_RCP link ) or hang it after the last SRCP bank if there
C---are banks already there.
C
      LLINK = GZSL2H()
      LOFF  = IZL2JETS_RCP
   10 CONTINUE
      IF ( LC( LLINK - LOFF )  .GT. 0 ) THEN
        LLINK = LC( LLINK -LOFF )
        LOFF  = IZSRCP
        GOTO 10
      ENDIF

C
C---Now move the new chain of SRCP bank underneath SL2H
C
      CALL EZSHUNT(BANK_NAMES(1),LLINK,LOFF) ! Move it here
      IF (EZERR(IER)) THEN
        IER = -3
        CALL ERRMSG('L2J_RCP_CHAIN','Error in EZSHUNT',
     &    'Could not EZSHUNT banks ','F')
        RETURN
      ENDIF

  999 RETURN
      END
