      SUBROUTINE SETCHK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialization routine for COMPACK
C-                         VAX-specific
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: Returns immediately if already called
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-
C-	27-OCT-1989 Penelope Constanta-Fanourakis
C-	    Added support for alternating between command file input
C-	    terminal input. A LUN is assigned in this routine for
C-	    duration of the program that a command file will be opened
C-	    on. The COMLUN is any LUN other than 5 (used for terminal
C-	    input).
C-      Updated 17-SEP-91 Herbert Greenlee
C-
C-               4-NOV-92 Soren G. Frederiksen
C-                        For UNIX machines check to see if "nosmg" was
C-                        on the command line, and if it was then turn
C-                        off SMG. If SMG is off, then do NOT call REASET
C-   Updated  24-NOV-1992   Jan Hoftun  Take out conditional on call to REASET
C-                                      again (Causes BAD side effects)
C-   Updated  24-Nov-1992   Herbert Greenlee - Put Soren's changes into
C-                                             machine block.
C-
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INTEGER ISTAT
      LOGICAL GETDEV,PARSOK
      CHARACTER COMAND*256
      INTEGER*2 ISIZ
C&IF VAXVMS
      INTEGER LIB$GET_FOREIGN,CLI$DCL_PARSE,SYS$EXIT,I,
     &        LIB$ESTABLISH
      EXTERNAL COMPACKCLI_TABLE,NULHDL,COMPACK$_FACILITY
C&ELSE
C&      INCLUDE 'D0$INC:SSDEF.DEF'
C&      INTEGER*4 LIB$GET_FOREIGN             ! RUN-TIME LIBRARY MODULES
C&      INTEGER*4 STATUS                      ! RUN-TIME LIB. CALL RETURN STATUS
C&ENDIF
      INTEGER  IERR
C----------------------------------------------------------------------
      IF(.NOT.SETDON) THEN               ! Could be called more than once
	CALL GTUNIT(555,COMLUN,IERR)	 ! Get LUN for command file input
	IF (IERR .NE. 0) THEN		 ! If no unit is available donot
	   COMLUN = 0			 ! allow command file input
	END IF
	INPLUN=5		  ! Default to terminal input
        SETDON=.TRUE.
        SMGON=.TRUE.              ! Default to use SMG
        ENDFLG=.TRUE.             ! Default to confirm for exit
        BEGJOU=.FALSE.            ! Default NO journaling
        PARSOK=.FALSE.
        UPRLEV=0                  ! Start with no levels
        MAILEV=0                  ! Start with no main level
        DEFDIR=' '
        NUMCOL=2
        LINSPA=2
C&IF VAXVMS
        ISTAT=LIB$ESTABLISH(NULHDL)  ! Establish special error without traceback
C
C     Check if qualifiers were entered on the command line
C
        ISTAT=LIB$GET_FOREIGN(COMAND,,ISIZ,)
        I=INDEX(COMAND,' ')
        IF(ISTAT.AND.(I.GT.1.OR.I.EQ.0)) THEN             ! Check if anything extra on command line
          PARSOK=.TRUE.
          ISTAT=CLI$DCL_PARSE('COMPACK '//COMAND(1:ISIZ),
     *         COMPACKCLI_TABLE,,,)
          IF(.NOT.ISTAT) THEN
            CALL EXIT(1)
          ENDIF
          CALL GINCOM(COMAND(1:ISIZ))       !GET possible command on command-line
          CALL GFULSC       !Check if /FULLSCREEN was present
          CALL GNOSMG       !Check if /NOSMG was present
        ENDIF
C&ELSE
C&        STATUS = LIB$GET_FOREIGN(COMAND,,ISIZ,)
C&        IF ( STATUS .NE. SS$_NORMAL ) CALL LIB$SIGNAL(%VAL(STATUS))
C&        IF(INDEX(COMAND,'NOSMG').NE.0
C&     &                  .OR. INDEX(COMAND,'nosmg').NE.0) THEN
C&          CALL GNOSMG
C&        ENDIF
C&ENDIF
C
C     Set up terminal the first time around
C
C&IF VAXVMS
        CALL REASET
C&ELSE
C&        IF(SMGON) CALL REASET
C&ENDIF
C&IF VAXVMS
C
C     Now ready to check if a command file should be read before
C     going to normal mode
C
        IF(PARSOK) THEN
          CALL GSPLIT       !Check if /SPLITMODE was present
          CALL GSTATU       !Check if /STATUSPART was present
          CALL CMPK_GCONFI       !Check if /CONFIRM_END was present
          CALL GJOURN       !Check if /JOURNAL was present
          CALL GCOMND       !Check if /COMMAND was present
          CALL GUSERM       !Check if /USERMODE was present
        ENDIF
C&ENDIF
        IF(.NOT.SMGON) THEN
          FULSCR=.FALSE.
        ENDIF
      ENDIF
      RETURN
      END
