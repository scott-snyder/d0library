      SUBROUTINE REASET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set up for screen manipulation via SMG routines
C-                         VAX-specific
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: Many of the flags in /COMNUM/ and /SMGCOM/ are set here
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-   Updated 17-SEP-1991   Herbert Greenlee
C-   Modified 14-AUG-1992   sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
C&IF VAXVMS
      INCLUDE '($SMGDEF)'
C&ELSE
C&      INCLUDE 'D0$INC:SMGDEF.DEF'
C&ENDIF
      LOGICAL GETDEV
      LOGICAL ISTAT,SMG$CREATE_PASTEBOARD
      LOGICAL SMG$CREATE_VIRTUAL_DISPLAY,SMG$PASTE_VIRTUAL_DISPLAY
      LOGICAL SMG$CREATE_VIRTUAL_KEYBOARD,TRALOG
      INTEGER OUTLEN
      CHARACTER*132 OUTNAM
C----------------------------------------------------------------------
C&IF VAXVMS
      ISTAT=TRALOG('SYS$OUTPUT',OUTNAM,OUTLEN)
      IF(ISTAT.AND.(OUTNAM(1:4).EQ.CHAR(27)//CHAR(0)//CHAR(2)//
     &   CHAR(129).OR.OUTNAM(1:4).EQ.CHAR(27)//CHAR(0)//CHAR(3)//
     &   CHAR(129))) THEN         ! VMS version dependent escape sequence
                                  ! indicating that this terminal can handle SMG
                                  ! I/O
C&ELSE
C&      IF(.TRUE.)THEN
C&ENDIF
        ISTAT=SMG$CREATE_PASTEBOARD(PASTID,'SYS$OUTPUT',
     &      PBROWS,PBCOLS,1)
        PBSAVE=PBROWS                        !Make sure to save max. # of lines
        IF(.NOT.ISTAT) THEN
          CALL MSGSCR(ISTAT,'PASTEBOARD-->')
        ELSEIF(SMGON.AND.GETDEV()) THEN
          ISTAT=SMG$CREATE_VIRTUAL_DISPLAY(PBROWS,PBCOLS,MAINID,
     &                                     %VAL(0),%VAL(0),%VAL(0))
          IF(.NOT.ISTAT) THEN
            CALL MSGSCR(ISTAT,'VIRTUAL_DISPLAY-->')
          ELSE
            ISTAT=SMG$PASTE_VIRTUAL_DISPLAY(MAINID,PASTID,1,1,%VAL(0))
            IF(.NOT.ISTAT) THEN
              CALL MSGSCR(ISTAT,'PASTE_VIRTUAL_DISPLAY-->')
            ELSE
              ISTAT=SMG$CREATE_VIRTUAL_KEYBOARD(KEYID)
              IF(.NOT.ISTAT) THEN
                CALL MSGSCR(ISTAT,'VIRTUAL_KEYBOARD-->')
              ELSE
                TRMFLG=.TRUE.               ! Indicate that we ready for SMG I/O
                SPLLIN=PBROWS/2-3           ! Default split screen to half screen
                STALIN=3                    ! Default message screen to
C                                         ! three lines
                CALL BROADC(.TRUE.)         ! Turn on broadcast trapping
              ENDIF
            ENDIF
          ENDIF
        ELSE
          TRMFLG=.TRUE.                        ! Indicate reading from terminal
        ENDIF
      ELSE
        PBCOLS=80
        PBROWS=10000
        TRMFLG=.TRUE.                        ! Indicate reading from terminal
      ENDIF
      IF(.NOT.ONEFLG) THEN
        IF(SMGON) SMGON=GETDEV()
      ELSE
        TRMFLG=.TRUE.       !Indicate terminal reading in ONEFLG mode
      ENDIF
      RETURN
      END
