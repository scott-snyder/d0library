      SUBROUTINE MENEXG(COMOUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Main control of menu. Setup display and loop
C-                         until a command has been entered. Handle all
C-                         change of mode requests, HELP requests etc.
C-
C-   Inputs  : None
C-   Outputs : COMOUT: Unique command identifier to be passed back to
C-                     dispatch loop
C-   Controls: PF, POS and other variables in /COMNUM/ may change
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-   Updated   8-AUG-1990   Harrison B. Prosper
C-      Added event_flag code
C-   Updated  18-OCT-1991   Lupe Howell The reset PF without using WAIFLG
C-   Updated  20-OCT-1991   Harrison B. Prosper
C-      Check for -ve PF key
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) COMOUT
C
C     Get definitions for COMPACK
C
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
C
      CHARACTER*80 TOPS,ADDL
      CHARACTER*132 INSTR
      CHARACTER*132 COMAND
      CHARACTER*132 CTEMP
      INTEGER POSO,LIBPUT,I,K
      INTEGER ISTAT,READPF,LIBSCR,LIBCUR,LINGET
      INTEGER TRULEN,POSAV(MAXLEV),LEVSAV,IERR,CURONF,ILEV
      LOGICAL LEVEL0,ONEUSE,NXTFLG,DONLEV
      DATA POSAV/MAXLEV*1/,LEVSAV/0/
C----------------------------------------------------------------------
      COMOUT='BLANK'
      EXIFLG=.FALSE.
C                                        ! resets at the end of this routine
      IF(SAVLEV.NE.CURLEV) THEN
        IF(ASTFLG) THEN
          ASTFLG=.FALSE.                 ! Sub-level from interrupt menu
          CALL DISABL
        ENDIF
        DONLEV=.FALSE.                   ! No command done at the new level
        PF=0
        IF(CURLEV.GT.0) POSAV(CURLEV)=POS
        IF(LEVSAV.EQ.0.AND.NXTFLG) LEVSAV=CURLEV   ! Save level when going down
C                                                  ! using ';'
        ILEV=CURLEV                                ! Save level temporarily
        CURLEV=SAVLEV
        IF(NAMLEV(CURLEV).EQ.'MENUDEF') SAVLEV=ILEV ! Save old level when
C                                                   ! going to CONTROL menu
        POS=POSAV(CURLEV)                 ! RESTORE old position within menu
        IF(CURLEV.EQ.MAILEV) THEN
          TOPGO=.FALSE.                   ! RESET flag for going back to top menu
        ENDIF
      ENDIF
C
C ****  Do not alter PF value if it is < 0; < 0 --> event flag was set.
C ****  This will prevent the menu from being redrawn in this case.
C
      IF ( PF .GT. 0 ) THEN
CC        IF ( ((.NOT.WAIFLG).AND.(.NOT.ONEFLG)).OR.RDCOM) PF=0
        IF ( ((.NOT.ONEFLG)).OR.RDCOM) PF=0
      ENDIF
      WAIFLG=.FALSE.                      ! May only get back into command
C                                         ! when PFWAIT has been called
      IF(LEVSAV.GT.0) THEN
        IF(LEVSAV.EQ.CURLEV) THEN
          LEVSAV=0
        ELSEIF(.NOT.NXTFLG) THEN          ! No more parts left on command line
          PF=4
          LEVEL0=.TRUE.
        ENDIF
      ENDIF
C
      IF ( (PF.EQ.0)     .AND.          ! Regenerate menu if PF = 0
     &     (.NOT.ASTFLG))THEN
        CALL MENDIS(.TRUE.)
      ENDIF
      IF ( PF .LT. 0 ) PF = 0
C
  100 CONTINUE
      IF(ONEFLG.AND..NOT.RDCOM.AND..NOT.ASTFLG) THEN
        IF(ONEUSE) THEN
          LEVEL0=.TRUE.
          IF(NXTLEV.EQ.0) THEN
            PF=4
          ELSE
            COMAND=COMPRT(0)(NXTLEV:)       ! Pick new command after ';'
            IF(TRULEN(COMAND).GT.0) THEN
              DO WHILE (COMAND(1:1).EQ.' ') ! Strip off any leading blanks
                COMAND=COMAND(2:)
              ENDDO
            ENDIF
            PF=1                            ! Indicate command already there
            POS=0
          ENDIF
        ELSE
          ONEUSE=.TRUE.
          COMAND=COMSAV
          CALL LINCHK(COMAND,'HELP,MENU',PF,POS,MAXPOS)
          IF(PF.EQ.1.AND.POS.GT.0) THEN
            I=MAX0(TRULEN(COMAND),1)
            CTEMP=MENLIN(POS,CURLEV)//COMAND(1:I)
            COMAND=CTEMP
          ENDIF
        ENDIF
      ELSE
        DO WHILE (PF.EQ.0)
          LEVEL0=.TRUE.
          IF(NXTLEV.GT.0) THEN
            COMAND=COMPRT(0)(NXTLEV:)        ! Pick new command after ';'
            IF(TRULEN(COMAND).GT.0) THEN
              DO WHILE (COMAND(1:1).EQ.' ')  ! Strip off any leading blanks
                COMAND=COMAND(2:)
              ENDDO
            ENDIF
            PF=1                              ! Indicate command already there
            POS=0
            DONLEV=.TRUE.                     ! Now has got a real command at this level
          ELSEIF(NXTFLG.AND.DONLEV) THEN
            NXTFLG=.FALSE.
            PF=4
          ELSE
            IF(.NOT.ASTFLG) THEN
              IF(.NOT.RDCOM) THEN
                IF(SETUP) THEN
                  CALL OUTMSG('0IN SETUP MODE'//CHAR(7))
                ELSEIF(LOGUP) THEN
                  CALL OUTMSG('0IN LOGGING MODE')
                ELSEIF(TRMFLG) THEN
                  CALL OUTMSG(' ')             ! For extra space for next prompt
                ENDIF
                TOPS=TOPLIN(0,CURLEV)          ! Remove blanks in title again
                DO WHILE (TOPS(1:1).EQ.' ')
                  TOPS=TOPS(2:)
                ENDDO
                I=INDEX(TOPS,'  ')
                IF(I.EQ.0) THEN
                  I=TRULEN(TOPS)
                ENDIF
                IF(CURLEV.NE.MAILEV) THEN
                  K=INDEX(NAMLEV(MAILEV),'_')
                  IF(K.NE.0) THEN
                    ADDL=NAMLEV(MAILEV)(1:K-1)
                  ELSE
                    ADDL=NAMLEV(MAILEV)
                  ENDIF
                  CALL OUTMSG(' SUB_LEVEL: '//TOPS(1:I)//
     *                     '   '//ADDL(1:TRULEN(ADDL)))
                ELSE
                  CALL OUTMSG(' Menu: '//TOPS(1:I))
                ENDIF
              ENDIF
              IF(OLDLEV.EQ.CURLEV.AND.SETUP) THEN
                CALL CURLIN('{command, #, HELP (#), MENU, CLOSE}',
     *                  PF,POS,MAXLIN(CURLEV),COMAND)
              ELSEIF(CURLEV.EQ.MAILEV) THEN
                CALL CURLIN('{command, #, HELP (#), MENU, EXIT}',
     *                  PF,POS,MAXLIN(CURLEV),COMAND)
              ELSE
                CALL CURLIN('{command, #, HELP (#), MENU, BACK}',
     *                  PF,POS,MAXLIN(CURLEV),COMAND)
              ENDIF
              IF(RDCOM.AND.COMAND.NE.' ') THEN
                CALL OUTMSG(' COMMAND read--> '//COMAND)
              ENDIF
            ELSE
              CALL CURLIN('{HELP (#), MENU}',
     *                  PF,POS,MAXLIN(CURLEV),COMAND)
              IF(PF.EQ.0.AND.POS.EQ.0) THEN
                COMOUT=' '
              ENDIF
            ENDIF
          ENDIF
          IF(PF.EQ.1.AND.POS.GT.0) THEN
            I=MAX0(TRULEN(COMAND),1)
            CTEMP=MENLIN(POS,CURLEV)//COMAND(1:I)
            COMAND=CTEMP
          ENDIF
          IF(PF.EQ.0.AND.ASTFLG) THEN
            GOTO 6
          ENDIF
        ENDDO
      ENDIF
      IF(PF.EQ.1 ) THEN
        CALL DOPROC(COMAND,COMOUT)
        IF(NXTLEV.GT.0) THEN
          NXTFLG=.TRUE.
        ENDIF
        IF(COMOUT.NE.'BLANK') THEN
          PF=1
          GOTO 6
        ELSEIF(ASTFLG) THEN
          CALL MENDIS(.TRUE.)   ! To get prompt on screen
        ENDIF
      ELSEIF(PF.EQ.2) THEN
        IF(LOGUP) THEN
          WRITE(COMUNI,902) POS
  902     FORMAT('HELP',I3)
        ENDIF
        CALL HLPROC(POS)
        IF(LEVEL0)THEN
          PF=0                                ! At lowest level, restart menu
          IF (.NOT.ASTFLG) THEN
            CALL MENDIS(.TRUE.)                 ! Redisplay menu
          ENDIF
        ELSE
          PF=1                                ! At higher level, restart command
          WAIFLG=.TRUE.                       ! Fool it to start over on command
        ENDIF
      ELSEIF(PF.EQ.3) THEN
        PF=0
        POS=1
        CALL HLPROC(0)
      ELSEIF(PF.EQ.4) THEN
        IF(LEVEL0) THEN
          IF(SETUP.AND.CURLEV.EQ.OLDLEV) THEN
            SETUP=.FALSE.
            CLOSE(COMUNI)
            CALL RLUNIT(555,COMUNI,IERR)
            OLDLEV=0
            CALL MENDIS(.TRUE.)
          ELSE
            IF((SETUP.OR.LOGUP).AND.CURLEV.NE.1) THEN
              WRITE(COMUNI,904)
  904         FORMAT('BACK')
            ENDIF
            IF(ONEFLG.AND..NOT.ONEUSE.AND..NOT.RDCOM) THEN   ! Here when reading command file first,
              PF=0                                           ! then doing the single command
            ELSE
              COMOUT='EXIT'
              EXIFLG=.TRUE.
              PF=0
            ENDIF
          ENDIF
        ELSE
          PF=0
        ENDIF
      ENDIF
    5 CONTINUE
      LEVEL0=.FALSE.
      IF(.NOT.EXIFLG.AND..NOT.ASTFLG.AND..NOT.TOPGO) THEN
        GOTO 100
      ENDIF
    6 CONTINUE
      RETURN
      END
