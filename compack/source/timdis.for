      SUBROUTINE TIMDIS(DISPIT,INDEF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set up a timed loop of redoing a display
C-                         VAX-specific
C-
C-   Inputs  : DISPIT: Routine which will be called at the end of time
C-                     interval to perform a display task.
C-             INDEF:  Time string for default timer setting
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
C&IF VAXVMS
      IMPLICIT NONE
      EXTERNAL DISPIT
      CHARACTER*(*) INDEF
      INCLUDE 'D0$INC:TIMSTA.INC'
      INTEGER ISTAT,SYS$BINTIM,READPF,PF0,SYS$CANTIM,TRULEN,PFNUM
      INTEGER POS1
      LOGICAL SETMOD,GETDEV,FULMOD
      CHARACTER*8 TIME_STRING,INTIM,COMAND
      CHARACTER*80 PROMPT
      CHARACTER*8 PFL(4)
C----------------------------------------------------------------------
      PRMTIM='Type EXIT to end display'
      WRITE(PROMPT,100) INDEF(1:TRULEN(INDEF))
  100 FORMAT(' Enter time interval for redisplay',
     *       ' (min:sec) [',A<TRULEN(INDEF)>'] >')
      CALL GETPAR(1,PROMPT,'C',INTIM)
      IF(PFNUM().EQ.0.AND..NOT.SETMOD()) THEN
        IF(TRULEN(INTIM).EQ.0) THEN
          INTIM=INDEF
        ENDIF
        TIME_STRING='0 :'//INTIM
        IF(GETDEV()) THEN
          CALL OUTMSG('1')
        ENDIF
        ISTAT=SYS$BINTIM(TIME_STRING,BIN_TIME)
        IF(ISTAT) THEN                    ! Translate delta-time to binary
          CALL DISPIT
          DISPOK=.TRUE.
          CALL RESTAT(DISPIT)
          IF(GETDEV()) THEN
            PF0=0
            CALL PFGET(PFL)
            CALL PFLABL(' ',' ',' ','END')
            DO WHILE (PF0.NE.4)
              PF0=READPF()
            ENDDO
            CALL PFLABL(PFL(1),PFL(2),PFL(3),PFL(4))
            IF(.NOT.FULMOD()) THEN
              CALL OUTMSG('1')   ! Erase screen to avoid overwriting by prompt
            ENDIF
          ELSE
            PF0=0
            DO WHILE (PF0.NE.4)
              CALL README(COMAND,PF0,.FALSE.,'>')
              CALL LINCHK(COMAND,'EXIT',PF0,POS1,1)
            ENDDO
          ENDIF
          DISPOK=.FALSE.
          ISTAT=SYS$CANTIM(,)
        ELSE
          CALL MSGSCR(ISTAT,'BINTIM-->')
          CALL PFWAIT
        ENDIF
      ENDIF
C&ENDIF
      RETURN
      END
