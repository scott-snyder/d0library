      SUBROUTINE EXIMEN(ISTAT,ILEV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Exit from a COMPACK program
C-
C-   Inputs  : ISTAT: Status to be reported at exit
C-             ILEV:  Level of menu exiting from (not used).
C-   Outputs : None
C-   Controls: None
C-
C-   Modified 22-SEP-1988   Jan S. Hoftun
C-   Updated  30-SEP-1991   Herbert Greenlee
C-     Added calls to smg$delete_virtual_keyboard and smg$delete_pasteboard.
C-   Updated   4-NOV-1992   Soren G. Frederiksen
C-                          For UNIX: only call the SMG routines if
C-                          FULSCR is true.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ISTAT,ILEV
C
C     Get definitions for COMPACK
C
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER STAT,LIBSCR,LIBLIN,LIBCUR,LIBERA,LIBPUT
C&IF VAXVMS
C&ELSE
C&      INTEGER SMG$DELETE_VIRTUAL_KEYBOARD, SMG$DELETE_PASTEBOARD
C&ENDIF
      INTEGER TRULEN,FROBAC,I,IBEG,J
      CHARACTER*64 BLNK
      CHARACTER*132 MSGLIN
      DATA BLNK/' '/
C----------------------------------------------------------------------
      IF(FULSCR) THEN
        STAT=LIBSCR(1,PBROWS)                                      ! Reset scroll-region
        STAT=LIBPUT(' ',PBROWS,1,0)
        STAT=LIBERA(1,1)
        IF(ENDFLG) THEN
          FROBAC=PBCOLS/4-15
          STAT=LIBPUT(BLNK(1:FROBAC)//'>>>>>>>>> EXIT from <<<<<<<<'//
     *                  BLNK(1:PBCOLS/2-FROBAC-28),1,1,2)   ! Write end message(reverse video)
          STAT=LIBPUT(TOPLIN(0,MAILEV)(1:PBCOLS/2),2,1,3)   ! Write end message(reverse video)
          STAT=LIBCUR(4,1)
        ENDIF
      ELSEIF(.NOT.ONEFLG.AND.ENDFLG) THEN
        J=TRULEN(TOPLIN(0,MAILEV))
        IBEG=1
        DO I=1,J
          IF(TOPLIN(0,MAILEV)(I:I).NE.' ') THEN
            GOTO 90
          ELSE
            IBEG=I+1
          ENDIF
        ENDDO
   90   CONTINUE
        IF(MAILEV.GT.0) THEN
          CALL OUTMSG('0'//'Exit from---> '//TOPLIN(0,MAILEV)(IBEG:J))
          CALL OUTMSG(' ')
        ENDIF
      ENDIF
      CALL EXIUSR
C&IF VAXVMS
      CALL EXIT
C&ELSE
C&      IF (FULSCR) THEN
C&        STAT = SMG$DELETE_VIRTUAL_KEYBOARD(KEYID)
C&        STAT = SMG$DELETE_PASTEBOARD(PASTID, 0)
C&        STOP
C&      ELSE
C&        STOP
C&      ENDIF
C&ENDIF
      END
