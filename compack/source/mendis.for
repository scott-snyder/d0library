      SUBROUTINE MENDIS(FULDSP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To display menu again.
C-
C-   Inputs  : FULDSP: Flag to indicate whether display should be totally
C-                     regenerated or just updated.
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 1-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FULDSP
C
C     Get definitions for COMPACK
C
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER LIBERA,LIBBIG,LIBNXT,LIBCUR
      INTEGER ISTAT,LIBSCR,K,LINTOP,LINOLD,LIBPUT
      INTEGER OLDPOS,TRULEN,OLDL,OLDC
      SAVE SKIP
      LOGICAL GETDEV,SKIP,BOOLE
      CHARACTER*20 SETXT
      PARAMETER (SETXT= '   In SETUP mode')
      CHARACTER*20 LOGTXT
      PARAMETER (LOGTXT='   In LOGGING mode')
C----------------------------------------------------------------------
      IF ( SKIP ) GOTO 999
C
      IF(FULSCR) THEN
        IF(OLDPOS.EQ.POS.OR.FULDSP) THEN
          ISTAT=LIBERA (1,1)
          IF(.NOT.NODISP) THEN
            ISTAT=LIBBIG(TOPLIN(0,CURLEV)(1:PBCOLS/2),1,1,3)
            IF(SETUP) THEN
              ISTAT=LIBPUT(SETXT,2,PBCOLS/2-10,3)
            ELSEIF(LOGUP) THEN
              ISTAT=LIBPUT(LOGTXT,2,PBCOLS/2-11,3)
            ENDIF
            IF(ASTFLG) THEN
              CALL PFLABL('DO','HELP','LINE',' ')
            ELSEIF(OLDLEV.EQ.CURLEV.AND.SETUP) THEN
              CALL PFLABL('DO','HELP','LINE','CLOSE')
            ELSEIF(CURLEV.EQ.MAILEV) THEN
              CALL PFLABL('DO','HELP','LINE','EXIT')
            ELSE
              CALL PFLABL('DO','HELP','LINE','BACK')
            ENDIF
            ISTAT=LIBSCR(3,PBROWS-2)
            IF(MAXLIN(CURLEV).GT.0) THEN
              CALL LINES1(POS,MENLIN(1,CURLEV),MAXLIN(CURLEV),
     *                    LINTOP,LINOLD,NUMCOL,LINSPA)
            ELSE
              ISTAT=LIBCUR(3,1)
              CALL OUTMSG('0No items defined for this level!'//CHAR(7))
            ENDIF
          ENDIF
        ELSE
          CALL LINES(OLDPOS,POS,MENLIN(1,CURLEV),MAXLIN(CURLEV),
     *                 LINTOP,LINOLD,NUMCOL,LINSPA)
        ENDIF
      ELSEIF(ASTFLG.AND.FULDSP.AND..NOT.NODISP) THEN
        ISTAT=LIBCUR(PBROWS,1)
        CALL OUTMSG(' ')
        CALL OUTMSG('0'//PROMP1)
        IF(GETDEV()) THEN
          ISTAT=LIBNXT(PROMP2(1:TRULEN(PROMP2)),0)
        ELSE
          CALL OUTMSG('$'//PROMP2(1:TRULEN(PROMP2)))
        ENDIF
      ENDIF
C
  999 CONTINUE
      OLDPOS=POS
      NODISP=.FALSE.     !Should only be valid for ONE call
      RETURN
C
      ENTRY MENSKP(BOOLE)
      SKIP = BOOLE
      END
