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
      INTEGER ISTAT,K
      INTEGER OLDPOS,TRULEN,OLDL,OLDC
      SAVE SKIP
      LOGICAL SKIP,BOOLE
      CHARACTER*20 SETXT
      PARAMETER (SETXT= '   In SETUP mode')
      CHARACTER*20 LOGTXT
      PARAMETER (LOGTXT='   In LOGGING mode')
C----------------------------------------------------------------------
      IF ( SKIP ) GOTO 999
C
      CALL OUTMSG(' ')
      CALL OUTMSG('0'//PROMP1)
      CALL OUTMSG('$'//PROMP2(1:TRULEN(PROMP2)))
C
  999 CONTINUE
      OLDPOS=POS
      NODISP=.FALSE.     !Should only be valid for ONE call
      RETURN
C
      ENTRY MENSKP(BOOLE)
      SKIP = BOOLE
      END
