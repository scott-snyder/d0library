      SUBROUTINE ADDITM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Add a menu item in MENUMAKER
C-                         VAX-specific
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-   Modified   16-MAY-1991   Scott Snyder
C-    Store help with STRSTO,
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C     Get definitions for COMPACK
C
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
C&IF VAXVMS
      INCLUDE '($HLPDEF)'
C
      CHARACTER*80 INCOM1,INCOM2
      CHARACTER*40 PROMPT(2)
      CHARACTER*1 TYPARR(2)
      INTEGER ISTAT,SPAWIT,LBR$OUTPUT_HELP, STRSTO
      EXTERNAL HLPGET, STRSTO
      DATA PROMPT/'$_Enter name of command for menu >',
     *            '$_Enter VMS command to be executed >'/
      DATA TYPARR/'C','C'/
C----------------------------------------------------------------------
      IF(MAXLIN(CURLEV).LT.MAXPOS) THEN
        CALL GETPAR(2,PROMPT,TYPARR,INCOM1,INCOM2)
        IF(PF.EQ.0) THEN
          IF(.NOT.SETUP) THEN
            MAXLIN(CURLEV)=MAXLIN(CURLEV)+1
            MENLIN(MAXLIN(CURLEV),CURLEV)=INCOM1
            HELP_COOKIES(MAXLIN(CURLEV), CURLEV) = STRSTO(' ')
            ISTAT=LBR$OUTPUT_HELP(HLPGET,PBCOLS,INCOM2,
     &           'HELPLIB',
     &            HLP$M_PROCESS+HLP$M_GROUP+HLP$M_SYSTEM,)
C
C       Add CALL to command specifier to force the dispatching routine to be
C       called. The VMS command will then be done by SYSDSP.
C
            COMLIN(MAXLIN(CURLEV),CURLEV)='CALL '//INCOM2
          ENDIF
        ENDIF
      ELSE
        CALL OUTMSG('0No more available command slots!'//CHAR(7))
      ENDIF
C&ELSE
C&      CALL INTMSG('0ADD MENU ITEM not implemented here!'//CHAR(7))
C&ENDIF
      RETURN
      END
