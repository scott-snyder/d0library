      SUBROUTINE MENADD(MENNAM,NOTITL,ITMNAM,ACTION,HLPTXT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Put information for a new item into commonblock
C-
C-   Inputs  : MENNAM: Name of menulevel to use.
C-             NOTITL: Logical flag for title or no title
C-             ITMNAM: Name of item for menu display
C-             ACTION: Action verb for the command
C-             HLPTXT: Help information for item.
C-   Outputs : None
C-   Controls: MAXLIN for level is increased
C-
C-   Created  20-SEP-1988   Jan S. Hoftun
C-   Modified 16-MAY-1991   Scott Snyder
C-    Store help with STRSTO.
C-   Updated  18-SEP-1991   Herbert Greenlee
C-   Updated  12-OCT-1992   Robert E. Avery  Check number of items. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) MENNAM,ITMNAM,ACTION,HLPTXT
      LOGICAL NOTITL
C
C     Get definitions for COMPACK
C
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
C&IF VAXVMS
      INCLUDE 'D0$COMPACK$SOURCE:COMMSG.DEF'
C&ENDIF
      INCLUDE 'D0$INC:SMGCOM.INC'
C
      INTEGER TRULEN,USELEV,J,K
      INTEGER*4 STRSTO
      CHARACTER*132 BLNK,CTEMP
      CHARACTER*80 TRANUP
      DATA BLNK/' '/
C----------------------------------------------------------------------
      USELEV=0
      DO 5000 J=1,UPRLEV
        K=TRULEN(MENNAM)
        IF(NAMLEV(J).EQ.TRANUP(MENNAM)) THEN
          USELEV=J
          GOTO 5001
        ENDIF
 5000 CONTINUE
 5001 CONTINUE
      IF(USELEV.EQ.0) THEN
C&IF VAXVMS
        CALL ABOMEN(COMPACK__NOMATCH,MENNAM)
C&ELSE
C&        CTEMP = '0NO match with MENU name: '//
C&     &          MENNAM(1:LEN(MENNAM))//CHAR(7)
C&        CALL OUTMSG(CTEMP)
C&        STOP
C&ENDIF
      ELSEIF(MAXLIN(USELEV).LT.MAXPOS) THEN
        MAXLIN(USELEV)=MAXLIN(USELEV)+1
        MENLIN(MAXLIN(USELEV),USELEV)=ITMNAM
        COMLIN(MAXLIN(USELEV),USELEV)=ACTION
        IF(NOTITL) THEN
          TOPLIN(MAXLIN(USELEV),USELEV)='NO TITLE'
        ELSE
          J=TRULEN(ITMNAM)
          K=MAX0((PBCOLS/2-J-9)/2,0)
          TOPLIN(MAXLIN(USELEV),USELEV)=BLNK(1:K)//'Doing "'//
     &          ITMNAM(1:J)//'"'
        ENDIF
        HELP_COOKIES(MAXLIN(USELEV), USELEV) = STRSTO(HLPTXT)
      ENDIF
      RETURN
      END
