      SUBROUTINE MENSUB(MENNAM,ITMNAM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Delete Menu item ITMNAM from Menu MENNAM
C-
C-   Inputs  : MENNAM: Name of menulevel to use.
C-             ITMNAM: Name of item for menu display
C-   Outputs : None
C-
C-   Created  12-DEC-1990   Rajendran Raja
C-   Reverse of MENADD
C-   Modified 16-MAY-1991   Scott Snyder
C-    Free strings with STRFRE; move cookies.
C-   Updated  30-OCT-1992   Susan K. Blessing  Use actual length of ITMNAM
C-    for matching.  
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) MENNAM,ITMNAM
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
      INTEGER ITMLENGTH
      CHARACTER*132 BLNK,CTEMP
      CHARACTER*80 TRANUP
      INTEGER ILEV,I
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
C&     &           MENNAM(1:LEN(MENNAM))//CHAR(7)
C&        CALL INTMSG(CTEMP)
C&        STOP
C&ENDIF
      ELSEIF(MAXLIN(USELEV).GT.0)THEN
        ILEV = 0
        ITMLENGTH = TRULEN(ITMNAM)
        DO I = 1 , MAXLIN(USELEV)
C          IF(ITMNAM(1:32).EQ.MENLIN(I,USELEV))THEN  !Use max length allowed
          IF(ITMNAM(1:ITMLENGTH).EQ.MENLIN(I,USELEV))THEN  !Use max length allowed
            ILEV = I
            GO TO 5002
          ENDIF
        ENDDO
 5002   IF(ILEV.EQ.0)THEN
C&IF VAXVMS
          CALL INTMSG(' MENSUB:Menu ITEM '//ITMNAM(1:TRULEN(ITMNAM))//
     &      ' Not found in Menu Level '//MENNAM)
C&ELSE
C&          CTEMP = ' MENSUB:Menu ITEM '//ITMNAM(1:TRULEN(ITMNAM))//
C&     &      ' Not found in Menu Level '//MENNAM
C&          CALL INTMSG(CTEMP)
C&ENDIF
          RETURN
        ENDIF
C
        CALL STRFRE(HELP_COOKIES(ILEV, USELEV))
        IF(ILEV.LT.MAXLIN(USELEV))THEN
          DO I = ILEV, MAXLIN(USELEV)
            MENLIN(I,USELEV) = MENLIN(I+1,USELEV) ! Contract
            COMLIN(I,USELEV) = COMLIN(I+1,USELEV)
            TOPLIN(I,USELEV) = TOPLIN(I+1,USELEV)
            HELP_COOKIES(I, USELEV) = HELP_COOKIES(I+1, USELEV)
          ENDDO
        ENDIF
        MAXLIN(USELEV)=MAXLIN(USELEV)-1
      ENDIF
      RETURN
      END
