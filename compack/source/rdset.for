      SUBROUTINE RDSET(FILNAM,USELEV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set up menu and help information for a particular
C-                         menu level
C-
C-   Inputs  : FILNAM: Logical name of file to read information from
C-             USELEV: Number of the level as calculated by MENSET
C-   Outputs : None
C-
C-   Modified 14-JUN-1988   Jan S. Hoftun
C-   Modified   16-MAY-1991   Scott Snyder
C-    Store help with STRSTO.
C-   Updated  18-SEP-1991  Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CHARACTER*(*) FILNAM
      INTEGER USELEV
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
      INTEGER IUNI,IERR,MAX,IOS,TRULEN,J,K
      CHARACTER*132 INBUFF(3),BLNK,CTEMP
      CHARACTER*2048 HLPTXT
      CHARACTER*64 MSGLIN
      DATA BLNK/' '/
      INTEGER*4 STRSTO
C
C     Open setup input file
C
      CALL GTUNIT(555,IUNI,IERR)
      IF(IERR.EQ.0) THEN
C&IF VAXVMS
        OPEN(UNIT=IUNI,FILE=FILNAM,STATUS='OLD',READONLY,SHARED,
     &       FORM='UNFORMATTED',IOSTAT=IOS,ACCESS='DIRECT')
        IF(IOS.EQ.44) THEN
          CALL ORDSET(FILNAM,USELEV)
        ELSEIF(IOS.NE.0) THEN
          CALL ABOMEN(COMPACK__NOSETUP,FILNAM)
C&ELSE
C&        OPEN(UNIT=IUNI,FILE=FILNAM,STATUS='OLD',RECL=611,
C&     &       FORM='UNFORMATTED',IOSTAT=IOS,ACCESS='DIRECT')
C&        IF(IOS.NE.0) THEN
C&          CTEMP = '0Failed in OPEN of MENU file '//
C&     &         FILNAM(1:LEN(FILNAM))//CHAR(7)
C&          CALL OUTMSG(CTEMP)
C&          WRITE(MSGLIN,100) IUNI
C&100       FORMAT(' Unit number used is: ',I2)
C&          CALL OUTMSG(MSGLIN)
C&          STOP
C&ENDIF
        ELSE
          MAX=0
          IOS=0
          DO WHILE (IOS.NE.36.AND.MAX.LT.MAXPOS)
            READ(IUNI,IOSTAT=IOS,REC=(MAX+1),ERR=2) INBUFF,HLPTXT
            IF(IOS.NE.36) THEN
              MAX=MAX+1
              MENLIN(MAX,USELEV)=INBUFF(2)
              COMLIN(MAX,USELEV)=INBUFF(3)
              IF(INDEX(INBUFF(1),'NO TITLE').EQ.0) THEN
                J=TRULEN(INBUFF(2))
                K=MAX0((PBCOLS/2-J-9)/2,1)
                TOPLIN(MAX,USELEV)=BLNK(1:K)//'Doing "'//
     &          INBUFF(2)(1:J)//'"'
              ELSE
                TOPLIN(MAX,USELEV)='NO TITLE'
              ENDIF
              HELP_COOKIES(MAX, USELEV) = STRSTO(HLPTXT)
            ENDIF
          ENDDO
    2     CONTINUE
          MAXLIN(USELEV)=MAX
        ENDIF
        CLOSE(IUNI)
      ENDIF
      CALL RLUNIT(555,IUNI,IERR)
      RETURN
      END
