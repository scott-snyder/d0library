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
      INCLUDE 'D0$INC:SMGCOM.INC'
C
      INTEGER IUNI,IERR,MAX,IOS,TRULEN,J,K
      CHARACTER*132 INBUFF(3),BLNK,CTEMP
      CHARACTER*2048 HLPTXT
      CHARACTER*64 MSGLIN
      LOGICAL OK
      DATA BLNK/' '/
      INTEGER*4 STRSTO
C
C     Open setup input file
C
      CALL GTUNIT(555,IUNI,IERR)
      IF(IERR.EQ.0) THEN
        CALL D0RZOPEN(IUNI, FILNAM, 'IU', 2444, OK)
        IF(.NOT.OK) THEN
          CTEMP = '0Failed in OPEN of MENU file '//
     &         FILNAM(1:LEN(FILNAM))//CHAR(7)
          CALL OUTMSG(CTEMP)
          WRITE(MSGLIN,100) IUNI
100       FORMAT(' Unit number used is: ',I2)
          CALL OUTMSG(MSGLIN)
          STOP
        ELSE
          MAX=0
          IOS=0
          DO WHILE (IOS.EQ.0.AND.MAX.LT.MAXPOS)
            READ(IUNI,IOSTAT=IOS,REC=(MAX+1),ERR=2) INBUFF,HLPTXT
            IF(IOS.EQ.0) THEN
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
