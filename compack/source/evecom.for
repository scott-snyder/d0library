      SUBROUTINE EVECOM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Select and edit a command file
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  19-FEB-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C     Get definitions for COMPACK
C
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
C
C&IF VAXVMS
      INTEGER MAXITM
      PARAMETER (MAXITM=50)
      CHARACTER*40 TOPS,OUTDIR,COMAND
      CHARACTER*32 PRTNAM
      CHARACTER*32 ITEMS(1:MAXITM)
      CHARACTER*80 EVELIN
      INTEGER ISTAT,LIBERA,SPAWIT,K,PFNUM
      INTEGER POSI,TRULEN
      LOGICAL EDIFIL
      DATA TOPS/'Available Command Files'/
C
      ITEMS(1)=' '        ! Make sure first one is empty in case no files
C                         ! exist
      PRTNAM=NAMLEV(CURLEV)
      IF(PRTNAM.EQ.'MENUDEF') THEN
        PRTNAM=NAMLEV(SAVLEV)(1:TRULEN(NAMLEV(SAVLEV)))//'$MENUDEF'
      ENDIF
  100 CONTINUE
      IF(COMNUM.GT.0) THEN
        PF=1
        POSI=1
        ITEMS(1)=COMPRT(1)
      ELSE
        CALL FNDFIL(POSI,MAXITM,PRTNAM(1:TRULEN(PRTNAM))//
     *               '$'//'*.INP',TOPS,ITEMS)
        IF(PF.EQ.0) THEN
          GOTO 100
        ELSEIF(PF.EQ.4.AND.ITEMS(1).EQ.' ') THEN
          PF=1              !Fool it when no files available to ask about
C                           !editing a new file
          POSI=1
        ENDIF
      ENDIF
      GOTO (1,2,3,4),PF
    1 CONTINUE
      IF(SETUP.OR.LOGUP) THEN
        WRITE(COMUNI,101) ITEMS(POSI)
  101   FORMAT(A30)
      ENDIF
      IF(.NOT.SETUP) THEN
        OLDLEV=CURLEV
        K=INDEX(ITEMS(POSI),' ')-1
        EDIFIL=.TRUE.
        IF(K.GT.0) THEN
          EVELIN=PRTNAM(1:TRULEN(PRTNAM))//
     *           '$'//ITEMS(POSI)(1:K)//'.INP'
        ELSE
          CALL GETPAR(1,'Do you want to create a new file? (YES,NO)'//
     *                  ' [Y] >','L',EDIFIL)
          IF(PFNUM().NE.0) THEN
            GOTO 5
          ENDIF
          IF(EDIFIL) THEN
            CALL GETPAR(1,'Enter filename (no type) >','C',EVELIN)
            IF(PFNUM().NE.0) THEN
              GOTO 5
            ENDIF
            EVELIN=PRTNAM(1:TRULEN(PRTNAM))//
     *           '$'//EVELIN(1:TRULEN(EVELIN))//'.INP'
          ENDIF
        ENDIF
        IF(EDIFIL) THEN
          CALL EVEFIL(EVELIN)
        ENDIF
      ENDIF
      PF=0
      GOTO 5
    2 CONTINUE
      IF(FULSCR) ISTAT=LIBERA(1,1)
      ISTAT=SPAWIT('HELP '//ITEMS(POSI))
      PF=0
      GOTO 100
    3 PF=0
      GOTO 100
    4 CONTINUE
      PF=0
    5 CONTINUE
C&ELSE
C&      CALL INTMSG('0Command file mode NOT supported here!'//CHAR(7))
C&      CALL INTMSG(' ')
C&ENDIF
      RETURN
      END
