      SUBROUTINE RUNCOM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Let user select and run an internal COMPACK
C-                         command file
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-
C-      27-Oct-1989 Penelope Constanta-Fanourakis
C-          Replaced command file opening on LUN 5 with
C-          opening on LUN COMLUN.
C-	    Make sure to close file opened on COMLUN
C-	    before opening new file.
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
      INTEGER MAXITM
      PARAMETER (MAXITM=50)
      CHARACTER*40 TOPS,COMAND
      CHARACTER*32 PRTNAM
      CHARACTER*80 ITEMS(1:MAXITM)
      CHARACTER*80 MSGLIN
      CHARACTER*132 FNAME,NEWTOP
      INTEGER ISTAT,SPAWIT,LIBCUR
      INTEGER I,K,LIBBIG,LIN,POSI,TRULEN,J,L,M
      LOGICAL OK
      DATA TOPS/'Command Files'/
C----------------------------------------------------------------------
      ITEMS(1)=' '              !Used to see if no files are available
      PRTNAM=NAMLEV(CURLEV)
      IF(PRTNAM.EQ.'MENUDEF') THEN
        PRTNAM=NAMLEV(SAVLEV)(1:TRULEN(NAMLEV(SAVLEV)))//'$MENUDEF'
      ENDIF
  100 CONTINUE
      IF(COMNUM.GT.0) THEN
        K=INDEX(COMPRT(1),' ')-1
        IF(K.GT.0.OR.TRULEN(ITEMS(POSI)).EQ.80) THEN
          J=INDEX(COMPRT(1),']')
          M=INDEX(COMPRT(1),':')
          IF(J.NE.0.AND.J.EQ.TRULEN(COMPRT(1))) THEN
            FNAME=COMPRT(1)(1:J)//PRTNAM(1:TRULEN(PRTNAM))//
     &          '$*.INP'                   !Insert menu name identifier
            CALL FNDFIL(POSI,MAXITM,FNAME,TOPS,ITEMS)
            IF(PF.EQ.0) THEN
              GOTO 100
            ENDIF
          ELSEIF(M.NE.0.AND.M.EQ.TRULEN(COMPRT(1))) THEN
            FNAME=COMPRT(1)(1:M)//PRTNAM(1:TRULEN(PRTNAM))//'$*.INP'
            CALL FNDFIL(POSI,MAXITM,FNAME,TOPS,ITEMS)
            IF(PF.EQ.0) THEN
              GOTO 100
            ENDIF
          ELSE
            PF=1
            POSI=1
            ITEMS(1)=COMPRT(1)
          ENDIF
        ENDIF
      ELSE
        IF(DEFDIR.NE.' ') THEN
          CALL FNDFIL(POSI,MAXITM,DEFDIR(1:TRULEN(DEFDIR))//
     &       PRTNAM(1:TRULEN(PRTNAM))//'$'//'*.INP',
     &       TOPS(1:TRULEN(TOPS))//' in '//DEFDIR,ITEMS)
        ELSE
          CALL FNDFIL(POSI,MAXITM,DEFDIR(1:TRULEN(DEFDIR))//
     &       PRTNAM(1:TRULEN(PRTNAM))//'$'//'*.INP',
     &       TOPS(1:TRULEN(TOPS)),ITEMS)
        ENDIF
        IF(PF.EQ.0) THEN
          GOTO 100
        ENDIF
      ENDIF
      GOTO (1,2,3,4),PF
    1 CONTINUE
      OLDLEV=CURLEV
      K=INDEX(ITEMS(POSI),' ')-1
      IF(K.GT.0.OR.TRULEN(ITEMS(POSI)).EQ.80) THEN
        J=INDEX(ITEMS(POSI),']')
        M=INDEX(ITEMS(POSI),':')
        IF(J.NE.0) THEN
          FNAME=ITEMS(POSI)(1:J)//PRTNAM(1:TRULEN(PRTNAM))//'$'//
     &          ITEMS(POSI)(J+1:K)       !Insert menu name identifier
        ELSEIF(M.NE.0) THEN
          FNAME=ITEMS(POSI)(1:M)//PRTNAM(1:TRULEN(PRTNAM))//'$'//
     &          ITEMS(POSI)(M+1:K)
        ELSE
          FNAME=DEFDIR(1:TRULEN(DEFDIR))//PRTNAM(1:TRULEN(PRTNAM))//
     &          '$'//ITEMS(POSI)(1:K)
        ENDIF
	CLOSE(COMLUN)		! Close previous open file.
        IF(INDEX(FNAME, '.INP').EQ.0)
     &    FNAME = FNAME(1:TRULEN(FNAME))//'.INP'
        CALL D0OPEN(COMLUN, FNAME, 'IF', OK)
	INPLUN = COMLUN		! Make input to be from command file
        IF(.NOT.OK) THEN
	  INPLUN = 5		! Default INPLUN if open fails
          CALL INTMSG('0Open on command file '//
     &         FNAME(1:TRULEN(FNAME))//' failed!'//CHAR(7))
          CALL INTMSG(' ')
        ELSE
C
C      Set TRMFLG to .FALSE to indicate to CURLIN that we're reading from a file.
C
          SAVTRM=TRMFLG
          TRMFLG=.FALSE.
          WRITE(MSGLIN,206) ITEMS(POSI)(1:20)
  206     FORMAT('0Reading Commands from: ',A20)
          CALL OUTMSG(MSGLIN)
          CALL OUTMSG(' ')
C
C      Turn of full screen mode while reading commands from file
C
          RDCOM=.TRUE.
        ENDIF
      ENDIF
      PF=0
      GOTO 5
    2 CONTINUE
      ISTAT=SPAWIT('HELP '//ITEMS(POSI))
      PF=0
      GOTO 100
    3 PF=0
      GOTO 100
    4 CONTINUE
      IF(ITEMS(1)(1:1).EQ.' ') THEN
        CALL PFWAIT
      ENDIF
      PF=0
    5 CONTINUE
      RETURN
      END
