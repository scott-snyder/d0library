      SUBROUTINE ECOPEN(LWRITE,LUN,IKEY,FNAME,CSTAT,IERR)
C----------------------------------------------------------------------
C-
C-   PURPOSE AND METHODS : Open an  event catalog file.
C-      Called from D0DAD_OPEN.
C-
C-   INPUTS  : 
C-   OUTPUTS : 
C-   CONTROLS: 
C-
C-   CREATED   8-NOV-1993   John D Hobbs
C-   MODIFIED 31-JUL-1994   John D Hobbs - Open sharable and use lock 
C-                file
C-            19-DEC-1994   John D Hobbs - Add ECFAIL_BYPASS entry
C-                point to allow opening of corrupted catalogs.  This
C-                will cause IERR to return an error code, but the
C-                open will be performed as much as possible...
C-            06-JUN-1995   JDH - Fix filename parsing to reflect new
C-                version of FILENAME_PARSE.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      LOGICAL LWRITE,OK,STATE
      INTEGER LUN,IRECL,IERR,IKEY,IRECEC,IRPREC,NRUNS,NW,IC
      CHARACTER*(*) FNAME,CSTAT,CHOPT*3,FELOCK*(*)
C&IF VAXVMS
      PARAMETER(FELOCK='.evtcat-lock;1')
C&ELSE
C&      PARAMETER(FELOCK='.evtcat.lock')
C&ENDIF
C
      INTEGER LENOCC
      LOGICAL ECRUN_CHECK
C----------------------------------------------------------------------
C
C  Initialize record length
C
      CALL ECFGET('KRPREC',IRPREC,IERR)
      IRECL=KRECEC*IRPREC
C
C  Do the lock file setup and create.  If can't be created, No writing.
C
      CALL FILENAME_PARSE(FNAME,'DEV+DIR+NAM',FNLOCK,IC)
      FNLOCK=FNLOCK(1:IC)//FELOCK
      IF(LWRITE) THEN
        OPEN(LUN,FILE=FNLOCK(1:LENOCC(FNLOCK)),STATUS='NEW',ERR=908)
        CLOSE(LUN)
      ENDIF
C
C  Do the open...
C
      CHOPT='IUS'                                      ! open Sharable 
      IF( LWRITE ) CHOPT='OUS'
      IF( LWRITE .AND. CSTAT.EQ.'OLD' ) CHOPT='MUS'
CJDH      CHOPT='IU'                                       ! open Non-shared
CJDH      IF( LWRITE ) CHOPT='OU'
CJDH      IF( LWRITE .AND. CSTAT.EQ.'OLD' ) CHOPT='MU'
C
      CALL D0RZOPEN(LUN,FNAME,CHOPT,4*IRECL,OK)
      IF( .NOT.OK ) GOTO 901
C
C  Setup the Zebra bank required to hold file info...
C
      CALL ECBANK(IERR)
      IF( IERR.NE.0 ) GOTO 905
      IQ(LECHD+JLUN)=LUN
      IF( LWRITE ) IQ(LECHD+JRW)=1
      IQ(LECHD+JKEY)=IKEY
C
C  Do header...
C
      IF( CSTAT.NE.'OLD' ) THEN
         CALL ECHWRT(LUN,IERR)
         IF( IERR.NE.0 ) GOTO 903
      ELSE
         CALL ECHRD(LUN,IERR)
         IF( IERR.EQ.(-4) ) GOTO 906   ! Corrupted catalog
         IF( IERR.NE.0 ) GOTO 904
      ENDIF
C
C  We used to set the DIRTY flag here if writable.  This has been moved
C  to ECINT to better reflect the true state...
C
C  Check record size to keep from inserting dead space...
C
      IRPREC=IQ(LECHD+NDEC+JEC_IRPREC)
      IRECEC=IQ(LECHD+NDEC+JEC_IRECEC)
      IF( IRECL.NE.IRPREC*IRECEC ) THEN
C
C  Initial record size wrong.  Reopen and reload run section.
C
        CLOSE(LUN)
        IRECL=IRPREC*IRECEC
        CALL D0RZOPEN(LUN,FNAME,CHOPT,4*IRECL,OK)
        IF( .NOT.OK ) GOTO 901
        NRUNS=IQ(LECHD+NDEC+JEC_IRUN1)-IQ(LECHD+NDEC+JEC_IRUN0)+1
        NRUNS=IRPREC*NRUNS
        NW=IRECEC*NRUNS
        CALL ECLOAD(IQ(LRUNS+1),2,NRUNS,IERR)
        IF( IERR.NE.0 ) GOTO 901
      ENDIF
C
C  Check run section for logical consistancy do this only at open, not
C  when reading header...
C
      IF( .NOT.ECRUN_CHECK() ) GOTO 907
C
C  Save lock-file name for use in ECCLOS/ECGRUN
C
      CALL UCTOH(FNLOCK,IQ(LFLOK+1),4,LEN(FNLOCK))
C
 999  CONTINUE
      RETURN

 901  CONTINUE
      IERR = -1
      RETURN

 902  CONTINUE
      IERR = -2
      RETURN

 903  CONTINUE
      IERR = -3
      RETURN

 904  CONTINUE
      IERR = -4
      RETURN

 905  CONTINUE
      IERR = -5
      RETURN

 906  CONTINUE
      IERR = -6
      RETURN

 907  CONTINUE
      IERR = -7
      RETURN

 908  CONTINUE
      IERR = -8
      RETURN
      END
