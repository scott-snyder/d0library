      SUBROUTINE CHOT_READ(IRUN,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Read the hot channel bank from the database
C-                          (or a file)
C-
C-   Inputs  : IRUN  - run number
C-   Outputs : IER   - error code
C-                     0  ok
C-                     1  error finding database name (dbmu_gtfile)
C-                    10  error reading parameters from RCP bank
C-                    11  error initializing database
C-                    12  no information in database for this run
C-   Controls:
C-
C-   Created  26-JAN-1993   Jan Guida
C-   Updated  28-MAY-1993   Jan Guida  Updated for database mode 
C-   Updated  17-JAN-1994   Jan Guida  Add call to DBMU_GTFILE 
C-   Updated  21-JUL-1995   Jan Guida  Add error code 12, run not in database 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:D3U.INC'
      INCLUDE 'D0$LINKS:IZCHOT.LINK'
      INTEGER NKYS
      PARAMETER (NKYS=15)
      INTEGER KEY(NKYS),NKEY
      INTEGER IRUN,IER,IERR
      INTEGER NR,LCGEV,GZCGEV,LOC,LCHOT,LINKH
      INTEGER LENF,LENP,WHAT
      INTEGER FILID
      LOGICAL D3U_SET_PATH,BYPASS_DBL3_ERROR
      LOGICAL LFIRST,LOK
      CHARACTER*6  CCON,CHOP
      CHARACTER*24 PATH,CHOT_PATH
      CHARACTER*64 DBCHOT,FILNAM
      CHARACTER*80 STRING
      DATA LFIRST/.TRUE./
C----------------------------------------------------------------------
      IER = 0
C
      IF (LFIRST) THEN
        NR = 0
        LCGEV = GZCGEV()
        IF (LCGEV.EQ.0) CALL BKCGEV(NR,LCGEV)
C
        CALL EZLOC('CAHITS_RCP',LOC)
        IF(LOC.GT.0) CALL EZPICK('CAHITS_RCP')
        CALL EZERR(IERR)
        IF (IERR.EQ.0) THEN                      ! CAHITS_RCP exists
          CALL EZGETS('DBCHOT',1,DBCHOT,LENF,IERR)       ! Area of FZ file
          IF(IERR.NE.0) THEN                             ! Or database area,name
            CALL ERRMSG('NO DBCHOT IN CAHITS_RCP','CHOT_READ',
     &        'ABORTING - Modify RCP file according','F')
            IER = 10
            GO TO 999
          END IF
          CALL EZGETS('CHOT_PATH',1,CHOT_PATH,LENP,IERR)
          IF (IERR.NE.0) THEN
            CHOT_PATH = '//TOP/RDET/CAL/CHOT'
            CALL ERRMSG('NO CHOT_PATH IN CAHITS_RCP','CHOT_READ',
     &        'CHOT_PATH not in CAHITS_RCP, using default','W')
          ENDIF
          BYPASS_DBL3_ERROR = .FALSE.
          CALL EZGET('BYPASS_DBL3_ERROR',BYPASS_DBL3_ERROR,IERR)
          IF(IERR.NE.0) CALL ERRMSG('NO_ERR_SWITCH','CHOT_READ',
     &      'USE DBL3 BYPASS ERROR SWITCH  = FALSE AS DEFAULT','W')
          CALL EZRSET
        ELSE                        ! No CAHITS_RCP use defaults
          CALL ERRMSG(' NO CAHITS_RCP','CHOT_READ',
     &      'Unable to pick CAHITS_RCP, aborting','F')
          IER = 10
          GOTO 999
        END IF
C
C      Needed for file mode only
c        IF(DBCHOT(LENF:LENF).NE.']')THEN
c          LENF=LENF+1
c          DBCHOT(LENF:LENF)=':'
c        ENDIF
C
        LFIRST = .FALSE.
      ENDIF
C
C ****  Read input from file for now
C
c      WRITE(FILNAM,301)DBCHOT(1:LENF),IRUN,IRUN
c  301 FORMAT(A,'CAL_CHOT_',I6.6,'_',I6.6,'.FZ')
C
c      WHAT = 0
c      NKEY = 0
c      PATH = ' '
c      CHOP = ' '
c      CALL VZERO(KEY,NKYS)
c      CALL D0DBL3_OPENFZ(FILNAM,IDVSTP,'IX',FILID,IER)
c      IF (IER.NE.0) THEN
c        WRITE(STRING,310)IER
c  310   FORMAT(' OPENFZ returns error code ',I3,' - aborting')
c        CALL ERRMSG('OPEN FAILURE','CHOT_READ',STRING,'W')
c        GOTO 999
c      ENDIF
c      CALL D0DBL3_INFZ(FILID,CHOP,WHAT,PATH,NKEY,KEY,CCON,LINKH,IER)
c      IF (IER.NE.0) THEN
c        WRITE(STRING,311)IER
c  311   FORMAT(' INFZ returns error code ',I3,' - aborting')
c        CALL ERRMSG('CALHOT','CHOT_READ',STRING,'W')
c        GO TO 999
c      ENDIF
c      CALL D0DBL3_CLOSEFZ (FILID,IER)
C
C- Initialize dbl3 parameters to default
C
      CALL DBMU_GTFILE('RSM',IRUN,' ',DBCHOT,LOK)
      IF (.NOT.LOK) THEN
        WRITE (STRING,500) IRUN
  500   FORMAT(' Err finding DB name, ',I9)
        IF (BYPASS_DBL3_ERROR) THEN
          CALL ERRMSG(STRING,'CHOT',
     &      'ABORTING - Check runsum database','W')
        ELSE
          CALL ERRMSG(STRING,'CHOT',
     &      'ABORTING - Check runsum database','F')
        ENDIF
        IER = 1
        GO TO 999
      ENDIF
C
      PATH = CHOT_PATH
      CALL D3U_INI (IDVSTP,PATH,'S348','NOT',IERR)
      IF (IERR .NE. 0) THEN
        WRITE (STRING,501) IRUN
  501   FORMAT(' Err init DB ',I9)
        IF (BYPASS_DBL3_ERROR) THEN
          CALL ERRMSG(STRING,'CHOT',
     &      'ABORTING - Check runsum database','W')
        ELSE
          CALL ERRMSG(STRING,'CHOT',
     &      'ABORTING - Check runsum database','F')
        ENDIF
        IER = 11
        GO TO 999
      ENDIF
C
C  *** initialize DBL3 database
C
      CALL D3U_START (DBCHOT,' ',IERR)
      IF (IERR .NE. 0) THEN
        WRITE (STRING,502) IRUN
  502   FORMAT(' Err init/start DB ',I9)
        IF (BYPASS_DBL3_ERROR) THEN
          CALL ERRMSG(STRING,'CHOT',
     &      'ABORTING - Check runsum database','W')
        ELSE
          CALL ERRMSG(STRING,'CHOT',
     &      'ABORTING - Check runsum database','F')
        ENDIF
        IER = 11
        GOTO 999
      ENDIF
C
      PATH = CHOT_PATH
      IF ( .NOT. D3U_SET_PATH(PATH,0) ) THEN
        WRITE (STRING,503) IRUN
  503   FORMAT(' Err setting DB path, ',I9)
        IF (BYPASS_DBL3_ERROR) THEN
          CALL ERRMSG(STRING,'CHOT',
     &      'ABORTING - Check runsum database','W')
        ELSE
          CALL ERRMSG(STRING,'CHOT',
     &      'ABORTING - Check runsum database','F')
        ENDIF
        IER = 11
        GOTO 999
      ENDIF
      KEY(3) = IRUN
      KEY(4) = KEY(3)
      KEY(8) = KEY(3)
C
      CALL D3U_FETCH(KEY,1,LINKH,IERR)        ! Get hot channel bank
      IF (IERR.NE.1 .OR. LINKH.EQ.0) THEN
        WRITE (STRING,504) IRUN
  504   FORMAT(' Err reading DB for ',I9)
        CALL ERRMSG(STRING,'CHOT',
     &      'Failed to find run in database','I')
        IER = 12
        GO TO 999
      ENDIF
      LCGEV = GZCGEV()
      LCHOT=LC(LCGEV-IZCHOT)
      IF(LCHOT.NE.0)THEN
        CALL MZDROP(IXSTP,LCHOT,'L')
      ENDIF

      CALL ZSHUNT(IXSTP,LINKH,LCGEV,-IZCHOT,1)
      LCHOT = LC(LCGEV-IZCHOT)
C
  999 CALL D3U_END
C
      RETURN
      END
