      SUBROUTINE DBCLB_INSERT(PATH,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Inserts a calibration bank into the DBL3 database
C-
C-   Inputs  : PATH - Data base tree structure
C-   Outputs : IOK   - .TRUE. if ok
C-   Controls:
C-
C-   Created  18-NOV-1989   S. Abachi, Jan Guida, S. Rajagopalan
C-   Updated  28-FEB-1990   J. Guida - Add KEY(11) as Run number
C-   Updated   9-JAN-1991   Srini Rajajopalan  Change DBUSE call to be only keys
C-   Updated  02-FEB-1991   S. Abachi ; Posibility of special backup file added
C-   Updated  26-FEB-1991   Jan Guida, Srini Rajagopalan   If duplicate run
C-                                      delete before re-entering it
C-   Updated  15-APR-1991   Jan Guida  Allow start validity to be a number
C-                                      other than pedrun
C-   Updated   4-JUN-1991   Jan Guida  Add calls to RZSTAT (beginning and end)
C-   Updated  30-AUG-1991   S. Abachi  Made changes visible after insertion
C-   Updated     Nov-1991   J.Green    reduce printed output
C-   Updated  09-MAY-1992   S. Abachi  Modified to handel database without
C-                                     empty banks. (Backwards compatible).
C-   Updated     JUL-1992   J.Green    remove calls to RZCDIR
C-                                     implement server mode
C-   Updated     Jul-1993   J.Green    fix to find entry which has later
C-                                  validity range than the one to be inserted
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CALIB.DEF'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:LKCALIB.INC'
      INCLUDE 'D0$INC:DBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
C
      INTEGER LBANK                     ! link to bank being inserted
      INTEGER IDATE
      INTEGER KEY(NKYS),KEYO(NKYS),KEYN(NKYS),KEYC(NKYS)
      INTEGER I,LD,LK,LDAT,LKEY,IR,FSTRUN
      CHARACTER*80  MSG
C
      CHARACTER*(*) PATH
      CHARACTER*8 COPT
      LOGICAL IOK,DUPLICATE,IEMPTY
      EQUIVALENCE (CALIB_LNK(1),LBANK)
C
C----------------------------------------------------------------------
C
      IOK = .TRUE.                      ! assume success
      IEMPTY = .FALSE.
      DUPLICATE = .FALSE.
      CALL VZERO(KEY,NKYS)
      KEY(3) = IC(LBANK+6)              ! start validity
      IF (IC(LBANK+4).GT.IC(LBANK+6)) KEY(3) = IC(LBANK+4)
      KEY(4) = IC(LBANK+5)              ! end validity
      KEY(8) = IC(LBANK+9)              ! Crate Number
      IDATE = IC(LBANK+7)/100 + MOD(IC(LBANK+7),100)*10000
      CALL DBPKTS(IDATE, IC(LBANK+8), KEY(9))
      KEY(11) = IC(LBANK+6)             ! Run number
C
      IF(DOPT .GT. 0) THEN
        COPT = 'R-KS348'
        CALL D0DBL3_WRITFZ('TODO_AREA',IDVSTP,10,PATH,NKYS,KEY,COPT,
     &                        LBANK,IR)
        IF(IR .LT. 0)THEN
          CALL INTMSG(' DBCLB_ENTER: error in making FZ header')
          GOTO 999
        ENDIF
      ENDIF
C
      IF(DOPT.EQ.0 .OR. DOPT.EQ.2) THEN
C
        CALL RZCDIR(PATH,'U')
        CALL DBPKTS((IC(LBANK+7)/100 + MOD(IC(LBANK+7),100)*10000),
     &               IC(LBANK+8), KEY(9))
        IF (IQUEST(1).NE.0) THEN
          CALL ERRDB('DBPKTS:  Error packing date and time')
          IOK = .FALSE.
          GO TO 999
        ENDIF
C
        KEYC(3) = KEY(3)                  ! start validity
        KEYC(4) = KEY(3)                  ! end validity
        KEYC(8) = KEY(8)                  ! crate Number
C
        CALL DBUSE(PATH,LKEY,LDAT,KEY(3),KEYC,'KS348')
C
        IF (IQUEST(1) .NE. 0 .AND. IQUEST(1) .NE. 24) THEN
          CALL ERRDB('DBUSE')
          IOK = .FALSE.
          CALL DBCLB_FINISH
          GO TO 999
        ELSEIF(LKEY .EQ. 0 .OR. IQUEST(1) .EQ. 24) THEN
C                                      try to find entry with later run
          CALL DBUSE(PATH,LKEY,LDAT,KEY(3),KEYC,'KS48')
          IF (IQUEST(1) .NE. 0 .AND. IQUEST(1) .NE. 24) THEN
            CALL ERRDB('DBUSE')
            IOK = .FALSE.
            CALL DBCLB_FINISH
            GO TO 999
          ELSEIF(LKEY .EQ. 0 .OR. IQUEST(1) .EQ. 24) THEN
            WRITE(MSG,11) IQUEST(1),KEY(8)
   11       FORMAT(' IQUEST(1) =',I3,
     &        ' signifies no previous entry for crate ',I3,
     &        '. Not to worry.')
            CALL INTMSG(MSG)
            IEMPTY = .TRUE.
            GOTO 21
          ELSE                            ! found something later
C                                       search through linear struc. to find
C                                       earliest run
            FSTRUN = 999999999
            DO WHILE (LKEY.GT.0)
              FSTRUN = MIN(IC(LKEY+3),FSTRUN)
              LKEY = LC(LKEY)             
            ENDDO
            IF (FSTRUN.GT.KEY(3)) THEN
              KEY(4) = FSTRUN-1     ! fix end validity of run being entered
              GO TO 21
            ELSE
              WRITE(MSG,22) FSTRUN,KEY(8)
   22         FORMAT (' DBUSE failed to find run ',I9,' for crate/mod ',
     &                  I5 )
              CALL INTMSG(MSG)
              CALL DBCLB_FINISH
              GOTO 999
            ENDIF
          ENDIF
        ENDIF
C
        DO 10 I = 1,NKYS
          KEYO(I) = IC(LKEY+I)        ! get old keys
          KEYN(I) = KEYO(I)               ! name new same as old
   10   CONTINUE
C
        IF (KEYN(3).NE.KEY(3)) THEN
          KEYN(4) = KEY(3) - 1            ! Overwrite End Validity of
        ELSE                              ! previous Run
          DUPLICATE = .TRUE.
        ENDIF
C
        KEY(4) = KEYO(4)                  ! Overwrite End Validity of
C                                       ! Current Run
C
        IF (DUPLICATE) THEN
C        CALL RZCDIR( PATH, ' ')            ! set CWD before lock
          CALL DBFREE ( PATH, LKEY, KEYN, 'KS348' )
          IF (IQUEST(1).NE.0) THEN
            CALL ERRDB('DBCLB_DELETE: DBFREE')
            CALL DBCLB_FINISH
            GO TO 999
          ENDIF
          CALL DBPURK ( PATH, KEYN(3), KEYN, 'S348' )  ! delete run
          IF (IQUEST(1).NE.0) THEN
            CALL ERRDB('DBCLB_DELETE: DBPURK')
            CALL DBCLB_FINISH
            GO TO 999
          ENDIF
          WRITE(MSG,100)KEYN(3),KEYN(8)
  100     FORMAT(' Duplicate Run ',I9,' crate/module ',I3,
     &    ' deleted from database ')
        ELSE
          CALL DBRENK(PATH,KEYO,KEYN)
          IF (IQUEST(1).NE.0) THEN
            CALL ERRDB('DBRENK:  Error modifying keys')
            IOK = .FALSE.
            CALL DBCLB_FINISH
            GO TO 999
          ENDIF
        ENDIF
C
C      CALL DBTBPR
   21   CONTINUE
C
        IF(OPTB) THEN
          CALL DBENTR(PATH,LK,LD,IDVSTP,LBANK,NKYS,KEY,0,'RB')
        ELSE
          CALL DBENTR(PATH,LK,LD,IDVSTP,LBANK,NKYS,KEY,0,'R')
        ENDIF
        IF (IQUEST(1).NE.0) THEN
          CALL ERRDB('DBENTR:  Error in entering data in data base')
          IOK = .FALSE.
          IF(.NOT. IEMPTY) THEN
            CALL DBRENK(PATH,KEYN,KEYO)
            IF (IQUEST(1).NE.0) THEN
              CALL ERRDB('DBRENK:  Error re-modifying keys')
            ENDIF
          ENDIF
          CALL DBCLB_FINISH
          GO TO 999
        ENDIF
        CALL RZCDIR( PATH, 'U')            ! make it visible
      ENDIF                                ! DOPT
C
C  999 CALL RZSTAT(PATH,2,' ')
  999 CONTINUE
      RETURN
      END
