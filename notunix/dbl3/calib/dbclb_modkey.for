      SUBROUTINE DBCLB_MODKEY(PATH,DECT,CALTYPE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Reads a pedestal file from DBL3 database
C-
C-   Inputs  :   PATH  -  path name needed for access to the database
C-               DECT  -  detector type
C-               CALTYPE  - calibration type
C-   Outputs :
C-   Controls:
C-
C-   Created   9-JUN-1990   Jan Guida
C-   Updated  21-MAR-1991   Jan Guida  Allow user to modify key=999999999
C-   Modified 15-JUN-1992   S. Abachi   Previous and next run were treated
C-   Updated  08-AUG-1992   S. Abachi  Provisons for D0 FZ file output added
C-                                     to be used in conjuction with server
C-                                     (D option).
C-   Updated  14-AUG-1992   J.Green    repair bugs in previous version
C-   Modified 25-SEP-1992   S. Abachi  Only one FZ is produced
C-   Updated   8-OCT-1992   Jan Guida  Modifies both clb and hst
C-   Modified  Jul_93       J.Green    allow user to modify run without giving
C-                                     exact run number
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CALIB.DEF'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:DBSTP.INC'
      INCLUDE 'D0$INC:LKCALIB.INC'
      INCLUDE 'D0$INC:QUEST.INC'
C
      CHARACTER*(*) CALTYPE,DECT
      CHARACTER*80 MSG
      CHARACTER*25 PATH,OLD_PATH
      CHARACTER*1 ANS
      CHARACTER*6 CRMOD
      LOGICAL FL99
      INTEGER CALRUN,CRATE
      INTEGER PEDRUN,LBANK
      INTEGER DATE,TIME,D(3),T(3)
      INTEGER I,NEWRUN,IERR
      INTEGER ITYP,NTYP1,NTYP2
      INTEGER KEYC(NKYS),KEYN(NKYS),KEYP(NKYS)
      INTEGER KEYC_OLD(NKYS),KEYN_OLD(NKYS),KEYP_OLD(NKYS)
      INTEGER KEYS(2*NKYS),IR,LINK
      CHARACTER*10 COPT
C
      EQUIVALENCE(CALIB_LNK(1),LBANK)
C
C----------------------------------------------------------------------
C
      FL99 = .FALSE.
      CALL VZERO(KEYP(1),NKYS)
      CALL VZERO(KEYC(1),NKYS)
      CALL VZERO(KEYN(1),NKYS)
C
      CALL GETPAR(1,' Enter Calibration run number to be modified > ',
     &  'I',CALRUN)
      IF(DECT.EQ.'MUO')THEN
        CRMOD = 'Module'
        CALL GETPAR(1,' Enter Module Number > ','I',CRATE)
      ELSE
        CRMOD = 'Crate'
        CALL GETPAR(1,' Enter Crate Number > ','I',CRATE)
      ENDIF
      IF (CRATE.GT.MAXCRT) THEN
        CALL INTMSG(' Invalid Crate Number ')
        GO TO 999
      ENDIF
C
      IF (CALRUN.LE.1 .OR. CALRUN.GT.999999990) CALRUN = 999999990
C
      CALL GETPAR(1,
     &  ' Modify keys for calibration, histogram, both (C,H,B)',
     &  'U',ANS)
      NTYP1 = 1
      NTYP2 = 2
      IF (ANS.EQ.'C') NTYP2 = 1
      IF (ANS.EQ.'H') NTYP1 = 2
      OLD_PATH = PATH
C***  Setup path for calibration mode
      CALTYPE(17:17)=' '
      CALL DBCLB_PATH(CALTYPE,DECT(1:3),PATH)
      CALL INTMSG(' Modify keys for calibration run')
C
      DO ITYP = NTYP1, NTYP2
        IF (ITYP.EQ.1) THEN
          CALL INTMSG(' Processing clb constants')
        ELSE
          CALL INTMSG(' Processing histograms')
        ENDIF
C
C- Current run
C
        CALL DBCLB_GTKEY(PATH,CALRUN,CRATE,KEYC,IERR)
        IF(IERR .NE. 0 .OR. CALRUN .NE. KEYC(11))THEN
          WRITE(MSG,20)CALRUN
   20     FORMAT(2x,' Run ',I10,' does not exist')
          CALL INTMSG(MSG)
          WRITE (MSG,21) KEYC(11)
   21     FORMAT(' Modify Run ',I10,' Instead? Y/[N] ')
          CALL GETPAR(1, MSG, 'U', ANS)
          IF (ANS.NE.'Y')               GO TO 999
        ENDIF
C
        PEDRUN = KEYC(11)
        DO I=1,NKYS
          KEYC_OLD(I) = KEYC(I)
        ENDDO
C
C- Previous run
C
        IF (KEYC(3).NE.1) THEN
          CALL DBCLB_GTKEY(PATH,KEYC(3)-1,CRATE,KEYP,IERR)
          IF(IERR .EQ. 0) THEN
            DO I=1,NKYS
              KEYP_OLD(I) = KEYP(I)
            ENDDO
            WRITE(MSG,23)CRMOD,KEYP(8),KEYP(11)
   23       FORMAT(' Previous calib run for ',A,I4,' is ',I10)
            CALL INTMSG(MSG)
            WRITE(MSG,26)KEYP(3),KEYP(4)
   26       FORMAT(' with validity ' ,I10,' to ',I10)
            CALL INTMSG(MSG)
          ELSE
            CALL INTMSG(' There does not exist a previous run ')
          ENDIF
        ELSE
          CALL INTMSG(' There does not exist a previous run ')
        ENDIF
C
C- Next run
C
        IF (KEYC(4).NE.999999999) THEN
          CALL DBCLB_GTKEY(PATH,KEYC(4)+1,CRATE,KEYN,IERR)
          IF(IERR .EQ. 0) THEN
            DO I=1,NKYS
              KEYN_OLD(I) = KEYN(I)
            ENDDO
            IF (KEYN(4).NE.0) THEN
              WRITE(MSG,24)CRMOD,KEYN(8),KEYN(11)
   24         FORMAT(' Next calib run for ',A,I4,' is ',I10)
              CALL INTMSG(MSG)
              WRITE(MSG,26)KEYN(3),KEYN(4)
              CALL INTMSG(MSG)
            ELSE
              CALL INTMSG(' There does not exist a next run ')
            ENDIF
          ELSE
            CALL INTMSG(' There does not exist a next run ')
          ENDIF
        ELSE
          CALL INTMSG(' There does not exist a next run ')
        ENDIF
C
C-------------------------------------------------------------------------
C
        IF (KEYC(3).EQ.1) GO TO 60
   30   WRITE(MSG,31)PEDRUN,KEYC(3)
   31   FORMAT(' The begin validity for Run ',I10,' is ',I10)
        CALL INTMSG(MSG)
        CALL GETPAR(1,' Would you like to change it (Y/N)?  ','U',ANS)
        IF(ANS.EQ.'Y')THEN
          CALL GETPAR(1,' Enter new begin validity > ','I',NEWRUN)
          IF (NEWRUN.LE.KEYP(3) .OR. NEWRUN.GT.KEYC(4))THEN
            CALL INTMSG(' Invalid run number')
            GO TO 30
          ENDIF
          WRITE(MSG,32)PEDRUN,NEWRUN
   32     FORMAT(' New start validity for Run ',I10,' will be ',I10)
C
          IF(KEYP(4) .NE. 0) THEN
            CALL INTMSG(MSG)
            WRITE(MSG,33)KEYP(11),KEYP(3),NEWRUN-1
   33       FORMAT(' Run ',I10,' validity range will now be ',
     &        I10,' to ',I10)
            CALL INTMSG(MSG)
          ENDIF
          CALL GETPAR(1,' Is this correct (Y/N)?  ','U',ANS)
          IF(ANS.EQ.'Y')THEN
            KEYC(3) = NEWRUN
            IF(KEYP(4) .NE. 0)         KEYP(4) = NEWRUN-1
          ENDIF
        ENDIF
C
   60   CONTINUE
C      IF (KEYC(4).EQ.999999999) GO TO 70
   40   WRITE(MSG,41)PEDRUN,KEYC(4)
   41   FORMAT(' The end validity for run ',I10,' is ',I10)
        CALL INTMSG(MSG)
        CALL GETPAR(1,' Would you like to change it (Y/N)?  ','U',ANS)
        IF(ANS.EQ.'Y')THEN
          CALL GETPAR(1,' Enter new end validity > ','I',NEWRUN)
          IF (KEYC(4).EQ.999999999.AND.NEWRUN.GE.KEYC(3)) THEN
            WRITE(MSG,65)NEWRUN
   65       FORMAT(
     &' WARNING: End validity of 999999999 is being changed to ',I9)
            CALL OUTMSG(MSG)
            CALL GETPAR(1,' Is this correct [N] > ','U',ANS)
            IF (ANS.EQ.'Y') THEN
              KEYC(4) = NEWRUN
              FL99 = .TRUE.
            ENDIF
          ELSE
            IF (NEWRUN.GT.KEYC(3).AND.KEYN(4).EQ.0) THEN
              FL99 = .TRUE.
            ELSE IF (NEWRUN.LE.KEYC(3).OR.NEWRUN.GE.KEYN(4))THEN
              CALL INTMSG(' Invalid run number')
              GO TO 40
            ENDIF
            WRITE(MSG,42)PEDRUN,NEWRUN
   42       FORMAT(' New end validity for run ',I10,'  will be ',I10)
            CALL INTMSG(MSG)
C
            IF(KEYN(4) .NE. 0) THEN
              WRITE(MSG,43)KEYN(11),NEWRUN+1,KEYN(4)
   43         FORMAT(' Next run ',I10,' validity range will now be ',
     &          I10,' to ',I10)
              CALL INTMSG(MSG)
            ENDIF
            CALL GETPAR(1,' Is this correct (Y/N)?  ','U',ANS)
            IF(ANS.EQ.'Y')THEN
              KEYC(4) = NEWRUN
              KEYN(3) = NEWRUN+1
            ENDIF
          ENDIF
        ENDIF
C
   70   CONTINUE
C
        IF (KEYP(4) .NE. 0 .AND. KEYP(4).NE.KEYP_OLD(3)) THEN
          IF(DOPT .NE. 1) THEN
            CALL DBRENK(PATH,KEYP_OLD,KEYP)
            IF (IQUEST(1).NE.0) THEN
              CALL ERRDB('DBRENK:  Error modifying keys')
              CALL DBCLB_FINISH
              GO TO 999
            ENDIF
          ENDIF
        ENDIF
C
  100   CONTINUE
C
        IF (KEYC(3).NE.KEYC_OLD(3) .OR. KEYC(4).NE.KEYC_OLD(4)) THEN
          IF(DOPT .NE. 1) THEN
            CALL DBRENK(PATH,KEYC_OLD,KEYC)
            IF (IQUEST(1).NE.0) THEN
              CALL ERRDB('DBRENK:  Error modifying keys')
              CALL DBCLB_FINISH
              GO TO 999
            ENDIF
          ENDIF
          IF(DOPT .GT. 0) THEN
            DO I=1,NKYS
              KEYS(I) = KEYC_OLD(I)
              KEYS(I+NKYS) = KEYC(I)
            ENDDO
            COPT = 'KS348'
            CALL D0DBL3_WRITFZ('TODO_AREA',IDVSTP,50,PATH,NKYS*2,KEYS,
     &                        COPT,LINK,IR)
            IF(IR .LT. 0)THEN
              CALL INTMSG(' DBCLB_MODKEY: error in making FZ header')
              CALL DBCLB_FINISH
              GO TO 999
            ENDIF
          ENDIF
        ENDIF
C
        IF (KEYN(4) .NE. 0 .AND. .NOT.FL99) THEN
          IF(DOPT .NE. 1) THEN
            CALL DBRENK(PATH,KEYN_OLD,KEYN)
            IF (IQUEST(1).NE.0) THEN
              CALL ERRDB('DBRENK:  Error modifying keys')
              CALL DBCLB_FINISH
              GO TO 999
            ENDIF
          ENDIF
        ENDIF
C
C***  Setup path for histogram mode
        CALTYPE(17:17)='H'
        CALL DBCLB_PATH(CALTYPE,DECT(1:3),PATH)
        CALL INTMSG(' Modify keys for histogram run')
      ENDDO
      PATH = OLD_PATH
C
  999 RETURN
      END
