      SUBROUTINE DBCLB_DELETE(PATH,DECT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Delete an entry from the database
C-
C-   Inputs  : PATH - Data base tree structure
C-   Outputs : none
C-   Controls:
C-
C-   Created  18-NOV-1989   J. Green
C-   Updated     DEC-1990   Jan Guida  Removed RZLOC and RZFREE
C-   Updated  15-JUN-1992   S. Abachi  Modified & reorganized to prevent
C-                                     link overwriting problems.
C-   Updated  08-AUG-1992   S. Abachi  Provisons for D0 FZ file output added
C-                                     to be used in conjuction with server
C-                                     (D option).
C-   Updated  25-SEP-1992   S. Abachi  Only one FZ is allowed to be produced
C-
C----------------------------------------------------------------------
      IMPLICIT      NONE
      INCLUDE      'D0$PARAMS:CALIB.DEF'
      INCLUDE      'D0$INC:ZEBSTP.INC'
      INCLUDE      'D0$INC:LKCALIB.INC'
      INCLUDE      'D0$INC:DBSTP.INC'
      INCLUDE      'D0$INC:QUEST.INC'
      CHARACTER*3   DECT
      CHARACTER*(*) PATH
      CHARACTER*40  DBFILE
      INTEGER       KEY(NKYS),KEYO(NKYS),KEY2(NKYS),DKEYS(2*NKYS)
      INTEGER       KEYN(NKYS),KEYC(NKYS),KEYP(NKYS)
      INTEGER       I,LD,LK,LDAT,LKEY
      INTEGER       D(3),T(3)
      CHARACTER*1   ANS
      CHARACTER*80  FILNAM
      INTEGER       LBANK,IR
      INTEGER       CRATE, RUN, SAVE_RUN
      CHARACTER*80  MSG
      CHARACTER*17  CALTYPE
      CHARACTER*1   CHYN
      CHARACTER*7   COPT
      LOGICAL IK
      LOGICAL       PRERUN              ! true if previous run exists
      INTEGER       INPID               ! Process ID
      INTEGER       LINK
      EQUIVALENCE  (CALIB_LNK(1),LBANK)
C
C----------------------------------------------------------------------
C
      CALL VZERO(KEY,NKYS)
cc      IF(LBANK .GT. 0) CALL MZDROP(IDIVDB,LBANK,' ')

      CALL GETPAR ( 1, 'Enter the run number to be deleted:', 'I', RUN )
      IF ( DECT .EQ. 'MUO' ) THEN
        CALL GETPAR ( 1, 'Enter the module to be deleted:', 'I', CRATE )
      ELSE
        CALL GETPAR ( 1, 'Enter the crate to be deleted:', 'I', CRATE )
      ENDIF
      KEY(3) = RUN                      ! start validity
      KEY(4) = RUN                      ! end validity
      KEY(8) = CRATE                    ! Crate Number
C
      CALL DBUSE(PATH,LKEY,LDAT,KEY(3),KEY,'S348')
      IF (IQUEST(1).NE.0) THEN
        CALL ERRDB('DBCLB_DELETE: DBUSE (first call)')
        GO TO 999
      ELSE
        DO 5 I = 1,NKYS
          KEY2(I) = IC(LKEY+I)
    5   CONTINUE
      ENDIF
C
      CALL INTMSG(
     &' **************************************************************')
      WRITE(MSG,15)RUN,CRATE
   15 FORMAT(2x,' Input run Number = ',I9,' CRATE = ',I3)
      CALL INTMSG(MSG)
      WRITE(MSG,20) IC(LDAT+6)
   20 FORMAT(2x,' Calibration Run Number = ',I10)
      CALL INTMSG(MSG)
C
      D(1) = IC(LDAT+7)/10000
      D(2) = IC(LDAT+7)/100 - D(1)*100
      D(3) = IC(LDAT+7) - D(1)*10000 - D(2)*100
      T(1) = IC(LDAT+8)/10000
      T(2) = IC(LDAT+8)/100 - T(1)*100
      T(3) = IC(LDAT+8) - T(1)*10000 - T(2)*100
      WRITE(MSG,25)D(1),D(2),D(3),T(1),T(2),T(3)
   25 FORMAT(2x,' Run taken on Date = ',I2.2,'-',I2.2,'-',I2.2,
     &       ' Time = ',I2.2,':',I2.2,':',I2.2)
      CALL INTMSG(MSG)
      WRITE(MSG,30) KEY2(3), KEY2(4)
   30 FORMAT(3x,'Corrected validity range for this run = '
     &        ,I10,' TO ',I10)
      CALL INTMSG(MSG)
      CALL INTMSG(
     &' **************************************************************')
C
C                          dont delete only run for crate/module
      IF ( KEY2(3).EQ.1 .AND. KEY2(4).EQ.999999999 ) THEN
        CALL INTMSG( ' Do not delete this data. ' )
        GO TO 999
      ELSE
        CALL GETPAR ( 1, ' Delete this run? [Y]: ', 'U', CHYN )
        IF ( CHYN .EQ. 'N' ) THEN
          GO TO 999
        ENDIF
      ENDIF
C
      IF(DOPT .NE. 1) THEN
        CALL DBFREE ( PATH, LKEY, KEY2, 'KS348')
        CALL DBPURK ( PATH, KEY2(3), KEY2, 'S348' )  ! delete run
        IF (IQUEST(1).NE.0) THEN
          CALL ERRDB('DBCLB_DELETE: DBPURK')
          GO TO 998
        ENDIF
      ENDIF
      IF(DOPT .GT. 0) THEN
        COPT = 'S348'
        CALL D0DBL3_WRITFZ('TODO_AREA',IDVSTP,30,PATH,NKYS,KEY2,
     &                       COPT,LINK,IR)
        IF(IR .LT. 0) THEN
          CALL INTMSG(' DBCLB_DELETE: Error in D0DBL3_WRITFZ')
          GOTO 998
        ENDIF
      ENDIF
C
      KEY(3) = KEY2(3) - 1              ! find previous run
      IF ( KEY(3) .EQ. 0 ) THEN            ! no previous run
        KEY(3) = KEY2(4) + 1
        PRERUN = .FALSE.
      ELSE
        SAVE_RUN = KEY2(4)
        KEY(4) = KEY(3)
        CALL DBUSE(PATH,LKEY,LDAT,KEY(3),KEY,'KS348')
        IF (IQUEST(1) .EQ. 24) THEN
          CALL INTMSG(
     &      'DBCLB_DELET: No previous run. But deletion will go on.')
          PRERUN = .FALSE.
        ELSE
          DO I = 1,NKYS
            KEYO(I) = IC(LKEY+I)            ! get previous or next keys
            KEYN(I) = KEYO(I)               ! name new same as old
          ENDDO
          PRERUN = .TRUE.
        ENDIF
      ENDIF
      KEY(4) = KEY(3)
C
C
   97 CONTINUE
      CALL RZCDIR( PATH, 'U')
      IF(.NOT. PRERUN) THEN
        CALL DBUSE(PATH,LKEY,LDAT,KEY(3),KEY,'KS348')
        IF (IQUEST(1).NE.0) THEN
          CALL INTMSG(' No previous/next run. Deletion will go on.')
          GO TO 998
        ENDIF
        DO 10 I = 1,NKYS
          KEYO(I) = IC(LKEY+I)            ! get previous or next keys
          KEYN(I) = KEYO(I)               ! name new same as old
   10   CONTINUE
        KEYN(3) = KEY2(3)
      ELSE
        KEYN(4) = SAVE_RUN
      ENDIF
C
      IF(DOPT .NE. 1) THEN
        CALL DBRENK(PATH,KEYO,KEYN)       ! change keys
        IF (IQUEST(1).NE.0) THEN
          CALL ERRDB('DBCLB_DELETE: DBRENK')
          GO TO 998
        ENDIF
      ENDIF
C
      CALL DBTBPR
  998 CONTINUE
      CALL RZCDIR( PATH, 'U')
  999 RETURN
      END
