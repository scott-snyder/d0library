      SUBROUTINE DBARC_DELETE(PATH,DECT,CRATE,RUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Delete entries from the database
C-
C-   Inputs  : PATH  -  Data base tree structure
C-             DECT  -  Detector name
C-             CRATE -  Crate number
C-             RUN   -  Run number to be deleted
C-   Outputs :
C-   Controls: 
C-
C-   Created  22-MAR-1991   S. ABACHI  (made by modifying dbclb_delete)
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
      INTEGER       KEY(NKYS),KEYO(NKYS),KEYN(NKYS),KEYC(NKYS)
      INTEGER       I,LD,LK,LDAT,LKEY
      INTEGER       D(3),T(3)
      CHARACTER*1   ANS
      INTEGER       LBANK
      INTEGER       CRATE, RUN, SAVE_RUN
      CHARACTER*80  MSG
      CHARACTER*1   CHYN
      LOGICAL       PRERUN              ! true if previous run exists
      INTEGER       INPID               ! Process ID
      EQUIVALENCE  (CALIB_LNK(1),LBANK)
      INTEGER         IDEBDB, IDISDB, IDIVDB, IHKYDB, IKDRDB, IONODB    
     +              , KOFSDB, KOFUDB, LBDADB, LBFXDB, LBFYDB, LBKYDB    
     +              , LBNODB, LFIXDB, LSAVDB, LTOPDB, LPRTDB, NTOPDB    
      COMMON /DBUSER/ IDEBDB, IDISDB, IDIVDB, IHKYDB, IKDRDB, IONODB    
     +              , KOFSDB, KOFUDB, LBDADB, LBFXDB, LBFYDB, LBKYDB    
     +              , LBNODB, LFIXDB, LSAVDB, LTOPDB, LPRTDB, NTOPDB    
C
C----------------------------------------------------------------------
C
      CALL VZERO(KEY,NKYS)
      CALL DBNODE(PATH,LBNODB)
      CALL MZDROP(IDIVDB,LBNODB,'L')
C
      KEY(3) = RUN                      ! start validity
      KEY(4) = RUN                      ! end validity
      KEY(8) = CRATE                    ! Crate Number
C
      CALL DBUSE(PATH,LKEY,LDAT,KEY(3),KEY,'S348')
      IF (IQUEST(1).NE.0) THEN
        CALL ERRDB('DBCLB_DELETE: DBUSE (first call)')
        GO TO 999
      ENDIF
      DO 5 I = 1,NKYS
        KEYO(I) = IC(LKEY+I)            ! get returned keys
    5 CONTINUE
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
      WRITE(MSG,30) KEYO(3), KEYO(4)
   30 FORMAT(3x,'Corrected validity range for this run = '
     &        ,I10,' TO ',I10)
      CALL INTMSG(MSG)
      CALL INTMSG(
     &' **************************************************************')
C
C                          dont delete only run for crate/module
      IF ( KEYO(3).EQ.1 .AND. KEYO(4).EQ.999999999 ) THEN
        CALL INTMSG( ' Do not delete this data. ' )
        GO TO 999
      ENDIF
C
      CALL RZCDIR( PATH, ' ')            ! set CWD before lock
      CALL DBFREE ( PATH, LKEY, KEYO, 'KS348' )
      IF (IQUEST(1).NE.0) THEN
        CALL ERRDB('DBCLB_DELETE: DBFREE')
        GO TO 998
      ENDIF      
      CALL DBPURK ( PATH, KEYO(3), KEYO, 'S348' )  ! delete run
      IF (IQUEST(1).NE.0) THEN
        CALL ERRDB('DBCLB_DELETE: DBPURK')
        GO TO 998
      ENDIF
C
      WRITE(MSG,101) RUN
  101 FORMAT(' Run number ',I9.9,' successfully deleted ')
      CALL INTMSG(MSG)
C
  998 CALL RZCDIR( PATH, ' ')           ! set CWD before free
      RETURN
C
  999 RETURN
      END
