      FUNCTION GAP_NTUP_FILL()
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fills extra gap words
C-   Returns FALSE if GAPJ_CUT not satisfied
C-   Created 07-NOV-1994  Andrew G. Brandt Calls gap routines for CW ntup maker
C-   Updated 02-SEP-1995  Andrew G. Brandt add DO_LOHITS and DO_TRACKING
C-   Updated 13-NOV-1995  Andrew G. Brandt add GAP_CATE_INFO
C-   Updated 25-JAN-1996  Bob Hirosky   ADD SAMUS HITS INFO
C-   Updated 29-FEB-1996  Andrew G. Brandt  Move SAMUS to GAP_SAMULT
C-   Updated 08-MAR-1996  Andrew G. Brandt  DO_TRACKING if CDC or FDC
C-   Updated 10-MAR-1996  Andrew G. Brandt  Add GAP_FSEG_FILL
C-   Updated 18-MAR-1996  Andrew G. Brandt  change l0hits to warning from F
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:QCD_JUTL_HEAD.INC/LIST'
      INCLUDE 'D0$INC:QCD_NTUP_INFO.INC/LIST'
      INCLUDE 'D0$INC:QCD_JET.INC/LIST'
      INCLUDE 'D0$INC:QCD_EVT_INFO.INC/LIST'
      INCLUDE 'D0$INC:GAP_CATD_INFO.INC/LIST'
      INCLUDE 'D0$INC:GAP_SAMULT.INC/LIST'
C
      LOGICAL GAPJ_CUT,GAP_NTUP_FILL,UPK_L0_HITS,DO_TRACKING, OK
      INTEGER I,IER
C----------------------------------------------------------------------
C
C If event does not pass GAP ntup user jet cuts do not make ntuple
C
      IF(NJETS.GE.2) THEN
        IF(ETAJ(1).GT.ETAJ(2)) THEN
          IL=2
          IH=1
        ELSE
          IH=2
          IL=1
        ENDIF
        DELTA_ETA_C = ETAJ(IH)-ETAJ(IL)-2.0*GAP_CONE_RADIUS
        ETA_BOOST = (ETAJ(IH)+ETAJ(IL))/2.0
      ELSE
        DELTA_ETA_C = -999.0
        ETA_BOOST = -999.0
      ENDIF
C
      GAP_NTUP_FILL=GAPJ_CUT()
      IF(.NOT.GAP_NTUP_FILL) GO TO 999
C
C Get Gap CATD/CATE info
C
      IF(CATDE.EQ.0) THEN
        CALL GAP_CATD_INFO
      ELSE
        CALL GAP_CATE_INFO
      END IF
C
C Call tracking routine
C
      IF(DO_TRACK.OR.DO_FDCTRK) THEN
        OK = DO_TRACKING()
        IF(.NOT.OK) CALL ERRMSG('GAP_NTUP_FILL','GAP_NTUP_FILL',
     &              ' DO_TRACKING false ','F')
      END IF
C
C FDC segments
C
      IF(DO_FDCSEG) CALL GAP_FSEG_FILL
C
C Fill L0HITS information
C
      IF(DO_L0HITS) THEN
        OK = UPK_L0_HITS()
        IF(.NOT.OK) CALL ERRMSG('GAP_NTUP_FILL','GAP_NTUP_FILL',
     &              ' DO_LOHITS false ','W')
      END IF
C
C ****  FILL SAMUS HITS
C
      IF(DO_SAMULT) THEN
        CALL SAMULT(NHITS_SAMUS,IER)
        IF (IER.LT.0) THEN
          DO I = 1,6
            NHITS_SAMUS(I) = -1
          ENDDO
        ENDIF
        DO I = 1,6
          NHITS_SAMUS(I) = MIN(NHITS_SAMUS(I),126)
        ENDDO
      ENDIF
C
  999 RETURN
      END
