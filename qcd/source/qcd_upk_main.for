      FUNCTION QCD_UPK_MAIN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Main program to unpack MDST and make Ntuples
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created 18-DEC-1992   Andrew G. Brandt   Based on J Yu's QCD_ANALYSIS
C_   Updated 28-MAY-1993   Chang L. Kim       For MC don't call QCD_TRIGON
C-   Updated 25-Sep-1993   Andrew G. Brandt   Add possibility of L2NAME
C-                                            choice of triggers
C-   Updated 25-Sep-1993   Andrew G. Brandt   Protect GTGLOB_2
C-   Updated 31-Jan-1994   Andrew G. Brandt   Add QCD_FILTER_BIT_INIT call
C-                                            remove -GLOBWD possibility
C-   Updated 25-Feb-1994   Andrew G. Brandt   Add EVT_CUT, Unique_event
C-   Updated 09-MAR-1994   Andrew G. Brandt   Add IRUN for 1A1B
C-   Updated 18-MAY-1994   Andrew G. Brandt   Add GOODRUN routine
C-   Updated 29-SEP-1994   Andrew G. Brandt   No unique event for MC
C-   Updated 02-NOV-1994   Andrew G. Brandt   Update for CW
C-   Updated 31-JUL-1995   Bob Hirosky   replace qcd_mask w/ logical array
C-   Updated 25-OCT-1995   Andrew G. Brandt   Add VAXT for Bob
C-   Updated 06-DEC-1995   Andrew G. Brandt   Add LUM, UNFILTERED, DO_AIDA,
C-                                            move CAFIX_BEGIN
C-   Updated   2-JAN-1996   Bob Hirosky       move RCP READ to QCD_UPK_INIT
C-                                            set path on each call
C-                                            drop QMS structures
C-   Updated  23-JAN-1996   Bob Hirosky   move filter selection to
C-                                            QCD_NTUP_FILTER
C-   Updated  01-MAR-1996   Andrew Brandt   add muon info
C-   Updated  09-MAR-1996   Andrew G. Brandt  rename QMS_FIX.INC to QMS for SGI
C-   Updated  11-MAR-1996   Andrew G. Brandt  add extra vert wds
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QCD_NTUP_INFO.INC/LIST'
      INCLUDE 'D0$INC:QCD_EVT_INFO.INC/LIST'
      INCLUDE 'D0$INC:QCD_JUTL_HEAD.INC/LIST'
      INCLUDE 'D0$INC:QMS.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      LOGICAL FIRST,QCD_UPK_MAIN
      INTEGER IBAD_WORD,IN_BAD
      LOGICAL EVT_CUT,OK,UNIQUE_EVENT
      LOGICAL UDST_TO_DST, CAFIX_BEGIN
      INTEGER NDUP,RUNTEMP
      REAL LUMS(2), AGE(2)
      INTEGER IER
      INTEGER VAXTIM(2)
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (QMS_DATA) THEN
        CALL PATHST('MDST') ! reset path for QMS DATA
      ENDIF
C
      QCD_UPK_MAIN=.TRUE.
      RUNNUM=IQ(LHEAD+6)   !local run_number
      EVTNUM=IQ(LHEAD+9)
C
C Check if 1A or 1B
C
      IF(RUNNUM.LT.69000) THEN
        IRUN=1
      ELSE
        IRUN=2
      END IF
C
C Intitialization
C
      IF(FIRST)THEN
C
        RUNTEMP=-1
C
C DO CAFIX initialization
C
        OK=CAFIX_BEGIN()
        IF(.NOT.OK) CALL ERRMSG('QCD_UPK_JETS','CAFIX_BEGIN',
     &        'FAILURE!!','F')
C
        CALL INRCP('CAFIX_RCP',IER)
        IF(IER.NE.0) CALL ERRMSG('QCD_UPK_JETS','CAFIX_RCP',
     &        'RCP FILE NOT PRESENT ','F')
C
        FIRST = .FALSE.
      ENDIF
C
C     Get event luminosity
C
      IF(DO_LUM) THEN
        VAXTIM(1) = IQ(LHEAD+4)
        VAXTIM(2) = IQ(LHEAD+5)
        CALL GETLUM(VAXTIM,LUMS,AGE,IER)
        IF(IER.EQ.0) THEN
          LUM=LUMS(1)
        ELSE
          LUM=-1
        END IF
      END IF
C
C For D0MDST need to unpack into ZEBRA form
C
      IF(D0MDST) THEN
        OK=UDST_TO_DST()
        IF(.NOT.OK) CALL ERRMSG('QCD_UPK_MAIN','UDST_TO_DST',
     &        'FAILURE!!','F')
      ENDIF
C
C Verify that event is not a duplicate (not necessary for MC)
C
      IF(.NOT.MC_DATA) THEN
        IF (.NOT.UNIQUE_EVENT(RUNNUM,EVTNUM,NDUP)) GOTO 999
      END IF
C
C At begin of new run write out previos run and number of duplicates
C
      IF (RUNNUM.NE.RUNTEMP) THEN
        RUNTEMP = RUNNUM
        IF(NDUP.GT.0) WRITE(61,990)RUNNUM,NDUP
      END IF
C
C  Check bad flag
C
      CALL QCD_BAD_DST_EVENT(IBAD_WORD,IN_BAD)
      N_BAD=IN_BAD
      BAD_WORD=IBAD_WORD
C
C  Option to not keep events with one or more bad jets
C
      IF(BADTST.EQ.1.AND.BAD_WORD.GT.0) GOTO 999
C
C  Get L1L2, electrons, photons, GLOB, Muons, extra vertwds, AIDA, if desired
C
      IF (DO_L1L2) CALL QCD_UPK_JUTL_JETS
      IF (DO_ELC)  CALL QCD_UPK_ELC
      IF (DO_PHO)  CALL QCD_UPK_PHO
      IF (GLOBWD.GT.0)  CALL QCD_UPK_GLOB
      IF (MUONWD.GT.0)  CALL QCD_UPK_MUONS
      IF (VERTWD.GT.1)  CALL QCD_UPK_VERT
      IF (DO_AIDA) CALL QCD_UPK_AIDA
C
C User cut on event information (anything but jet info)
C
      OK=EVT_CUT()
      IF(.NOT.OK) GO TO 999
C
C  QCD_NTUP_FILL calls jet and PNUT unpacking and books and fills ntuples
C
      CALL QCD_NTUP_FILL
  990 FORMAT(2I7)
C
  999 CONTINUE
      IF (QMS_DATA) THEN
        CALL PATHST('RECO') ! reset path for QMS DATA
      ENDIF
C
      RETURN
      END
