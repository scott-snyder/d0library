      SUBROUTINE TGNINI(CRUN,MAX_TRDCRT,DB_OPENED,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Retrieve Gains from DBL3 and store in Logical STP
C-   tree structure.
C-
C-   Inputs  : CRUN = Run Number
C-             MAX_TRDCRT = Maximum number of TRD crates being analyzed.
C-             DB_OPENED = .TRUE. if DBCLB_INITIALIZE has already been called.
C-   Outputs : DB_OPENED = .TRUE. if DBCLB_INITIALIZE was called.
C-   Controls: IOK = Logical Error flag.
C-
C-   Created  17-AUG-1990   Srini Rajagopalan
C-   Updated   5-OCT-1990   JFG Adapted to TRD 
C-   Updated  13-AUG-1991   Susan K. Blessing   Add logical DB_OPENED to 
C-    flag if DBCLB_INITIALIZE has been called for this run.  If updating
C-    is necessary for CRUN, then call DBCLB_INITIALIZE if it hasn't already
C-    been called.  Make LORUN and HIRUN arrays and check if CRUN is within
C-    the validity range without looking in the database.
C-   Updated   1-JUN-1992   Susan K. Blessing  Change call to DBCLB_FETCH
C-    to DBCLB_FETCH_OFFLINE.  Remove D0$INC:DBSTP.INC.  DBCLB_FETCH_OFFLINE
C-    returns LDATA and LKEY for CRTID.  Remove UZERO call for OBUF.
C-    Change number of elements in VALUE from 3 to 10 to match TGTGNS.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$LINKS:IZTGCH.LINK'
      INCLUDE 'D0$LINKS:IZCRCP.LINK'
C
      INTEGER CRUN,GAINRUN,BNKNUM
      INTEGER CRT,CRTID,MAX_TRDCRT,OBUF(0:7)
      INTEGER CHNL,ID,IMIN,IMAX
      INTEGER LTGCH,LINKH
      INTEGER FST_CARD,LST_CARD,ADCS_CRD
      INTEGER LORUN(0:7),HIRUN(0:7)
      INTEGER LTREL,WIRE,LAYER,UB
      INTEGER GZTREL
      INTEGER LENGTH,IER
      INTEGER LDATA,LKEY
C
      REAL VALUE(10)
C
      CHARACTER*25 PATH
      CHARACTER*10 SRCPNAM
      CHARACTER*4 SPTH
      CHARACTER*80 DBCALIB
C
      LOGICAL IOK,FIRST
      LOGICAL DB_OPENED
C
      PARAMETER (BNKNUM = 6)
      SAVE FIRST,LORUN,HIRUN,DBCALIB
      DATA FIRST /.TRUE./
      DATA LORUN/8*-1/
      DATA HIRUN/8*-1/
      DATA OBUF/8*0/
C
C----------------------------------------------------------------------
C
C  *** Make Logical tree on first call
C
      IOK = .TRUE.
      IF (FIRST) THEN
        CALL EZPICK('TRD_RCP')
        CALL EZGETS('DBCALIB$TRD',1,DBCALIB,LENGTH,IER)
        CALL EZRSET
        FIRST = .FALSE.
      ENDIF
C
C   *** Loop over All crates. Check to see if pedestal Run has changed
C       since last run. If so, update Logical tree structure.
C
      IF (DB_OPENED) CALL DBCLB_PATH('GAINS','TRD',PATH)
      DO 2 CRT = 0,MAX_TRDCRT
C
        IF (CRUN.GE.LORUN(CRT).AND.CRUN.LT.HIRUN(CRT)) THEN
C This crate doesn't need updating.
          GO TO 2
        ELSE
          IF (.NOT.DB_OPENED) THEN
            CALL DBCLB_INITIALIZE(DBCALIB,'S',IOK)
            IF (.NOT.IOK) THEN
              CALL ERRDB(' TDBINI - Error in DBL3 intialization ')
              GO TO 999
            END IF
            CALL DBCLB_PATH('GAINS','TRD',PATH)
            DB_OPENED = .TRUE.
          END IF
        END IF
C
        CRTID = 10*CRT + BNKNUM
        LDATA = 0
        LKEY = 0
C Get Crate ped banks.
        CALL DBCLB_FETCH_OFFLINE(PATH,CRUN,CRTID,LDATA,LKEY)
        IF (IQUEST(1).NE.0) THEN
          CALL ERRDB(' Error in TGNINI ')
          GO TO 2
        ENDIF
        LTGCH = LDATA
        IF (LTGCH.EQ.0) THEN
          CALL ERRDB(' TGNINI - Error in finding requested data ')
          IOK = .FALSE.
          GO TO 999
        ENDIF
        LORUN(CRT) = IC(LKEY+ 3)      ! Start Validity
        HIRUN(CRT) = IC(LKEY+4)      ! End validity
C
        LINKH = LC(LTGAI-IZTGCH)
        IF (LINKH.NE.0) THEN
          CALL EZDROP('SCPH')
          CALL MZDROP(IXSTP,LINKH,' ')
        ENDIF
C
        CALL ZSHUNT(IXSTP,LTGCH,LTGAI,-IZTGCH,0)
        LTGCH = LC(LTGAI-IZTGCH)
        GAINRUN = IC(LTGCH+6)
C
        CALL CPATHG(SPTH)               ! ?
        IF (OBUF(CRT).NE.GAINRUN) THEN
          OBUF(CRT) = GAINRUN
          WRITE(SRCPNAM,5)CRTID,SPTH(4:4)
    5     FORMAT('CRATE_',I3.3,A1)
          CALL NASRCP('SCPH',LTGCH,IZCRCP)
          CALL NASRCP(SRCPNAM,LTGCH,4)
          CALL SLSRCP('SCPH')
          CALL GTSRCP('FST_CARD',FST_CARD,1)
          CALL GTSRCP('LST_CARD',LST_CARD,1)
          CALL GTSRCP('ADCS_CRD',ADCS_CRD,1)
          IMIN = (FST_CARD-1)*ADCS_CRD
          IMAX = LST_CARD*ADCS_CRD-1
          CALL RS_NUM_TO_ADDR           ! Initialize Num_to_addr
C
C   *** Fill Logical tree structure
C
          DO 10 CHNL = IMIN,IMAX
            CALL TGTGNS(CHNL,VALUE,1)
            CALL NUM_TO_ADDR(CHNL,ID,1)
            IF (ID.GE.0) THEN
              CALL TCODER(ID,LAYER,WIRE,UB,1)  
              LAYER = LAYER + 1
              WIRE  = WIRE  + 1
              IF (UB.EQ.0) THEN
                LTREL=GZTREL(LAYER)
C Pedestal sigma not implemented
                C(LTREL+WIRE)=VALUE(3)
              ENDIF
            ENDIF
   10     CONTINUE
        ENDIF
C
C  *** Drop Electronic bank structure.
C
        LINKH = LC(LTGAI-IZTGCH)
        IF (LINKH.NE.0) THEN
          CALL EZDROP('SCPH')
          CALL MZDROP(IXSTP,LINKH,' ')
        ENDIF
    2 CONTINUE
C       
  999 RETURN
      END
