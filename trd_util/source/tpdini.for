      SUBROUTINE TPDINI(CRUN,MAX_TRDCRT,DB_OPENED,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialzation routine for Pedestals.
C-                         Called by TDBINI.
C-   Inputs  : CRUN = Current Run Number (I6 format)
C-             MAX_TRDCRT = number of crates being read in (count from 0)
C-             DB_OPENED = .TRUE. if DBCLB_INITIALIZE has already been called.
C-   Outputs : DB_OPENED = .TRUE. if DBCLB_INITIALIZE was called.
C-   Controls: IOK = .TRUE. if everything goes well.
C-
C-   Created  20-JUN-1990   Srini Rajagopalan
C-   Updated   4-OCT-1990   JFG adapted to TRD 
C-   Updated  13-AUG-1991   Susan K. Blessing   Add logical DB_OPENED to 
C-    flag if DBCLB_INITIALIZE has been called for this run.  If updating
C-    is necessary for CRUN, then call DBCLB_INITIALIZE if it hasn't already
C-    been called.  Make LORUN and HIRUN arrays and check if CRUN is within
C-    the validity range without looking in the database.
C-   Updated   1-JUN-1992   Susan K. Blessing  Change call to DBCLB_FETCH
C-    to DBCLB_FETCH_OFFLINE.  Remove D0$INC:DBSTP.INC.  DBCLB_FETCH_OFFLINE
C-    returns LDATA and LKEY for CRTID.  Remove UZERO call for OBUF.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$LINKS:IZTPCH.LINK'
      INCLUDE 'D0$LINKS:IZCRCP.LINK'
C
      INTEGER CRUN,PEDRUN,BNKNUM
      INTEGER CRT,CRTID,MAX_TRDCRT,OBUF(0:7)
      INTEGER CHNL,ID,IMIN,IMAX
      INTEGER LTPCH,LINKH
      INTEGER FST_CARD,LST_CARD,ADCS_CRD
      INTEGER LORUN(0:7),HIRUN(0:7)
      INTEGER LTRPD,WIRE,LAYER,UB
      INTEGER GZTRPD
      INTEGER LENGTH,IER
      INTEGER LDATA,LKEY
C
      REAL VALUE(2)
C
      CHARACTER*24 PATH
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
      IF (DB_OPENED) CALL DBCLB_PATH('PEDESTAL','TRD',PATH)
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
            CALL DBCLB_PATH('PEDESTAL','TRD',PATH)
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
          CALL ERRDB(' Error in TPDINI ')
          GO TO 2
        ENDIF
        LTPCH = LDATA
        IF (LTPCH.EQ.0) THEN
          CALL ERRDB(' TPDINI - Error in finding requested data ')
          IOK = .FALSE.
          GO TO 999
        ENDIF
        LORUN(CRT) = IC(LKEY+3)      ! Start Validity
        HIRUN(CRT) = IC(LKEY+4)      ! End validity
C
        LINKH = LC(LTPDH-IZTPCH)
        IF (LINKH.NE.0) THEN
          CALL EZDROP('SCPH')
          CALL MZDROP(IXSTP,LINKH,' ')
        ENDIF
C
        CALL ZSHUNT(IXSTP,LTPCH,LTPDH,-IZTPCH,0)
        LTPCH = LC(LTPDH-IZTPCH)
        PEDRUN = IC(LTPCH+6)
        CALL CPATHG(SPTH)
C
        IF (OBUF(CRT).NE.PEDRUN) THEN
          OBUF(CRT) = PEDRUN
          WRITE(SRCPNAM,5)CRTID,SPTH(4:4)
    5     FORMAT('CRATE_',I3.3,A1)
          CALL EZNAME('SCPH',LTPCH,IZCRCP)
          CALL EZNAME(SRCPNAM,LTPCH,4)
          CALL EZPICK('SCPH')
          CALL EZGSET('FST_CARD',FST_CARD,1)
          CALL EZGSET('LST_CARD',LST_CARD,1)
          CALL EZGSET('ADCS_CRD',ADCS_CRD,1)
          IMIN = (FST_CARD-1)*ADCS_CRD
          IMAX = LST_CARD*ADCS_CRD-1
          CALL RS_NUM_TO_ADDR           ! Initialize Num_to_addr
C
C   *** Fill Logical tree structure
C
          DO 10 CHNL = IMIN,IMAX
            CALL TGTPDS(CHNL,VALUE,1)
            CALL NUM_TO_ADDR(CHNL,ID,1)
C
            IF (ID.GE.0) THEN
              CALL TCODER(ID,LAYER,WIRE,UB,1)  
              LAYER = LAYER + 1
              WIRE  = WIRE  + 1
              IF (UB.EQ.0) THEN
                LTRPD=GZTRPD(LAYER)
C Pedestal sigma not implemented
                C(LTRPD+WIRE)=VALUE(1)
              ENDIF
            ENDIF
   10     CONTINUE
          CALL EZRSET
        ENDIF
C
C  *** Drop Electronic bank structure.
C
        LINKH = LC(LTPDH-IZTPCH)
C
        IF (LINKH.NE.0) THEN
          CALL EZDROP('SCPH')
          CALL MZDROP(IXSTP,LINKH,' ')
        ENDIF
    2 CONTINUE
C
  999 RETURN
      END
