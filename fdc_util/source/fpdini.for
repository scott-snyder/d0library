      SUBROUTINE FPDINI(CRUN,MAX_FDCRT,DB_OPENED,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialzation routine for Pedestals.
C-                         Called by FDBINI.
C-   Inputs  : CRUN = Current Run Number (I6 format)
C-             MAX_FDCRT = number of crates being read in (count from 0)
C-             DB_OPENED = .TRUE. if DBCLB_INITIALIZE has already been called.
C-   Outputs : DB_OPENED = .TRUE. if DBCLB_INITIALIZE was called.
C-   Controls: IOK = .TRUE. if everything goes well.
C-
C-   Created  20-JUN-1990   Srini Rajagopalan
C-   Updated  20-MAR-1991   Jeffrey Bantly  change call to FGTPDS params 
C-   Updated  13-AUG-1991   Susan K. Blessing   Add logical DB_OPENED to 
C-    flag if DBCLB_INITIALIZE has been called for this run.  If updating
C-    is necessary for CRUN, then call DBCLB_INITIALIZE if it hasn't already
C-    been called.  Make LORUN and HIRUN arrays and check if CRUN is within
C-    the validity range without looking in the database.
C-   Updated  23-SEP-1991   Susan K. Blessing  Replace accidentally 
C-    deleted line.
C-   Updated  12-MAY-1992   Susan K. Blessing  Remove FDLOGI.INC. 
C-   Updated   1-JUN-1992   Susan K. Blessing  Change call to DBCLB_FETCH
C-    to DBCLB_FETCH_OFFLINE.  Remove D0$INC:DBSTP.INC.  DBCLB_FETCH_OFFLINE
C-    returns LDATA and LKEY for CRTID.  Remove UZERO call for OBUF.
C-   Updated  21-SEP-1992   Susan K. Blessing  Add message about reading
C-    pedestals for first time through.
C-   Updated  11-DEC-1992   Susan K. Blessing  Add BYPASS_DBL3_ERROR
C-    switch.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$LINKS:IZFPCH.LINK'
      INCLUDE 'D0$LINKS:IZCRCP.LINK'
C
      INTEGER CRUN,PEDRUN,BNKNUM
      PARAMETER (BNKNUM = 5)
      INTEGER CHNL,ID,IMIN,IMAX
      INTEGER CRT,CRTID,MAX_FDCRT,OBUF(0:11)
      INTEGER LKFPSE,LFPCH,LINKH,LSRCP
      INTEGER FST_CARD,LST_CARD,ADCS_CRD
      INTEGER GZFPSE
      INTEGER LORUN(0:11),HIRUN(0:11)
      INTEGER NMPARW 
      INTEGER LENGTH,IER
      INTEGER LDATA,LKEY
      INTEGER HA, UN, QU, SE, WI, UB
C
      REAL VALUE(2)
C
      CHARACTER*25 PATH
      CHARACTER*10 SRCPNAM
      CHARACTER*4 SPTH
      CHARACTER*80 DBCALIB
C
      LOGICAL IOK,FIRST
      LOGICAL DB_OPENED
      LOGICAL BYPASS_DBL3_ERROR
C
      SAVE FIRST,LORUN,HIRUN,DBCALIB
      DATA FIRST /.TRUE./
      DATA LORUN/12*-1/
      DATA HIRUN/12*-1/
      DATA OBUF/12*0/
      DATA BYPASS_DBL3_ERROR/.FALSE./
C
C----------------------------------------------------------------------
C
      IOK = .TRUE.
      IF (FIRST) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGETS('DBCALIB$FDC',1,DBCALIB,LENGTH,IER)
        CALL EZGET('BYPASS_DBL3_ERROR',BYPASS_DBL3_ERROR,IER)
        CALL EZRSET
        FIRST = .FALSE.
        CALL INTMSG(' Reading FDC pedestals.')
      ENDIF
C
C   *** Loop over All crates. Check to see if pedestal Run has changed
C       since last run. If so, update Logical tree structure.
C
      IF (DB_OPENED) CALL DBCLB_PATH('PEDESTAL','FDC',PATH)
      DO 2 CRT = 0,MAX_FDCRT
C
        IF (CRUN.GE.LORUN(CRT).AND.CRUN.LT.HIRUN(CRT)) THEN
C This crate doesn't need updating.
          GO TO 2
        ELSE
          IF (.NOT.DB_OPENED) THEN
            CALL DBCLB_INITIALIZE(DBCALIB,'S',IOK)
            IF (.NOT.IOK) THEN
              CALL ERRDB(' FDBINI - Error in DBL3 intialization ')
              GO TO 999
            END IF
            CALL DBCLB_PATH('PEDESTAL','FDC',PATH)
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
          IF (BYPASS_DBL3_ERROR) THEN
            CALL ERRDB(' Error in FPDINI ')
            GO TO 2
          ELSE
            CALL ERRMSG('FTRAKS','FPDINI','Failed to fetch from DBL3',
     &        'F')
            GO TO 999
          END IF
        ENDIF
        LFPCH = LDATA
        IF (LFPCH.EQ.0) THEN
          CALL ERRDB(' FPDINI - Error in finding requested data ')
          IOK = .FALSE.
          GO TO 999
        ENDIF
        LORUN(CRT) = IC(LKEY+3)      ! Start Validity
        HIRUN(CRT) = IC(LKEY+4)      ! End validity
C
        LINKH = LC(LFPDH-IZFPCH)
        IF (LINKH.NE.0) THEN
          CALL EZDROP('SCPH')
          CALL MZDROP(IXSTP,LINKH,' ')
        ENDIF
C
        CALL ZSHUNT(IXSTP,LFPCH,LFPDH,-IZFPCH,0)
        LFPCH = LC(LFPDH-IZFPCH)
        PEDRUN = IC(LFPCH+6)
C
        CALL CPATHG(SPTH)
        IF (OBUF(CRT).NE.PEDRUN) THEN
          OBUF(CRT) = PEDRUN
          LSRCP = 0
          WRITE(SRCPNAM,5)CRTID,SPTH(4:4)
    5     FORMAT('CRATE_',I3.3,A1)
          CALL EZLOC(SRCPNAM,LSRCP)
          IF (LSRCP.NE.0) CALL EZDROP(SRCPNAM)
          CALL EZNAME('SCPH',LFPCH,IZCRCP)
          CALL EZNAME(SRCPNAM,LFPCH,4)
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
            CALL FGTPDS(CHNL,VALUE,1)
            CALL NUM_TO_ADDR(CHNL,ID,1)
            IF (ID.GE.0) THEN
              CALL FCODER(ID,HA,UN,QU,SE,WI,UB,1)
              IF (UB.EQ.0) THEN
                LKFPSE=GZFPSE(HA,UN,QU,SE)
                IF (LKFPSE .NE. 0) THEN              
                  IF (IC(LKFPSE+1).NE.LORUN(CRT) 
     &              .OR.IC(LKFPSE+2).NE.HIRUN(CRT)) THEN  
                    IC(LKFPSE+1) = LORUN(CRT)   
                    IC(LKFPSE+2) = HIRUN(CRT)
                  ENDIF
                  NMPARW = IC(LKFPSE+4)
                  C(LKFPSE+NMPARW*WI+1+6)=VALUE(1)      ! PEDESTAL
                  C(LKFPSE+NMPARW*WI+2+6)=VALUE(2)      ! PED_SIGMA 
                ENDIF                                
              ENDIF                                  
            ENDIF
   10     CONTINUE
          CALL EZRSET
        ENDIF
C
C  *** Drop Electronic bank structure.
C
        LINKH = LC(LFPDH-IZFPCH)
        IF (LINKH.NE.0) THEN
          CALL EZDROP('SCPH')
          CALL MZDROP(IXSTP,LINKH,' ')
        ENDIF
    2 CONTINUE
C-------------------------------------------------------------------------
  999 RETURN
      END
