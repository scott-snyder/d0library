      SUBROUTINE FGNINI(CRUN,MAX_FDCRT,DB_OPENED,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Retrieve Gains from DBL3 and store in Logical STP
C-   tree structure.
C-
C-   Inputs  : CRUN = Run Number
C-             MAX_FDCRT = Maximum number of FDC crates being analyzed.
C-   Outputs : none
C-   Controls: IOK = Logical Error flag.
C-
C-   Created  18-MAR-1991   Robert E. Avery   Based on Srini's fpdini.
C-   Updated  13-AUG-1991   Susan K. Blessing   Add logical DB_OPENED to 
C-    flag if DBCLB_INITIALIZE has been called for this run.  If updating
C-    is necessary for CRUN, then call DBCLB_INITIALIZE if it hasn't already
C-    been called.  Make LORUN and HIRUN arrays and check if CRUN is within
C-    the validity range without looking in the database.
C-   Updated  12-MAY-1992   Susan K. Blessing  Remove FDLOGI.INC. 
C-   Updated   1-JUN-1992   Susan K. Blessing  Change call to DBCLB_FETCH
C-    to DBCLB_FETCH_OFFLINE.  Remove D0$INC:DBSTP.INC.  DBCLB_FETCH_OFFLINE
C-    returns LDATA and LKEY for CRTID.  Remove UZERO call for OBUF.
C-   Updated  21-SEP-1992   Susan K. Blessing  Add message about reading
C-    gains for first time through.
C-   Updated  11-DEC-1992   Susan K. Blessing  Add BYPASS_DBL3_ERROR
C-    switch.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$LINKS:IZFGCH.LINK'
      INCLUDE 'D0$LINKS:IZCRCP.LINK'
C
      INTEGER CRUN,PEDRUN,BNKNUM
      INTEGER CRT,CRTID,MAX_FDCRT,OBUF(0:11)
      INTEGER CHNL,ID,IMIN,IMAX
      INTEGER LKFGSE,LFGCH,LINKH,LSRCP
      INTEGER FST_CARD,LST_CARD,ADCS_CRD
      INTEGER GZFGSE
      INTEGER LORUN(0:11),HIRUN(0:11)
      INTEGER NMPARW 
      INTEGER LENGTH,IER
      INTEGER LDATA,LKEY
      INTEGER HA, UN, QU, SE, WI, UB
C
      REAL VALUE(5)
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
      PARAMETER (BNKNUM = 5)
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
        CALL INTMSG(' Reading FDC gains.')
      ENDIF
C
C   *** Loop over All crates. Check to see if pedestal Run has changed
C       since last run. If so, update Logical tree structure.
C
      IF (DB_OPENED) CALL DBCLB_PATH('GAINS','FDC',PATH)
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
            CALL DBCLB_PATH('GAINS','FDC',PATH)
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
            CALL ERRDB(' Error in FGNINI ')
            GO TO 2
          ELSE
            CALL ERRMSG('FTRAKS','FGNINI','Failed to fetch from DBL3',
     &        'F')
            GO TO 999
          END IF
        ENDIF
        LFGCH = LDATA
        IF (LFGCH.EQ.0) THEN
          CALL ERRDB(' FGNINI - Error in finding requested data ')
          IOK = .FALSE.
          GO TO 999
        ENDIF
        LORUN(CRT) = IC(LKEY+3)      ! Start Validity
        HIRUN(CRT) = IC(LKEY+4)      ! End validity
C
        LINKH = LC(LFGNH-IZFGCH)
        IF (LINKH.NE.0) THEN
          CALL EZDROP('SCPH')
          CALL MZDROP(IXSTP,LINKH,' ')
        ENDIF
C
        CALL ZSHUNT(IXSTP,LFGCH,LFGNH,-IZFGCH,0)
        LFGCH = LC(LFGNH-IZFGCH)
        PEDRUN = IC(LFGCH+6)
C
        CALL CPATHG(SPTH)
        IF (OBUF(CRT).NE.PEDRUN) THEN
          OBUF(CRT) = PEDRUN
          LSRCP = 0
          WRITE(SRCPNAM,5)CRTID,SPTH(4:4)
    5     FORMAT('CRATE_',I3.3,A1)
          CALL EZLOC(SRCPNAM,LSRCP)
          IF (LSRCP.NE.0) CALL EZDROP(SRCPNAM)
          CALL EZNAME('SCPH',LFGCH,IZCRCP)
          CALL EZNAME(SRCPNAM,LFGCH,4)
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
            CALL FGTGNS(CHNL,VALUE,1)
            CALL NUM_TO_ADDR(CHNL,ID,1)
            IF (ID.GE.0) THEN
              CALL FCODER(ID,HA,UN,QU,SE,WI,UB,1)
              IF (UB.EQ.0) THEN
                IF (LKFGSE .NE. 0) THEN              
                LKFGSE=GZFGSE(HA,UN,QU,SE)
                  IF (IC(LKFGSE+1).NE.LORUN(CRT)
     &              .OR.IC(LKFGSE+2).NE.HIRUN(CRT)) THEN  
                    IC(LKFGSE+1) = LORUN(CRT)   
                    IC(LKFGSE+2) = HIRUN(CRT)        
                  ENDIF                              
                  NMPARW = IC(LKFGSE+4)
                  C(LKFGSE+NMPARW*WI+1+6)=VALUE(3)      ! slope parameter
                ENDIF                                
              ENDIF                                  
            ENDIF
   10     CONTINUE
          CALL EZRSET
        ENDIF
C
C  *** Drop Electronic bank structure.
C
        LINKH = LC(LFGNH-IZFGCH)
        IF (LINKH.NE.0) THEN
          CALL EZDROP('SCPH')
          CALL MZDROP(IXSTP,LINKH,' ')
        ENDIF
    2 CONTINUE
C--------------------------------------------------------------------------
  999 RETURN
      END
