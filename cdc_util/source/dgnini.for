      SUBROUTINE DGNINI(CRUN,MAX_CDCRT,DB_OPENED,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Retrieve Gains from DBL3 and store in 
C-                         Logical STP tree structure.
C-
C-   Inputs  : CRUN = Run Number
C-             MAX_CDCRT = Maximum number of CDC crates being analyzed.
C-             DB_OPENED = .TRUE. if DBCLB_INITIALIZE has already been called.
C-   Outputs : DB_OPENED = .TRUE. if DBCLB_INITIALIZE was called.
C-             IOK = Logical Error flag.
C-
C-   Created  13-DEC-1990   Qizhong Li-Demarteau   
C-   Updated   9-JUL-1991   Qizhong Li-Demarteau  based on FGNINI & DPDINI
C-   Updated  13-AUG-1991   Susan K. Blessing   Add logical DB_OPENED to 
C-    flag if DBCLB_INITIALIZE has been called for this run.  If updating
C-    is necessary for CRUN, then call DBCLB_INITIALIZE if it hasn't already
C-    been called.  Make LORUN and HIRUN arrays and check if CRUN is within
C-    the validity range without looking in the database.
C-   Updated  21-APR-1992   Qizhong Li-Demarteau  changed to call  
C-                                                DBCLB_FETCH_OFFLINE
C-   Updated   1-JUN-1992   Qizhong Li-Demarteau  change UZERO to VZERO 
C-   Updated  11-DEC-1992   Qizhong Li-Demarteau  handling DBL3 errors 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$LINKS:IZDGCH.LINK'
      INCLUDE 'D0$LINKS:IZCRCP.LINK'
C
      INTEGER CRUN, MAX_CDCRT
      INTEGER PEDRUN, BNKNUM, CRT, CRTID, OBUF(0:5)
      INTEGER CHNL, ID, IMIN, IMAX, UB
      INTEGER LKDGNL, LDGCH, LINKH, LSRCP
      INTEGER FST_CARD,LST_CARD,ADCS_CRD
      INTEGER GZDGNL
      INTEGER LAYER, SECTOR, WIRE, INDEX
      INTEGER LORUN(0:5),HIRUN(0:5)
      INTEGER LENGTH,IER
      INTEGER LDAT, LKEY
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
      LOGICAL EZERROR, BYPASS_DBL3_ERROR
C
      PARAMETER (BNKNUM = 4)
      SAVE FIRST,LORUN,HIRUN,DBCALIB
      DATA FIRST /.TRUE./
      DATA LORUN/6*-1/
      DATA HIRUN/6*-1/
C----------------------------------------------------------------------
C
      IOK = .TRUE.
      IF (FIRST) THEN
        CALL VZERO(OBUF(0),6)
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','DGNINI',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGETS('DBCALIB$CDC',1,DBCALIB,LENGTH,IER)
        CALL EZGET('BYPASS_DBL3_ERROR',BYPASS_DBL3_ERROR,IER)
        CALL EZRSET
        FIRST = .FALSE.
      ENDIF
C
C   *** Loop over All crates. Check to see if gain Run has changed
C       since last run. If so, update Logical tree structure.
C
      IF (DB_OPENED) CALL DBCLB_PATH('GAINS','CDC',PATH)
      DO 2 CRT = 0,MAX_CDCRT
C
        IF (CRUN.GE.LORUN(CRT).AND.CRUN.LT.HIRUN(CRT)) THEN
C This crate doesn't need updating.
          GO TO 2
        ELSE
          IF (.NOT.DB_OPENED) THEN
            CALL DBCLB_INITIALIZE(DBCALIB,'S',IOK)
            IF (.NOT.IOK) THEN
              CALL ERRDB(' DDBINI - Error in DBL3 intialization ')
              GO TO 999
            END IF
            CALL DBCLB_PATH('GAINS','CDC',PATH)
            DB_OPENED = .TRUE.
          END IF
        END IF
C
        CRTID = 10*CRT + BNKNUM
        LDAT = 0
        LKEY = 0
        CALL DBCLB_FETCH_OFFLINE(PATH,CRUN,CRTID,LDAT,LKEY)
        IF (IQUEST(1).NE.0) THEN
          IF (BYPASS_DBL3_ERROR) THEN
            CALL ERRDB(' Error in DGNINI ')
            GOTO 2
          ELSE
            CALL ERRMSG('DTRAKS','DGNINI',
     &        'failed to fetch from DBL3','F')
            GOTO 999
          ENDIF
        ENDIF
        LDGCH = LDAT
        IF (LDGCH .LE. 0 .OR. LKEY .LE. 0) THEN
          CALL ERRDB(' DGNINI - Error in finding requested data ')
          IOK = .FALSE.
          GO TO 999
        ENDIF
        LORUN(CRT) = IC(LKEY+3)      ! Start Validity
        HIRUN(CRT) = IC(LKEY+4)      ! End validity
C
        LINKH = LC(LDGNH-IZDGCH)
        IF (LINKH.NE.0) THEN
          CALL EZDROP('SCPH')
          CALL MZDROP(IXSTP,LINKH,' ')
        ENDIF
C
        CALL ZSHUNT(IXSTP,LDGCH,LDGNH,-IZDGCH,0)
        LDGCH = LC(LDGNH-IZDGCH)
        PEDRUN = IC(LDGCH+6)
C
        CALL CPATHG(SPTH)
        IF (OBUF(CRT).NE.PEDRUN) THEN
          OBUF(CRT) = PEDRUN
          LSRCP = 0
          WRITE(SRCPNAM,5)CRTID,SPTH(4:4)
    5     FORMAT('CRATE_',I3.3,A1)
          CALL EZLOC(SRCPNAM,LSRCP)
          IF (LSRCP.NE.0) CALL EZDROP(SRCPNAM)
          CALL EZNAME('SCPH',LDGCH,IZCRCP)
          CALL EZNAME(SRCPNAM,LDGCH,4)
          CALL EZPICK('SCPH')
          IF ( EZERROR(IER) ) THEN
            CALL ERRMSG('DTRAKS','DGNINI',
     &        'Unable to find bank SCPH','W')
            GOTO 999
          ENDIF
          CALL EZGSET('FST_CARD',FST_CARD,1)
          CALL EZGSET('LST_CARD',LST_CARD,1)
          CALL EZGSET('ADCS_CRD',ADCS_CRD,1)
          IF (IER .NE. 0) ADCS_CRD = 16         
          IMIN = (FST_CARD-1)*ADCS_CRD
          IMAX = LST_CARD*ADCS_CRD-1
          CALL RS_NUM_TO_ADDR           ! Initialize Num_to_addr
          CALL EZRSET
C
C   *** Fill Logical tree structure
C
          DO 10 CHNL = IMIN,IMAX
            CALL DGTGNS(CHNL,VALUE,1)
            CALL NUM_TO_ADDR(CHNL,ID,1)
            IF (ID.GE.0) THEN
              CALL DCODER(ID,LAYER,SECTOR,WIRE,UB,1)
              IF (UB.EQ.0) THEN
                LKDGNL = GZDGNL(LAYER)
                IF (IC(LKDGNL+1).NE.LORUN(CRT).OR.
     &            IC(LKDGNL+2).NE.HIRUN(CRT)) THEN
                  IC(LKDGNL+1) = LORUN(CRT)
                  IC(LKDGNL+2) = HIRUN(CRT)
                ENDIF
                INDEX = LKDGNL + (SECTOR * IC(LKDGNL + 4) + WIRE) *
     &                  IC(LKDGNL + 3) + 4
                C(INDEX + 1) = VALUE(3)         ! slope parameter
              ENDIF                                  
            ENDIF
   10     CONTINUE
        ENDIF
C
C  *** Drop Electronic bank structure.
C
        LINKH = LC(LDGNH-IZDGCH)
        IF (LINKH.NE.0) THEN
          CALL EZDROP('SCPH')
          CALL MZDROP(IXSTP,LINKH,' ')
          CALL MZGARB(IXSTP,0)
        ENDIF
    2 CONTINUE
C
  999 RETURN
      END
