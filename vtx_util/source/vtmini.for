      SUBROUTINE VTMINI(CRUN, MAX_VTXCRT, DB_OPENED, OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read VTX time constants from database.
C-                         Called by VBDINI.
C-
C-   Inputs  : CRUN = Current run number
C-             MAX_VTXCRT = number of crates to read in (count fom 0)
C-             DB_OPENED = .TRUE. if DBCLB_INITIALIZE has already been called.
C-   Outputs : DB_OPENED = .TRUE. if DBCLB_INITIALIZE was called.
C-   Controls: OK = .TRUE. if all is well
C-
C-   Created   7-FEB-1991   Peter Grudberg
C-   Updated   8-APR-1991   Peter Grudberg  Force garbage collection
C-   Updated  13-AUG-1991   Susan K. Blessing   Add logical DB_OPENED to
C-    flag if DBCLB_INITIALIZE has been called for this run.  If updating
C-    is necessary for CRUN, then call DBCLB_INITIALIZE if it hasn't already
C-    been called.  Make LORUN and HIRUN arrays and check if CRUN is within
C-    the validity range without looking in the database.
C-   Updated  11-OCT-1991   Peter M. Grudberg  Fix pointer into VTMW
C-   Updated   8-APR-1992   Peter M. Grudberg  Remove pads, avoid problems with
C-                                             T0 detector
C-   Updated   1-JUN-1992   Susan K. Blessing  Change call to DBCLB_FETCH
C-    to DBCLB_FETCH_OFFLINE.  Remove D0$INC:DBSTP.INC.  DBCLB_FETCH_OFFLINE
C-    returns LDATA and LKEY for CRTID.  Remove UZERO call for OBUF.
C-   Updated  19-OCT-1992   Peter M. Grudberg  Remove MZGARB call 
C-   updated   7-Dec-1992    Liang-ping Chen  match EZRSET with EZPICK
C-   updated  11-Dec-1992    Liang-ping Chen  Add BYPASS_DBL3_ERROR
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$LINKS:IZVTCH.LINK'
      INCLUDE 'D0$LINKS:IZCRCP.LINK'
C
      INTEGER CRUN, TIMRUN, BNKNUM
      INTEGER CRT, CRTID, MAX_VTXCRT, OBUF(0:9)
      INTEGER CHNL, ID, IMIN, IMAX
      INTEGER LVTCH, LINKH, LSRCP
      INTEGER LVTMW, LVTMZ, GZVTMW, GZVTMZ
      INTEGER LAY, SEC, WIR, STR, END, TYPE, UB, POINT
      INTEGER FST_CARD, LST_CARD, ADCS_CRD
      INTEGER LORUN(0:9), HIRUN(0:9)
      INTEGER LENGTH,IER
      INTEGER LDATA,LKEY,ACCESSED
C
      REAL VALUE(2)
C
      CHARACTER*25 PATH
      CHARACTER*10 SRCPNAM
      CHARACTER*4 SPTH
      CHARACTER*80 DBCALIB,MSG
C
      LOGICAL OK, FIRST
      LOGICAL DB_OPENED
      LOGICAL BYPASS_DBL3_ERROR
C
      PARAMETER ( BNKNUM = 3 )
      SAVE FIRST,LORUN,HIRUN,DBCALIB
      DATA FIRST /.TRUE./
      DATA LORUN/10*-1/
      DATA HIRUN/10*-1/
      DATA OBUF/10*0/
C
C----------------------------------------------------------------------
C
      OK = .TRUE.
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGETS('DBCALIB$VTX',1,DBCALIB,LENGTH,IER)
        CALL EZGET('BYPASS_DBL3_ERROR',BYPASS_DBL3_ERROR,IER)
        CALL EZRSET
      ENDIF
C
C ****  Loop over all crates.  Check if time run has changed since last run
C ****  processed - if so, update!
C
      ACCESSED = 0
      IF (DB_OPENED) CALL DBCLB_PATH('TIMES', 'VTX', PATH)
      DO 2 CRT = 0, MAX_VTXCRT
C
        IF (CRUN.GE.LORUN(CRT).AND.CRUN.LT.HIRUN(CRT)) THEN
C This crate doesn't need updating.
          GO TO 2
        ELSE
          IF (.NOT.DB_OPENED) THEN
            CALL DBCLB_INITIALIZE(DBCALIB,'S',OK)
            IF (.NOT.OK) THEN
              CALL ERRDB(' VDBINI - Error in DBL3 intialization ')
              GO TO 999
            END IF
            CALL DBCLB_PATH('TIMES','VTX',PATH)
            DB_OPENED = .TRUE.
          END IF
        END IF
C
        ACCESSED = ACCESSED + 10**CRT
        CRTID = 10*CRT + BNKNUM
        LDATA = 0
        LKEY = 0
C Get Crate time banks.
        CALL DBCLB_FETCH_OFFLINE(PATH,CRUN,CRTID,LDATA,LKEY)
        IF ( IQUEST(1) .NE. 0 ) THEN
          IF (BYPASS_DBL3_ERROR) THEN
            CALL ERRDB(' Error in VTMINI ')
            GO TO 999
          ELSE
            CALL ERRMSG('VTRAKS','VTMINI',
     &        'failed to fetch from DBL3','F')
            GOTO 999
          ENDIF
        ENDIF
        LVTCH = LDATA
        IF ( LVTCH .EQ. 0 ) THEN
          CALL ERRDB(' VTMINI - Error in finding requested data ')
          OK = .FALSE.
          GO TO 999
        ENDIF
        LORUN(CRT) = IC( LKEY + 3 )      ! Start Validity
        HIRUN(CRT) = IC( LKEY + 4 )      ! End validity
C
        LINKH = LC(LVTMH - IZVTCH)
        IF ( LINKH .NE. 0 ) THEN
          CALL EZDROP('SCPH')
          CALL MZDROP(IXSTP,LINKH,' ')
        ENDIF
C
        CALL ZSHUNT(IXSTP, LVTCH, LVTMH, -IZVTCH, 0)
        LVTCH = LC(LVTMH - IZVTCH)
        TIMRUN = IC(LVTCH+6)
C
        CALL CPATHG(SPTH)
        IF ( OBUF(CRT) .NE. TIMRUN ) THEN
          OBUF(CRT) = TIMRUN
          LSRCP = 0
          WRITE(SRCPNAM,5) CRTID, SPTH(4:4)
    5     FORMAT('CRATE_',I3.3,A1)
          CALL EZLOC(SRCPNAM, LSRCP)
          IF ( LSRCP .NE. 0 ) CALL EZDROP(SRCPNAM)
          CALL EZNAME('SCPH', LVTCH, IZCRCP)
          CALL EZNAME(SRCPNAM, LVTCH, 4)
          CALL EZPICK('SCPH')
          CALL EZGSET('FST_CARD', FST_CARD, 1)
          CALL EZGSET('LST_CARD', LST_CARD, 1)
          CALL EZGSET('ADCS_CRD', ADCS_CRD, 1)
          IMIN = (FST_CARD - 1) * ADCS_CRD
          IMAX = LST_CARD * ADCS_CRD - 1
          CALL RS_NUM_TO_ADDR           ! Initialize Num_to_addr
C
C   *** Fill Logical tree structure
C
          DO CHNL = IMIN, IMAX
            CALL VGTTMS(CHNL, VALUE, 1)
            CALL NUM_TO_ADDR(CHNL, ID, 1)
            IF ( ID .GE. 0 ) THEN
              CALL VCODER(ID, TYPE, LAY, SEC, WIR, STR, END, UB, 1)
              IF ( UB .EQ. 0 .AND. TYPE .EQ. 0 ) THEN
                LVTMW = GZVTMW(LAY)
                IF ( IC(LVTMW+1) .NE. LORUN(CRT)
     &                .OR. IC(LVTMW+2) .NE. HIRUN(CRT) ) THEN
                  IC(LVTMW + 1) = LORUN(CRT)
                  IC(LVTMW + 2) = HIRUN(CRT)
                ENDIF
                POINT = LVTMW + (SEC*IC(LVTMW+4)+WIR)*IC(LVTMW+3)
     &              + 2*END + 5
                C(POINT + 1)=VALUE(1)
                C(POINT + 2)=VALUE(2)
              ENDIF
            ENDIF
          ENDDO
          CALL EZRSET
        ENDIF
C
C  *** Drop Electronic bank structure.
C
        LINKH = LC(LVTMH - IZVTCH)
        IF (LINKH.NE.0) THEN
          CALL EZDROP('SCPH')
          CALL MZDROP(IXSTP,LINKH,' ')
        ENDIF
    2 CONTINUE
C
      IF ( ACCESSED .GT. 0 ) THEN
        WRITE(MSG,'(A,I10.10)') 
     &    ' VTX TIMES read from DBL3 online path, crate access = ', 
     &    ACCESSED
        CALL INTMSG(MSG)
      ENDIF
C
  999 RETURN
      END
