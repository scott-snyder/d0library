      SUBROUTINE FTMINI(CRUN,MAX_FDCRT,DB_OPENED,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Retrieve Times from DBL3 and store in Logical STP
C-   tree structure.
C-
C-   Inputs  : CRUN = Run Number
C-             MAX_FDCRT = Maximum number of FDC crates being analyzed.
C-   Outputs : none
C-   Controls: IOK = Logical Error flag.
C-
C-   Created  17-AUG-1990   Srini Rajagopalan
C-   Updated  22-FEB-1991   Robert E. Avery  Check existance of FTSE bank.
C-   Updated  20-MAR-1991   Jeffrey Bantly  change call to FGTTMS params
C-   Updated  13-JUN-1991   Robert E. Avery  Insure that only good channels
C-                              are used, store relative values (ave = ~0).
C-   Updated  21-JUN-1991   Robert E. Avery  Only read in first crate for 
C-                              NWA data. 
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
C-    tzeros for first time through.
C-   Updated   8-DEC-1992   Robert E. Avery   Subtract average crate-by-
C-    crate, since CALIB's crate-to-crate corrections are wrong.
C-   Updated  11-DEC-1992   Susan K. Blessing  Add BYPASS_DBL3_ERROR
C-    switch.
C-   Updated  28-DEC-1992   Robert E. Avery  Fix bug in 8-DEC update.
C-   Updated  10-JUN-1993   Robert E. Avery  Make error for no good
C-    t0's in crate Fatal (but BYPASSable).
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$LINKS:IZFTCH.LINK'
      INCLUDE 'D0$LINKS:IZCRCP.LINK'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER HALF,UNIT,QUAD,SECT,WIRE
      INTEGER MAXWIR 
      INTEGER MAXSEC 
      INTEGER MAXQUAD 
      INTEGER CRUN,PEDRUN,BNKNUM
      INTEGER CRT,CRTID,MAX_FDCRT,OBUF(0:11)
      INTEGER CHNL,ID,IMIN,IMAX
      INTEGER LKFTSE,LFTCH,LINKH,LSRCP
      INTEGER FST_CARD,LST_CARD,ADCS_CRD
      INTEGER GZFTSE
      INTEGER LORUN(0:11),HIRUN(0:11)
      INTEGER NMPARW
      INTEGER NUM_TOTAL 
      INTEGER INTEGER 
      INTEGER IPTR,IER
      INTEGER NUM_GOOD 
      INTEGER RUNTYPE
      INTEGER MAX_CRATE
      INTEGER LENGTH
      INTEGER LOUT ,USUNIT
      INTEGER LDATA,LKEY
      INTEGER HA, UN, QU, SE, WI, UB
C
      REAL VALUE(2)
      REAL AVE_VALUE(0:11)
      REAL SUM_GOOD 
      REAL ETZERO 
      REAL ETZERO_SIGN
      REAL ETZERO_CUT
      PARAMETER( ETZERO_CUT =  100. )
C
      CHARACTER*25 PATH
      CHARACTER*10 SRCPNAM
      CHARACTER*4 SPTH
      CHARACTER*80 DBCALIB
C
      LOGICAL IOK,FIRST,FIRST_GOOD
      LOGICAL DB_OPENED
      LOGICAL DBG_FTMINI
      LOGICAL BYPASS_DBL3_ERROR
      LOGICAL DB_READ 
C
      PARAMETER (BNKNUM = 5)
C
      SAVE FIRST,RUNTYPE,LORUN,HIRUN,DBCALIB
      SAVE ETZERO_SIGN
      SAVE DBG_FTMINI
C
      DATA FIRST /.TRUE./
C
C                                       ! RCP file
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
        CALL EZGET('RUNTYPE',RUNTYPE,IER)
        CALL EZGETS('DBCALIB$FDC',1,DBCALIB,LENGTH,IER)
        CALL EZGET('DBG_FTMINI',DBG_FTMINI,IER)
        CALL EZGET('BYPASS_DBL3_ERROR',BYPASS_DBL3_ERROR,IER)
        CALL EZRSET
        IF (RUNTYPE.LT.3) THEN
          ETZERO_SIGN = -1.
        ELSE
          ETZERO_SIGN = 1.
        ENDIF
        CALL INTMSG(' Reading FDC Tzeros.')
        FIRST = .FALSE.
      ENDIF
C
      IF (RUNTYPE.EQ.2) THEN            ! Only crate 0 tzero exists for NWA
        MAX_CRATE = 0
      ELSE
        MAX_CRATE = MAX_FDCRT
      ENDIF
      DB_READ = .FALSE.
C
C   *** Loop over All crates. Check to see if pedestal Run has changed
C       since last run. If so, update Logical tree structure.
C
      IF (DB_OPENED) CALL DBCLB_PATH('TIMES','FDC',PATH)
      DO 2 CRT = 0,MAX_CRATE
        FIRST_GOOD = .TRUE.
        NUM_GOOD = 0
        NUM_TOTAL = 0
        AVE_VALUE(CRT) = 0.0
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
            CALL DBCLB_PATH('TIMES','FDC',PATH)
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
            CALL ERRDB(' Error in FTMINI ')
            GO TO 2
          ELSE
            CALL ERRMSG('FTRAKS','FTMINI','Failed to fetch from DBL3',
     &        'F')
            GO TO 999
          END IF
        ENDIF
        LFTCH = LDATA
        IF (LFTCH.EQ.0) THEN
          CALL ERRDB(' FTMINI - Error in finding requested data ')
          IOK = .FALSE.
          GO TO 999
        ENDIF
        LORUN(CRT) = IC(LKEY+3)      ! Start Validity
        HIRUN(CRT) = IC(LKEY+4)      ! End validity
C
        LINKH = LC(LFTMH-IZFTCH)
        IF (LINKH.NE.0) THEN
          CALL EZDROP('SCPH')
          CALL MZDROP(IXSTP,LINKH,' ')
        ENDIF
C
        CALL ZSHUNT(IXSTP,LFTCH,LFTMH,-IZFTCH,0)
        LFTCH = LC(LFTMH-IZFTCH)
        PEDRUN = IC(LFTCH+6)
C
        CALL CPATHG(SPTH)
        IF (OBUF(CRT).NE.PEDRUN) THEN
          DB_READ = .TRUE.
          OBUF(CRT) = PEDRUN
          LSRCP = 0
          WRITE(SRCPNAM,5)CRTID,SPTH(4:4)
    5     FORMAT('CRATE_',I3.3,A1)
          CALL EZLOC(SRCPNAM,LSRCP)
          IF (LSRCP.NE.0) CALL EZDROP(SRCPNAM)
          CALL EZNAME('SCPH',LFTCH,IZCRCP)
          CALL EZNAME(SRCPNAM,LFTCH,4)
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
            CALL FGTTMS(CHNL,VALUE,1)
            CALL NUM_TO_ADDR(CHNL,ID,1)
            IF (ID.GE.0) THEN
              CALL FCODER(ID,HA,UN,QU,SE,WI,UB,1)
              IF (UB.EQ.0) THEN
                LKFTSE=GZFTSE(HA,UN,QU,SE)
                IF ( LKFTSE .NE. 0) THEN
                  IF ( IC(LKFTSE+1).NE.LORUN(CRT) .OR.
     &                 IC(LKFTSE+2).NE.HIRUN(CRT) ) THEN
                    IC(LKFTSE+1) = LORUN(CRT)
                    IC(LKFTSE+2) = HIRUN(CRT)
                  ENDIF
                  NMPARW = IC(LKFTSE+4)
                  IF (  VALUE(2) .GT. 0 ) THEN
                    VALUE(1) = VALUE(1) + 2*ETZERO_CUT  ! temporary offset
                    C(LKFTSE+ NMPARW*WI+1+6)= VALUE(1) 
                    IF ( FIRST_GOOD ) THEN
                      FIRST_GOOD = .FALSE.
                      NUM_GOOD = 1
                      SUM_GOOD = VALUE(1)
                      AVE_VALUE(CRT) = VALUE(1)
                    ELSEIF ( ABS(VALUE(1) - AVE_VALUE(CRT) ) 
     &                      .LT. ETZERO_CUT ) THEN
                      NUM_GOOD = NUM_GOOD+1
                      SUM_GOOD = SUM_GOOD + VALUE(1)
                      AVE_VALUE(CRT) = SUM_GOOD/NUM_GOOD
                    ELSE
                      C(LKFTSE+ NMPARW*WI+1+6)= 9999.
                    ENDIF
                  ELSE
                    C(LKFTSE+ NMPARW*WI+1+6)= 9999.       ! bad ETZERO
                  ENDIF
                  NUM_TOTAL = NUM_TOTAL + 1
                ENDIF
              ENDIF
            ENDIF
   10     CONTINUE
          CALL EZRSET
        ENDIF
C
C  *** Drop Electronic bank structure.
C
        LINKH = LC(LFTMH-IZFTCH)
        IF (LINKH.NE.0) THEN
          CALL EZDROP('SCPH')
          CALL MZDROP(IXSTP,LINKH,' ')
        ENDIF
C
C  Give warning if too many bad channels
C
        IF ( NUM_GOOD .LT. 0.5 * (NUM_TOTAL+1) ) THEN
          IF (BYPASS_DBL3_ERROR) THEN
            CALL ERRMSG('FDC_BAD_TZEROS',
     &                  'FTMINI',
     &                  'Bad t0s for a whole FDC crate','I')
            AVE_VALUE(CRT) = -1.
            GO TO 2
          ELSE
            CALL ERRMSG('FDC_BAD_TZEROS',
     &                  'FTMINI',
     &                  'Bad t0s for a whole FDC crate', 'F')
            GO TO 999
          END IF
        ENDIF
        IF ( DBG_FTMINI ) THEN
          LOUT = USUNIT()
          WRITE(LOUT,*) ' '
          WRITE(LOUT,*) ' Crate:',CRTID
          WRITE(LOUT,*) '   Number good:',NUM_GOOD
          WRITE(LOUT,*) '   Number total:',NUM_TOTAL
          WRITE(LOUT,*) '   Ave t0:',AVE_VALUE(CRT)
        ENDIF
    2 CONTINUE                          ! End CRT loop
C
      IF ( .NOT.DB_READ ) THEN
        GOTO 999                        ! No new tzeros for this run.
      ENDIF
C
C  Readjust to reference time and check for bad channels:
C
      DO  HALF =  0, MXHALF
        DO UNIT =  0, MXUNIT
          IF ( UNIT.EQ.0 ) THEN
            MAXSEC = MXSECT
            MAXWIR = MXWIRT + 2
            MAXQUAD = MXQUAD
          ELSE
            MAXSEC = MXSECP
            MAXWIR = MXWIRP
            MAXQUAD = 0
          ENDIF
          DO  QUAD=  0, MAXQUAD 
            DO  SECT=  0, MAXSEC 
              LKFTSE=GZFTSE(HALF,UNIT,QUAD,SECT)
              NMPARW = IC(LKFTSE+4)
              CALL FSECT_CRATE(HALF,UNIT,QUAD,SECT,CRTID)
              CRT = CRTID/10
              IF ( AVE_VALUE(CRT) .NE. 0.0 ) THEN
                DO WIRE =  0, MAXWIR
                  IPTR = LKFTSE + WIRE*NMPARW   + 6
                  ETZERO = C(IPTR + 1) 
C
C Readjust only newly read tzeros:
C
                  IF ( ABS(ETZERO) .GT. ETZERO_CUT ) THEN
                    ETZERO = ETZERO - AVE_VALUE(CRT) 
                    IF ( ABS(ETZERO) .LT. ETZERO_CUT ) THEN
                      C(IPTR + 1) = ETZERO_SIGN * ETZERO 
                    ELSE
                      C(IPTR + 1) = 0     ! bad etzero
                    END IF 
                  ENDIF
                ENDDO
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C--------------------------------------------------------------------------
  999 RETURN
      END
