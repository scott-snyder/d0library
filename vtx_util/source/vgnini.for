      SUBROUTINE VGNINI(CRUN, MAX_VTXCRT, DB_OPENED, OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read VTX gains from database.
C-                         Called by VBDINI.
C-
C-   Inputs  : CRUN = Current run number
C-             MAX_VTXCRT = number of crates to read in (count fom 0)
C-             DB_OPENED = .TRUE. if DBCLB_INITIALIZE has already been called.
C-   Outputs : DB_OPENED = .TRUE. if DBCLB_INITIALIZE was called.
C-   Controls: OK = .TRUE. if all is well
C-
C-   Created   7-FEB-1991   Peter Grudberg
C-   Updated  13-AUG-1991   Susan K. Blessing   Add logical DB_OPENED to 
C-    flag if DBCLB_INITIALIZE has been called for this run.  
C-    When routine is undummied, look at FGNINI for example.
C-   Updated  19-OCT-1992   Peter M. Grudberg  Undummy routine 
C-   Updated  11-DEC-1992   Liang-ping Chen Add BYPASS_DBL3_ERROR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$LINKS:IZCRCP.LINK'
      INCLUDE 'D0$LINKS:IZVGCH.LINK'
C
      INTEGER CRUN, GNRUN, BNKNUM
      INTEGER CRT, CRTID, MAX_VTXCRT, OBUF(0:9)
      INTEGER CHNL, ID, IMIN, IMAX
      INTEGER LINKH,LSRCP
      INTEGER LVGCH
      INTEGER LVGNL, GZVGNL, LVGNZ, GZVGNZ, POINT
      INTEGER LAY, SEC, WIR, STR, END, TYPE, UB
      INTEGER FST_CARD, LST_CARD, ADCS_CRD
      INTEGER LORUN(0:9), HIRUN(0:9)
      INTEGER LENGTH,IER
      INTEGER LDATA,LKEY,ACCESSED
C
      REAL VALUE(5)
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
C ****  Loop over all crates.  Check if gains run has changed since last run
C ****  processed - if so, update!
C
      ACCESSED = 0
      IF (DB_OPENED) CALL DBCLB_PATH('GAINS', 'VTX', PATH)
      DO 2 CRT = 0, MAX_VTXCRT
C
        IF (CRUN.GE.LORUN(CRT).AND.CRUN.LT.HIRUN(CRT)) THEN
C This crate doesn't need updating.
          GO TO 2
        ELSE
          IF (.NOT.DB_OPENED) THEN
            CALL DBCLB_INITIALIZE(DBCALIB,'S',OK)
            IF (.NOT.OK) THEN
              CALL ERRDB(' VGNINI - Error in DBL3 intialization ')
              GO TO 999
            END IF
            CALL DBCLB_PATH('GAINS','VTX',PATH)
            DB_OPENED = .TRUE.
          END IF
        END IF
C
        ACCESSED = ACCESSED + 10**CRT
        CRTID = 10*CRT + BNKNUM
        LDATA = 0
        LKEY = 0
C Get Crate ped banks.
        CALL DBCLB_FETCH_OFFLINE(PATH,CRUN,CRTID,LDATA,LKEY)
        IF ( IQUEST(1) .NE. 0 ) THEN
          IF (BYPASS_DBL3_ERROR) THEN
            CALL ERRDB(' Error in VGNINI ')
            GO TO 999
          ELSE
            CALL ERRMSG('VTRAKS','VGNINI',
     &        'failed to fetch from DBL3','F')
            GOTO 999
          ENDIF
        ENDIF
        LVGCH = LDATA
        IF ( LVGCH .EQ. 0 ) THEN
          CALL ERRDB(' VGNINI - Error in finding requested data ')
          OK = .FALSE.
          GO TO 999
        ENDIF
        LORUN(CRT) = IC( LKEY + 3 )      ! Start Validity
        HIRUN(CRT) = IC( LKEY + 4 )      ! End validity
C
        LINKH = LC(LVGNH - IZVGCH)
        IF ( LINKH .NE. 0 ) THEN
          CALL EZDROP('SCPH')
          CALL MZDROP(IXSTP,LINKH,' ')
        ENDIF
C
        CALL ZSHUNT(IXSTP, LVGCH, LVGNH, -IZVGCH, 0)
        LVGCH = LC(LVGNH - IZVGCH)
        GNRUN = IC(LVGCH+6)
C
        CALL CPATHG(SPTH)
        IF ( OBUF(CRT) .NE. GNRUN ) THEN
          OBUF(CRT) = GNRUN
          LSRCP = 0
          WRITE(SRCPNAM,5) CRTID, SPTH(4:4)
    5     FORMAT('CRATE_',I3.3,A1)
          CALL EZLOC(SRCPNAM, LSRCP)
          IF ( LSRCP .NE. 0 ) CALL EZDROP(SRCPNAM)
          CALL EZNAME('SCPH', LVGCH, IZCRCP)
          CALL EZNAME(SRCPNAM, LVGCH, 4)
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
            CALL VGTGNS(CHNL, VALUE, 1)
            CALL NUM_TO_ADDR(CHNL, ID, 1)
            IF ( ID .GE. 0 ) THEN
              CALL VCODER(ID, TYPE, LAY, SEC, WIR, STR, END, UB, 1)
              IF ( UB .EQ. 0 .AND. TYPE .EQ. 0) THEN
                LVGNL = GZVGNL(LAY)
                IF ( IC(LVGNL+1) .NE. LORUN(CRT)
     &                .OR. IC(LVGNL+2) .NE. HIRUN(CRT) ) THEN
                  IC(LVGNL + 1) = LORUN(CRT)
                  IC(LVGNL + 2) = HIRUN(CRT)
                ENDIF
                POINT = LVGNL +
     &              (SEC*IC(LVGNL+4)+2*WIR+END)*IC(LVGNL+3) + 5 + 3*41
                IF ( VALUE(3) .GT. 0. ) THEN
                  C(POINT + 1) = VALUE(3)  ! Slope value
                ELSE
                  C(POINT + 1) = 1.
                ENDIF
              ENDIF
            ENDIF
          ENDDO
          CALL EZRSET
        ENDIF
C
C  *** Drop Electronic bank structure.
C
        LINKH = LC(LVGNH - IZVGCH)
        IF (LINKH.NE.0) THEN
          CALL EZDROP('SCPH')
          LINKH = LC(LVGNH - IZVGCH)
          CALL MZDROP(IXSTP,LINKH,' ')
        ENDIF
C
    2 CONTINUE
C
      IF ( ACCESSED .GT. 0 ) THEN
        WRITE(MSG,'(A,I10.10)') 
     &    ' VTX GAINS read from DBL3 online path, crate access = ', 
     &    ACCESSED
        CALL INTMSG(MSG)
      ENDIF
C
  999 RETURN
      END
