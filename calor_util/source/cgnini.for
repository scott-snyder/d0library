      SUBROUTINE CGNINI(CRUN,NCRT,CRATES,LOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialzation routine for Gains.
C-                         Called from CDBINI
C-   Inputs  : CRUN = Current Run Number
C-             NCRT = Number of ADC crates being read
C-             CRATES = Array containing the ADC crates to be read
C-                      If NCRT=12, then assumes all 12 D0 ADC crates
C-   Controls: LOK = .TRUE. if everything goes well.
C-
C-   Created  20-JUN-1990   Jan Guida (Adapted from CDBINI)
C-   Updated  13-NOV-1990   Jan Guida  Add CRATES variable, remove PRERUN
C-                                      add garbage collection
C-   Updated   3-DEC-1990   Jan Guida   Add naming of SRCP bank
C-   Updated  20-APR-1991   Jan Guida   Fix naming of SRCP bank
C-   Updated  22-APR-1991   Jan Guida   Modify zebra stuff for multiple crates 
C-   Updated  28-APR-1992   Jan Guida   Fill IC(LCGNH+11) with GAIN_FACTOR, if
C-                                        not there
C-   Updated  18-AUG-1992   Chip Stewart  DBCLB_FETCH_OFFLINE 
C-   Updated  28-AUG-1992   Jan Guida   Remove EZPICK and EZRSET 
C-                                      Drop previous SRCP bank
C-   Updated  15-APR-1993   Jan Guida  Add setting of LOK, if errors
C-   Updated  19-APR-1993   Meenakshi Narain  Add Bypass DBL3 error switch 
C-   Updated  22-JUN-1993   Jan Guida  Fill IC(LCGNH+4,5) with validity range 
C-   Updated  11-MAR-1994   Jan Guida  Fix bug: VSN should be real not integer 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$LINKS:IZCGNH.LINK'
      INCLUDE 'D0$LINKS:IZCRCP.LINK'
C
      INTEGER BNKNUM1,BNKNUM2
      INTEGER CRUN
      INTEGER CRT,CRTID,NCRT,CRATES(*)
      INTEGER LINKH,LAST,LZFIND,LZLAST,LSRCP
      INTEGER LORUN,HIRUN,LDAT,LKEY
      INTEGER IER
C
      REAL FACT,VSN
      CHARACTER*32 SRCPNAME
      CHARACTER*25 PATH
      LOGICAL LOK,FSTCRT
C
      PARAMETER (BNKNUM1 = 7)
      PARAMETER (BNKNUM2 = 8)
C
      LOGICAL BYPASS_DBL3_ERROR
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
C
C ****  READ THE DBL3 BYPASS ERROR SWITCH STATE FROM CAHITS_RCP
C
      IF(FIRST)THEN                     ! LOCAL INIT
        BYPASS_DBL3_ERROR = .FALSE.
        CALL EZPICK('CAHITS_RCP')
        CALL EZERR(IER)
        IF ( IER.EQ.0) THEN
          CALL EZGET_l('BYPASS_DBL3_ERROR',BYPASS_DBL3_ERROR,IER)
          IF(IER.NE.0) CALL ERRMSG('NO_ERR_SWITCH','READ_PEDGNS',
     &      'USE DBL3 BYPASS ERROR SWITCH  = FALSE AS DEFAULT','W')
        END IF
        CALL EZRSET
        FIRST = .FALSE.
      ENDIF
C
C   *** Loop over All crates. Check to see if pulser run has changed
C       since last run. If so, shunt to CGNH
C
      FSTCRT = .TRUE.
      CALL DBCLB_PATH('GAINS','CAL',PATH)
      DO 2 CRT = 0,NCRT-1
        IF (NCRT.EQ. 12) THEN
          IF(CRT.LT.6)THEN
            CRTID = 10*CRT + BNKNUM1
          ELSE
            CRTID = 10*(CRT-6) + BNKNUM2
          ENDIF
        ELSE
          CRTID = CRATES(CRT+1)
        ENDIF
        LDAT = 0
        LKEY = 0
        CALL DBCLB_FETCH_OFFLINE(PATH,CRUN,CRTID,LDAT,LKEY) ! Get GNS banks.
        IF (IQUEST(1).NE.0) THEN
          LOK = .FALSE.
          IF (BYPASS_DBL3_ERROR) THEN
            CALL ERRDB(' Error in CGNINI, finding path ')
            GO TO 2
          ELSE
            CALL ERRMSG('CALORIMETER','CGNINI',
     &        ' failed in finding DBL3 path','F')
            GOTO 999
          ENDIF
        ENDIF
        LINKH = LDAT
        IF(LINKH.EQ.0)THEN
          CALL ERRDB(' CGNINI - Failed to find pulser run in data base')
          IF (BYPASS_DBL3_ERROR) THEN
            CALL ERRDB
     &        (' CGNINI - Failed to find pulser run in data base')
          ELSE
            CALL ERRMSG('CALORIMETER','CGNINI',
     &        ' Failed to find pulser run in data base','F')
          ENDIF
          LOK = .FALSE.
          GOTO 999
        ENDIF
        LORUN = IC(LKEY+3)      ! Start Validity
        HIRUN = IC(LKEY+4)      ! End validity
C
        IF(FSTCRT)THEN
          FSTCRT=.FALSE.
          LCGNH=LC(LSCAL-IZCGNH)
          IF(LCGNH.GT.0) LCGNH = LZFIND(IDVSTP,LCGNH,CRTID,9)
          IF(LCGNH.NE.0)THEN
            CALL MZDROP(IXSTP,LCGNH,' ')
            CALL MZGARB(IXSTP,0)
          ENDIF
          CALL ZSHUNT(IXSTP,LINKH,LSCAL,-IZCGNH,0)
        ELSE
          LAST = LZFIND(IXSTP,LCGNH,CRTID,9)
          IF (LAST.GT.0) THEN
            CALL MZDROP(IXSTP,LAST,'L')
            CALL MZGARB(IXSTP,0)
          ENDIF
          LAST = LZLAST(IXSTP,LCGNH)
          CALL ZSHUNT(IXSTP,LINKH,LAST,0,0)
        ENDIF
        LCGNH=LC(LSCAL-IZCGNH)
        IC(LCGNH+4) = LORUN
        IC(LCGNH+5) = HIRUN
C
        WRITE(SRCPNAME,5)CRTID
    5   FORMAT('STPC/CALG',I2.2)
        CALL EZLOC(SRCPNAME,LSRCP)
        IF(LSRCP.NE.0) CALL EZDROP(SRCPNAME)  ! Drop RCP bank from previous run
        CALL EZNAME(SRCPNAME,LINKH,IZCRCP)
    2 CONTINUE
      CALL EZGET('CALIB_VERSION',VSN,IER)
      IF (VSN.GE.3.0 .AND. LCGNH+11.EQ.0) THEN
        CALL EZGET('GAIN_FACTOR',FACT,IER)
        IC(LCGNH+11)=FACT
      ENDIF
C
  999 RETURN
      END
