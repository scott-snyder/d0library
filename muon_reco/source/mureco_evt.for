      LOGICAL FUNCTION MURECO_EVT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Steering routine for muon reconstruction
C-                         package, MURECO in event phase.
C-
C-   Returned value  :
C-   Inputs  : (none)
C-   Outputs : (none)
C-   Controls: (none)
C-
C-   Created   8-OCT-1989   Shuichi Kunori
C-   Modified
C-   17-MAY-1990   S.Kunori
C-       1) change to CALL MUANLZ from function call.
C-       2) return status is always .TRUE.
C-   12-JUN-1990   S.Kunori
C-       1) add MULINK,MUFITS,MUPMUO
C-       2) add ERRMSG
C-   Modified  10-OCT-1991   Shahriar Abachi    Switch for MUFITS added
C-   Modified  17-OCT-1991   Shahriar Abachi    Completely restructured to
C-                                              accomodate new precise fit and
C-                                              global fit.
C     DH 11/91 change MUANLZ call
C     DH 1/92 only MUANLZ errors < 0 are fatel
C     DH 3/92 slight modification to MUANLZ ERRMSG
C-   Updated  19-SEP-1992   Daria Zieminska   call MUGLOBAL
C-   Updated  12-OCT-1993   Daria Zieminska   check if Main Ring event
C-   Updated  15-NOV-1993   Darien Wood  call COMPUTE_MU_QUALITY
C-   Updated   2-JAN-1994   M. Fortner  use RCP params TRIG_LEVEL, DET_REGION
C-   Updated  16-MAR-1994   Darien Wood  call MUREFIT_DROP
C-   Updated  28-MAR-1994   M. Fortner  call SADROP
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IERR,SKIP_LEVEL,TRIG_LEVEL,DET_REGION,NTRAKS,MR_BITS
      CHARACTER*32 MESSID,CALLER
      CHARACTER*80 MESSAG
      INTEGER IER,GFIT, FLAG_MUD1,LPMUO,GZPMUO,MAG_DBL,IERR_POL
      INTEGER RUN,ID,MUREFIT
      LOGICAL MURECO_HST,FIRST,OK,COMPUTE_MU_QUALITY,MCONST
      EXTERNAL MURECO_HST,COMPUTE_MU_QUALITY,MCONST
      REAL TIME29
      REAL PWAM,PSAM
      DATA FIRST /.TRUE./
C
      MURECO_EVT=.TRUE.
C
C  Set RCP bank to MUON_RCP for now.   (This shoud be MURECO_RCP.)
C  =================================
C
      CALL EZPICK('MURECO_RCP')
C
      IF(FLAG_MUD1(2) .EQ. 1) THEN
        OK = MCONST()
      ENDIF
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZGET('SKIP_LEVEL',SKIP_LEVEL,IER)
        CALL EZGET('TRIG_LEVEL',TRIG_LEVEL,IER)
        CALL EZGET('DET_REGION',DET_REGION,IER)
        CALL EZGET('MAG_DBL',MAG_DBL,IER)
        CALL EZGET('GFIT',GFIT,IER)
        CALL EZGET('MUREFIT',MUREFIT,IER)
      END IF
C
C     -- Muon track finding/fitting in the muon system...
C
C drop appropriate banks for muon/global (re)finding/(re)fitting
        CALL MUREFIT_DROP(MUREFIT,IERR)
C
      IF (MAG_DBL.GE.1) THEN
        CALL MU_MAG_POL(1,PWAM,PSAM,IERR_POL)
      ELSE
        CALL MU_MAG_POL(2,PWAM,PSAM,IERR_POL)
      ENDIF
      CALL MUANLZ(IERR,SKIP_LEVEL,TRIG_LEVEL,DET_REGION)
      IF(IERR.NE.0) THEN
        IF(IERR.LT.0) THEN 
          MESSID='Fatal Error in MUANLZ' 
        ELSE
          MESSID='Non-fatal Error in MUANLZ' 
        ENDIF
        CALLER='MURECO_EVT'
        WRITE(MESSAG,61) IERR
        CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
        IF(IERR.LT.0) GO TO 800
      ENDIF
      CALL SADROP                ! Drop SAMUS working banks
C
C  Check if main ring event
C
      CALL MAIN_RING(TIME29,MR_BITS,IERR)
      IF (TIME29.GT.0.1.AND.TIME29.LT.0.5) THEN
        CALL GTMTRH(NTRAKS)
        IF (NTRAKS.GT.10) THEN
          CALL EVNTID(RUN,ID)
          MESSID='Main Ring Event'
          CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
          GO TO 900
        END IF
      END IF
C
C
C     -- Link muon tracks between the Muon system and the Central
C        tracker.
C
      CALL MUFIT(IERR)
      IF(IERR.NE.0) THEN
        MESSID='Error in MUFIT'
        CALLER='MURECO_EVT'              !FILLS MFIT,MHIT BANK
        WRITE(MESSAG,61) IERR
        CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
        GO TO 800
      ENDIF
      CALL MULINK(IERR)
      IF(IERR.NE.0) THEN
        MESSID='Error in MULINK'
        CALLER='MURECO_EVT'                !FILLS MUON,MUCA,MUCD BANK
        WRITE(MESSAG,61) IERR
        CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
        GO TO 800
      ENDIF
C
C     -- Global fit...
C
      IF(GFIT .GT. 0) THEN
C        IF (GFIT.EQ.1) CALL MUFITG(IERR)  ! this code is obsolete
        IF (GFIT.EQ.2.OR.GFIT.EQ.3) CALL MUGLOBAL(IERR)  
        IF(IERR.NE.0) THEN
          MESSID='Error in MUGLOBAL'         
          CALLER='MURECO_EVT'
          WRITE(MESSAG,61) IERR
          CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
        ENDIF
      ENDIF
C
C     -- Certified muon tracks...
C
      CALL MUPMUO(IERR)
      IF(IERR.NE.0) THEN
        MESSID='Error in MUPMUO'            !FILLS PMUO BANK
        CALLER='MURECO_EVT'
        WRITE(MESSAG,61) IERR
        CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
        GO TO 800
      ENDIF
C
C run CLEANMU on all PMUO banks and fill status masks
      IF(.NOT.COMPUTE_MU_QUALITY()) THEN
        MESSID='Error in COMPUTE_MU_QUALTIY'
        CALLER='MURECO_EVT'
        WRITE(MESSAG,61) IERR
        CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
        GO TO 800
      ENDIF  
C
  800 CONTINUE
C
C     -- Histograms...
C
      IF(.NOT. MURECO_HST()) THEN
        MESSID='Error in MURECO_HST'
        CALLER='MURECO_EVT'
        MESSAG='This should not happen.  Check MURECO_HST.'
        CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
        CALL EZRSET
        GO TO 999
      ENDIF
 900  CONTINUE
C
C  Reset RCP bank.
C  ===============
C
      CALL EZRSET
C
   61 FORMAT('Error code=',I10,' ')
  999 RETURN
      END
