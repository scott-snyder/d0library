      LOGICAL FUNCTION MUFIX_EVT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Steering routine for muon reconstruction
C-                         sub-package, MUFIX in event phase.
C-
C-   Returned value  :
C-   Inputs  : (none)
C-   Outputs : (none)
C-   Controls: (none)
C-
C-   Created  20-MAY-1991    Shahriar Abachi : Made from MURECO_EVT
C     DH 10/91 change MUANLZ call
C     DH 1/92 only MUANLZ erros < 0 are fatal
C     DH 3/92 modify error messages
C-    29-JAN-1992 Cecilia Gerber : add MUREFIT options
C     UPDATED  13-AUG-1993      S. ABACHI     MCONST replaced MRDBGE
C     UPDATED  31-oct-1993 D. Wood : add calls to MUREFIT routines
C_    Updated  12/93 MF : add RCP params TRIG_LEVEL, DET_REGION
C     UPDATED  28-apr-1994 D. Wood : add calls to MUREFIT_CLEAN
C     UPDATED  21-jun-1995 RE Hall : Change name from MUONLY_EVT to 
C              MUFIX_EVT may diverge from muonly from this date forward
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      INTEGER IERR,SKIP_LEVEL,TRIG_LEVEL,DET_REGION
      INTEGER IRUN, IEVT
      CHARACTER*32 MESSID,CALLER
      CHARACTER*80 MESSAG
      INTEGER IER,GFIT, FLAG_MUD1, MUREFIT, MAG_DBL, IERR_POL
      LOGICAL MURECO_HST,FIRST, OK,MCONST,OLDFIT_SAVE
      EXTERNAL MURECO_HST
      REAL PWAM,PSAM
      DATA FIRST/.TRUE./
C
      MUFIX_EVT=.TRUE.
C
C  Set RCP bank to MURECO_RCP for now.
C  =================================
C
      CALL EZPICK('MURECO_RCP')
      IF ( FLAG_MUD1(2).EQ.1 ) THEN
        OK = MCONST()
C        CALL MRDBGE(OK)
      END IF
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZGET('SKIP_LEVEL',SKIP_LEVEL,IER)
        CALL EZGET('TRIG_LEVEL',TRIG_LEVEL,IER)
        CALL EZGET('DET_REGION',DET_REGION,IER)
        CALL EZGET('MAG_DBL',MAG_DBL,IER)
        CALL EZGET('GFIT',GFIT,IER)
        CALL EZGET('MUREFIT',MUREFIT,IER)
        IF(MUREFIT.GE.10) THEN
          OLDFIT_SAVE = .TRUE.
        ELSE
          OLDFIT_SAVE = .FALSE.
        ENDIF
        MUREFIT = MOD(MUREFIT,10)
      END IF
C
C     -- Muon track finding/fitting in the muon system...
C
C check magnet polarity
      IF (MAG_DBL.GE.1) THEN
        CALL MU_MAG_POL(1,PWAM,PSAM,IERR_POL)
      ELSE
        CALL MU_MAG_POL(2,PWAM,PSAM,IERR_POL)
      ENDIF
C
C drop appropriate banks for muon/global (re)finding/(re)fitting
      CALL MUREFIT_DROP(MUREFIT,IERR)
C
C do track finding/fitting from MUD1 hits
      IF ((MUREFIT.EQ.1).OR.(MUREFIT.EQ.3)) THEN
        CALL MUANLZ(IERR,SKIP_LEVEL,TRIG_LEVEL,DET_REGION)
        IF(IERR.NE.0) THEN
          IF(IERR.LT.0) THEN
            MESSID='Fatal Error in MUANLZ'
          ELSE
            MESSID='Non-fatal Error in MUANLZ'
          ENDIF
          CALLER='MUFIX_EVT'
          WRITE(MESSAG,61) IERR
          CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
          IF(IERR.LT.0) GO TO 800
        ENDIF
        IF(MUREFIT.GE.3) THEN
C match up MUOT banks and update MFIT and MUON banks
          CALL MUREFIT_FILL(IERR)
        ENDIF
      ENDIF
C
      IF(MUREFIT.EQ.2) THEN
C book MFIT bank and fill it from MUOT
        CALL MUREFIT_MUFIT(IERR)
        IF(IERR.NE.0) THEN
          MESSID='Error in MUREFIT_MUFIT'
          CALLER='MUFIX_EVT'
          WRITE(MESSAG,61) IERR
          CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
        ENDIF
C fill MUON from MFIT
        CALL MUREFIT_LINK(IERR)
        IF(IERR.NE.0) THEN
          MESSID='Error in MUREFIT_LINK'
          CALLER='MUFIX_EVT'
          WRITE(MESSAG,61) IERR
          CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
        ENDIF
      ENDIF
C
C     -- Global fit...
C
      IF ((MUREFIT.EQ.2).OR.(MUREFIT.EQ.3)) THEN
        IF (GFIT.GE.2) CALL MUGLOBAL(IERR)
        IF(IERR.NE.0) THEN
          MESSID='Error in MUGLOBAL'
          CALLER='MUFIX_EVT'
          WRITE(MESSAG,61) IERR
          CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
        ENDIF
C
C     -- Certified muon tracks...
C
        CALL MUPMUO(IERR)
        IF(IERR.NE.0) THEN
          MESSID='Error in MUPMUO'            !FILLS PMUO BANK
          CALLER='MUFIX_EVT'
          WRITE(MESSAG,61) IERR
          CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
          GO TO 800
        ENDIF
        IF(.NOT.OLDFIT_SAVE) THEN
          CALL MUREFIT_CLEAN(IERR)        ! DROPS EXTRA BANKS AND LINKS
          IF(IERR.NE.0) THEN
            MESSID='Error in MUREFIT_CLEAN'
            CALLER='MUFIX_EVT'
            WRITE(MESSAG,61) IERR
            CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
            GO TO 800
          ENDIF
        ENDIF
      ENDIF
C
  800 CONTINUE
C
C     -- Histograms...
C
      IF(.NOT. MURECO_HST()) THEN
        MESSID='Error in MURECO_HST'
        CALLER='MUFIX_EVT'
        MESSAG='This should not happen.  Check MURECO_HST.'
        CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
        CALL EZRSET
        GO TO 999
      ENDIF
C
C  Reset RCP bank.
C  ===============
C
      CALL EZRSET
C
   61 FORMAT('Error code=',I10,' ')
  999 RETURN
      END
