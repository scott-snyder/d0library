      LOGICAL FUNCTION SAMRECO_EVT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Steering routine for SAMUS reconstruction
C-                         package, SAMRECO in event phase.
C-
C-   Returned value  :
C-   Inputs  : (none)
C-   Outputs : (none)
C-   Controls: (none)
C-
C-   Created   10-MAY-1991 O.Eroshin
C-   Updated   3-MAY-1994   Andrei Mayorov   book MUHT,MTRH for standalone
C-                  SAMUS reconstruction, add parameters in call to SAANAL
C-   Updated   5-MAY-1994   Andrei  Mayorov   add field map initialization
C-   Updated  16-MAY-1994   Andrei  Mayorov   call MU_MAG_POL to account
C-                                            magnet polarity correctly.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      LOGICAL SAANaL,NORTH,SOUTH
      INTEGER IERR,LMUHT,LMTRH
      CHARACTER*32 MESSID,CALLER
      CHARACTER*80 MESSAG
      LOGICAL  SAMRECO_HST,first,OK,MCONST
      EXTERNAL MCONST
      DATA first/.true./
      EXTERNAL SAMRECO_HST
      REAL pwam,psam
      INTEGER mag_dbl
      REAL v(3),f(3)
      DATA v/0.,0.,0./
      INTEGER FLG,FLAG_MUD1
      EXTERNAL FLAG_MUD1
C
      SAMRECO_EVT = .TRUE.
C
C  Set RCP bank to SAMUS_RCP for now.   (This shoud be SAMRECO_RCP.)
C  =================================
C
      IF ( FLAG_MUD1(2).EQ.1 ) THEN
        CALL EZPICK('MURECO_RCP')
        OK = MCONST()
        CALL EZRSET
      END IF
      IF (FIRST) THEN
      CALL EZPICK('MURECO_RCP')
        FIRST=.FALSE.
        CALL SAFLD(v,f) ! to initialize field map
        CALL EZGET('MAG_DBL',MAG_DBL,IERR)
      CALL EZRSET
      END IF
C
C     -- Muon track finding/fitting in the SAMUS system...
C
      IF (MAG_DBL.GE.1) THEN
        CALL MU_MAG_POL(1,PWAM,PSAM,IERR)
      ELSE
        CALL MU_MAG_POL(2,PWAM,PSAM,IERR)
      ENDIF

      CALL BKMUHT(0,0,LMUHT)
      CALL BKMTRH(0,0,lmtrh)
      CALL MUDPAK(ierr)
      NORTH=.TRUE.
      SOUTH=.TRUE.
      IF (.NOT.SAANAL(NORTH,SOUTH))                THEN
        MESSID = 'Error in SAANAL'
        CALLER = 'SAMRECO_EVT'
        WRITE(MESSAG,61) IERR
        CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
        GO TO 800
      ENDIF
C
C     -- Histograms...
C
  800 CONTINUE
      IF(.NOT. SAMRECO_HST()) THEN
        MESSID = 'Error in SAMRECO_HST'
        CALLER = 'SAMRECO_EVT'
        MESSAG = 'This should not happen.  Check SAMRECO_HST.'
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
