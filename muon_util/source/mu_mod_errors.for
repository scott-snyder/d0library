      SUBROUTINE MU_MOD_ERRORS(NMOD,E_DRIFT,E_NONDRIFT,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : give the spatial errors associated with a 
C-                         given muon module
C-
C-   Inputs  : NMOD = module number
C-   Outputs : E_DRIFT = error in drift direction (cm)
C-             E_NONDRIFT = error in non-drift (delta-t) direction (cm)
C-             IER = 0 for success
C-   Controls: 
C-
C-   Created  13-SEP-1993   Darien R. Wood
C-   Modified  6-OCT-1993   Paul Z. Quintas -- put in MC values
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER NMOD,IER
      REAL E_DRIFT,E_NONDRIFT
      INTEGER LMTIM,LMDTM
      INTEGER GZMTIM,GZMDTM
      EXTERNAL GZMTIM,GZMDTM
	INTEGER MC, MUDVER
	LOGICAL FIRST
	DATA FIRST /.TRUE./
C----------------------------------------------------------------------
	IF (FIRST) THEN
	  MC = MUDVER(0)
	  FIRST = .FALSE.
	ENDIF
C
      IER = -1
      E_DRIFT = 9999.
      E_NONDRIFT = 9999.
C
C if Monte Carlo hardwire to typical run 1A resolutions
C
	IF (MC.EQ.11.OR.MC.EQ.12) THEN
	  E_DRIFT = 0.0700			! 700 microns
	  E_NONDRIFT = 20.0			! 20 cm
	  IER = 0
	ENDIF
C
C retrieve module resolutions from calibration constant headers
C
      LMTIM = GZMTIM(NMOD)
      LMDTM = GZMDTM(NMOD)
      IF(LMTIM.GT.0 .AND. LMDTM.GT.0) THEN
        E_DRIFT = FLOAT(IC(LMTIM+3))/10000.    ! convert microns to cm
        E_NONDRIFT = FLOAT(IC(LMDTM+3))/1000.  ! convert 10*microns to cm
        IER = 0
      ENDIF  
C
  999 RETURN
      END
