      SUBROUTINE L2ETMISS_PARAMETERS(NEWPAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : initialization of downloaded parameters
C-      for Missing ET level 2 filter
C-      Always forget previously stored parameters
C-
C-   Inputs  : NEWPAR : [BYTE] if it's equal to zero, ignore
C-      this run begin--nothing downloaded for this tool.
C-      If it's nonzero, then look for new parameters, but DO NOT cause
C-      cause errors if none are there.  To be sure a node reloaded during a run
C-      gets caught up, ALL its tools get told to look for their parameters.
C-   Outputs : this loads parameters into a common block
C-        For an example which doesn't use common, see 
C-        D0$LEVEL2$SOURCE:L2_GENERIC.FOR
C-   Controls:
C-
C-   Created  29-JUN-1991   James T. Linnemann
C-   Modified 13-Jul-1992   Ulrich Heintz: add missing Et significance cut
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:L2ETMISS_CUTS.INC'
C&IF VAXVMS,VAXELN
      BYTE NEWPAR
C&ELSE
C&      INTEGER NEWPAR
C&ENDIF
      INTEGER NPARIN
      CHARACTER*80 MSG
      LOGICAL OK
      INTEGER IER,IER1,I
C----------------------------------------------------------------------
      IF(NEWPAR.GT.0) THEN      ! got a new download this run
        CALL EZPICK_NOMSG('L2ETMISS',IER) ! downloaded parameters for tool
        OK = IER.EQ.0
        IF (OK) THEN  ! did have a download
C
C...find out how many parameter sets downloaded
          NPAR = 0    !forget previous downloads
          IF (IER .EQ. 0) CALL EZGETA('ETMISS_MIN',0,0,0,NPARIN,IER)
          IF (IER .NE. 0) CALL ERRMSG('L2ETMISS','L2ETMISS_PARAMETERS',
     &          'Couldn''t find number of PARAMETER sets','F')
          IF (NPARIN.GT.NPAR_MAX) CALL ERRMSG('L2ETMISS',
     &      'L2ETMISS_PARAMETERS',
     &      'Too many parameter sets: some lost','F')
          NPAR = NPARIN
          IF (IER .EQ. 0) CALL EZGET('ETMISS_MIN',ETMISS_MIN,IER)
          IF (IER .EQ. 0) CALL EZGET('ETMISS_SIG_MIN',ETMISS_SIG_MIN,
     &      IER1)
          IF (IER .NE. 0) THEN      ! Error reading RCP
            CALL ERRMSG('L2ETMISS','L2ETMISS_PARAMETERS',
     &          'Couldn''t find parameter','F')
          ELSE
            IF(IER1.NE.0)THEN ! optional parameters
              DO I=1,NPAR_MAX
                ETMISS_SIG_MIN(I)=0.
              ENDDO
              CALL ERRMSG('L2ETMISS','L2ETMISS_PARAMETERS',
     &          'parameter ETMISS_SIG_MIN not found - set to 0','W')
            ENDIF
            CALL EZRSET
          ENDIF
        ENDIF
      ENDIF
C
C...now for fast unpacking init
      CALL CL2_INI      ! protected by IF(FIRST)
      CALL L2_VERT_PARAMETERS
  999 RETURN
      END
