      SUBROUTINE L2ETSUM_PARAMETERS(NEWPAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : initialization of downloaded parameters
C-      for Missing ET level 2 filter
C-      Always forget previously stored parameters
C-
C-   Inputs  : NEWPAR : Irrelevant except for CALIB
C-   Inputs  : NEWPAR : [BYTE] if it's equal to zero, ignore
C-      this run begin--nothing new downloaded.  It's the number of sets of
C-      parameters downloaded for THIS tool.
CC-   Outputs :
C-   Controls:
C-
C-   Created  29-JUN-1991   James T. Linnemann
C-   Updated  16-DEC-1992   William Cobau & Amber Boehlein - Add ETOT cut
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:L2ETSUM_CUTS.INC'
C&IF VAXVMS,VAXELN
      BYTE NEWPAR
C&ELSE
C&      INTEGER NEWPAR
C&ENDIF
      INTEGER NPARIN
      CHARACTER*80 MSG
      LOGICAL EZERROR,OK
      INTEGER IER
C----------------------------------------------------------------------
      IF(NEWPAR.GT.0) THEN      ! got a new download this run
        CALL EZPICK_NOMSG('L2ETSUM',IER) ! downloaded parameters for tool
        OK = IER.EQ.0
        IF (OK) THEN  ! did have a download
          NPAR = 0    !forget any previous constants
C
C...find out how many parameter sets downloaded
          IF (IER .EQ. 0) CALL EZGETA('ETSUM_MIN',0,0,0,NPARIN,IER)
          IF (IER .NE. 0) CALL ERRMSG('L2ETSUM','L2ETSUM_PARAMETERS',
     &          'Couldn''t find number of PARAMETER sets','F')
          IF (NPARIN.GT.NPAR_MAX) CALL ERRMSG('L2ETSUM',
     &      'L2ETSUM_PARAMETERS',
     &      'Too many parameter sets: some lost','F')
          NPAR = MAX(NPARIN,NPAR)
          IF (IER .EQ. 0) CALL EZGET('ETSUM_MIN',ETSUM_MIN,IER)
          IF (IER .EQ. 0) CALL EZGET('ETOT_MAX',ETOT_MAX,IER)
          IF (IER .NE. 0) THEN      ! Error reading RCP
            CALL ERRMSG('L2ETSUM','L2ETSUM_PARAMETERS',
     &          'Couldn''t find parameter','F')
          ELSE
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
