      SUBROUTINE COSMIC_TRD_PARAMETERS
     &         (NEWPAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dummy first pass at TOOL_PARAM
C-
C-   Inputs  : NEWPAR : Number of parameter sets to read
C-   Outputs : None
C-   Controls: None
C-
C-   Created  22-SEP-93   by the L2STATE program
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:COSMIC_TRD_CUTS.INC'
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
C
      IF(NEWPAR.GT.0) THEN      ! got a new download this run
        CALL EZPICK_NOMSG('COSMIC_TRD',IER) ! downloaded parameters for tool
        OK = IER.EQ.0
        IF (OK) THEN  ! did have a download
C
C...find out how many parameter sets downloaded
          NPAR = 0    !forget previous downloads
          IF (IER .EQ. 0) CALL EZGETA('NTRIPLET',0,0,0,NPARIN,IER)
          IF (IER .NE. 0) CALL ERRMSG('COSMIC_TRD',
     &          'COSMIC_TRD_PARAMETERS',
     &          'Could not find number of PARAMETER sets','F')
          IF (NPARIN.GT.NPAR_MAX) CALL ERRMSG('COSMIC_TRD',
     &      'COSMIC_TRD_PARAMETERS',
     &      'Too many parameter sets: some lost','F')
          NPAR = NPARIN
          IF (IER .EQ. 0) CALL EZGET('NTRIPLET',NTRIPLET,IER)
          IF (IER .NE. 0) THEN      ! Error reading RCP
            CALL ERRMSG('COSMIC_TRD','COSMIC_TRD_PARAMETERS',
     &          'Couldn''t find parameter','F')
          ELSE
            CALL EZRSET
          ENDIF
        ENDIF
      ENDIF
C
  999 RETURN
      END
