      SUBROUTINE L2_MASSCUT_PARAMETERS(NEWPAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : initialization of downloaded parameters
C-      for mass tool (dummy routine)
C-      Always forget previously stored parameters
C-
C-   Inputs  : NEWPAR : [BYTE] if it's equal to zero, ignore
C-      this run begin--nothing new downloaded.  It's the number of sets of
C-      parameters downloaded for THIS tool.
CC-   Outputs :
C-   Controls:
C-
C-   Created  19-APR-1994     Lewis Taylor Goss
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      BYTE NEWPAR
      INTEGER IP,NCHR,IER,NPAR
      LOGICAL OK,EZERROR
      INCLUDE 'D0$INC:L2_MASSCUT_CUTS.INC'
      CHARACTER*10 OBJECT
C----------------------------------------------------------------------
      IF (NEWPAR .GT. 0) THEN
        CALL EZPICK('L2_MASSCUT')
        OK = .NOT.EZERROR(IER)
        IF (OK) THEN
          CALL EZGET('NUMBER_OF_SETS',NPAR,IER)
          IF (IER .EQ. 0) THEN
            DO IP=1,NPAR
              CALL EZGETS('OBJECT',IP,OBJECT,NCHR,IER)
              IF (IER.EQ.0) CALL EZGETA('LEADING',IP,IP,1,LEADING,IER)
              IF (LEADING.AND.(OBJECT.EQ.'ELECTRON').AND.(IER.EQ.0)) 
     &      CALL ERRMSG('L2_MASSCUT_PARAMETERS','L2_MASSCUT_PARAMETERS',
     &      'LEADING = .TRUE. & OBJECT = ELECTRON not recommended.','W')
            END DO
            IF (IER.NE.0) CALL ERRMSG('L2_MASSCUT_PARAMETERS',
     &        'L2_MASSCUT_PARAMETERS','Couldn''t find parameter.','F')
          ENDIF
          CALL EZRSET
        ELSE
          CALL ERRMSG('L2_MASSCUT_RCP','L2_MASSCUT_PARAMETERS',    
     &      'Couldn''t find bank.','F')
        ENDIF
      END IF
  999 RETURN
      END
