      FUNCTION ISMG_BEG()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       hook for begin run
C-
C-   Created   2-APR-1991   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL ISMG_BEG
      LOGICAL FLGVAL,ISMG_DIA
C----------------------------------------------------------------------
C
      ISMG_BEG=.TRUE.
      IF ( FLGVAL('ISA_DIAL') ) THEN
        CALL FLGSET('ISA_DIAL',.FALSE.)
      ELSE
        ISMG_BEG=ISMG_DIA()
      ENDIF
  999 RETURN
      END
