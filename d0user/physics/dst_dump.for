      FUNCTION DST_DUMP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     dump DST banks
C-
C-   Returned value  : true
C-
C-   Created  30-JUL-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL DST_DUMP
      EXTERNAL PRPARH,PRPELC,PRPMUO,PRPNUT,PRJETS,PRPPHO,PRVERT
C----------------------------------------------------------------------
C
      DST_DUMP=.TRUE.
C
      CALL DMPANY('VERT',PRVERT)        ! vertices
      CALL DMPANY('PARH',PRPARH)        ! paticle banks header
C
      CALL DMPANY('PNUT',PRPNUT)        ! neutrino
      CALL DMPANY('JETS',PRJETS)        ! jets
      CALL DMPANY('PELC',PRPELC)        ! electrons
      CALL DMPANY('PPHO',PRPPHO)        ! photons
      CALL DMPANY('PMUO',PRPMUO)        ! muons
C
      CALL ISA_DUMP                     ! ISAJET banks
  999 RETURN
      END
