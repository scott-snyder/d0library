      LOGICAL FUNCTION MUGEANT_FLG(I)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : flag for muon GEANT
C-
C-   Inputs  : I : 0     reset flag
C-                 1     set   flag
C-                 other retur flag value
C-   Outputs : None
C-   Controls: None
C-
C-   Created   5-OCT-1992   Atsushi Taketani
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  I
      LOGICAL  FLG
      SAVE     FLG
      DATA FLG /.FALSE./
C----------------------------------------------------------------------
      IF ( I.EQ.0 ) THEN
        FLG = .FALSE.
      ELSE IF ( I.EQ.1 ) THEN
        FLG = .TRUE.
      END IF
C
      MUGEANT_FLG = FLG
C
  999 RETURN
      END
