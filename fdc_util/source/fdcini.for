      LOGICAL FUNCTION FDCINI
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Change HBOOK size.  Any task that must occur 
C-                         before any setup dialog, no runtime inter
C-                         action can  occur at this stage. FDCINI gets 
C-                         called before Setup Menu appears. If interface 
C-                         subroutine returns false program will abort.. 
C-
C-   Inputs  : 
C-   Outputs : 
C-
C-   Created  2-NOV-1988  Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NHB
      PARAMETER( NHB= 300000)
      REAL    HMEMOR
      COMMON // HMEMOR(NHB)
C----------------------------------------------------------------------
      CALL HLIMIT( NHB )
      FDCINI = .TRUE.
  999 RETURN
      END
