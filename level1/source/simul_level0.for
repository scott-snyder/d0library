      SUBROUTINE SIMUL_LEVEL0 ( Z_VERTEX, L0_BIN, L0_VALID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculates the Level 0 Trigger response to a 
C-                         given interaction vertex along the z-axis.
C-
C-   Inputs  : 
C-     Z_VERTEX            specifies position of the interaction vertex for
C-                         which the corresponding Level 0 Trigger Bin is
C-                         requested. 
C-                         Units: cm
C-                         
C-   Outputs : 
C-     L0_BIN              the number of the Level 0 Trigger Bin including the
C-                         specified vertex position.
C-                         Range: -15 to +15
C-
C-     L0_VALID            is the good signal accompanying the level 0 fast
C-                         vertex position. This signal will be negated for
C-                         vertices falling outside of the defined bins.
C-
C-   Controls: None
C-
C-   Comments :
C-                         A call to INIT_LOOKUP_ROUTINES must have been made
C-                         before this routine can be called.
C-                         
C-                         The routine will return bin #0 when the vertex
C-                         position is outside of the coverage of the level 0
C-                         bins, i.e. for (low boundary of bin #-15 ) < (vertex
C-                         z-position) < (high boundary of bin #+15). 
C-
C-                         This routine only partially simulate the level 0
C-                         "GOOD" signal that specifies whether the level 0 was
C-                         able to find the vertex of the interaction. This
C-                         line could be negated for reasons other than the
C-                         non-existence of a bin including  vertex position.
C-
C-                         A vertex exactly on a bin boundary between two bin
C-                         coverages will be considered as belonging to the bin
C-                         closer to the center of the detector.
C-                         [...[...[...[...]...]...]...]
C-                         
C-   Defined  6-FEB-1990 MICHIGAN STATE UNIVERSTITY, TRIGGER CONTROL SOFTWARE
C-   Updated  2-AUG-1990 MICHIGAN STATE UNIVERSTITY, TRIGGER CONTROL SOFTWARE
C-   Updated  Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                      - Made IMPLICIT NONE statement recognizable by D0FLAVOR
C----------------------------------------------------------------------
      IMPLICIT NONE 
C      
      INCLUDE     'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE     'D0$INC:LEVEL1_LOOKUP.INC'
C
      REAL         Z_VERTEX
      INTEGER      L0_BIN
      LOGICAL      L0_VALID
C
C       variable to allow register optimization to occur
      INTEGER      TEMP_BIN
C
C----------------------------------------------------------------------
C
      IF ( ( Z_VERTEX .LT. L0_BIN_COVERAGE(L0_BIN_MIN,Z_LOW)  ) 
     &     .OR.
     &     ( Z_VERTEX .GT. L0_BIN_COVERAGE(L0_BIN_MAX,Z_HIGH) ) ) THEN
        L0_VALID = .FALSE.
        TEMP_BIN   = 0
        GOTO 999        
      END IF
C
      L0_VALID = .TRUE.
C
      DO TEMP_BIN = L0_BIN_MIN, -1
        IF ( Z_VERTEX .LT. L0_BIN_COVERAGE(TEMP_BIN,Z_HIGH) ) GOTO 999
      END DO
C
      DO TEMP_BIN = 0, L0_BIN_MAX
        IF ( Z_VERTEX .LE. L0_BIN_COVERAGE(TEMP_BIN,Z_HIGH) ) GOTO 999
      ENDDO
C----------------------------------------------------------------------
  999 L0_BIN = TEMP_BIN
      RETURN
      END
