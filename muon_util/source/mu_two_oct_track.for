      SUBROUTINE MU_TWO_OCT_TRACK(ITRACK,QUAD,NPT,ANSWER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Identify Cosmics by checking if the track
C-             has hits from different phi octants.
C-
C-   Inputs  : ITRACK is the MUOT track number, NPT is number WAMUS
C-             hits on the track (identical to first two elements of GTMUOT).
C-   Outputs : Logical ANSWER
C-   Controls: QUAD = 1 to 4 indicates CF track. This routine is for CF
C-             tracks only.
C-
C-   Created   8-SEP-1992   Tom Diehl
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ITRACK,NPT,QUAD,OCTANT,LAST_OCTANT,CHECK34,CHECK07
      LOGICAL ANSWER
      INTEGER IHIT,IWADD,IHMUOH,ITSIGN,IDELT,IPAD   !mhtt
      INTEGER NMOD,NPLN,NWIR,JERR !muadd
C----------------------------------------------------------------------
      ANSWER = .FALSE.
      IF(QUAD.GT.4) RETURN      !This routine is for CF tracks at this time.
      LAST_OCTANT = -1
      CHECK07 = 0
      CHECK34 = 0
      DO IHIT = 1,NPT
        CALL GTMHTT(ITRACK,IHIT,IWADD,IHMUOH,ITSIGN,IDELT,IPAD)
        CALL MUADD(IWADD,NMOD,NPLN,NWIR,JERR)
        OCTANT = NMOD - (INT(NMOD/10)*10)
        IF(NMOD.EQ.10.OR.NMOD.EQ.20.OR.NMOD.EQ.30) THEN
          IF(LAST_OCTANT.NE.-1) THEN
            IF(LAST_OCTANT.NE.0.AND.LAST_OCTANT.NE.7) ANSWER = .TRUE.
          ELSE 
            CHECK07 = 1 
          ENDIF
        ELSEIF(NMOD.EQ.13.OR.NMOD.EQ.23.OR.NMOD.EQ.33) THEN
          IF(LAST_OCTANT.NE.-1) THEN
            IF(LAST_OCTANT.NE.3.AND.LAST_OCTANT.NE.4) ANSWER = .TRUE.
          ELSE
            CHECK34 = 1
          ENDIF
        ELSEIF(LAST_OCTANT.EQ.-1) THEN
          LAST_OCTANT = OCTANT
        ELSEIF(OCTANT.NE.LAST_OCTANT) THEN
          ANSWER = .TRUE.
        ENDIF
      ENDDO
      IF(CHECK07.EQ.1) THEN
        IF(LAST_OCTANT.NE.0.AND.LAST_OCTANT.NE.7.AND.
     $    LAST_OCTANT.NE.-1) THEN
          ANSWER = .TRUE.
        ENDIF
      ENDIF
      IF(CHECK34.EQ.1) THEN
        IF(LAST_OCTANT.NE.3.AND.LAST_OCTANT.NE.4.AND. 
     $    LAST_OCTANT.NE.-1) THEN
          ANSWER = .TRUE.
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
