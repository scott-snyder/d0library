C VAX/DEC CMS REPLACEMENT HISTORY, Element FOVRLP_COSMIC.FOR
C *1     4-NOV-1993 10:58:06 AVERY "FDC changes for v12 RECO"
C VAX/DEC CMS REPLACEMENT HISTORY, Element FOVRLP_COSMIC.FOR
      SUBROUTINE FOVRLP_COSMIC( HALF1,UNIT1,QUAD1,SECT1,
     &                          HALF2,UNIT2,QUAD2,SECT2, CHKOVL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Checks overlap of Theta and Phi chamber
C-                         for given sectors.
C-                         Simplified loose version for cosmic ray events.
C-
C-   Inputs  : HALF1,UNIT1,QUAD1,SECT1 = Location of starting sector 
C-             HALF2,UNIT2,QUAD2,SECT2 = Location of possible overlapping
C-                                 sector
C-   Outputs : CHKOVL = Set TRUE if ANY starting and possible overlapping
C-                      sector actually do overlap.
C-
C-   Created   28-MAY-1993   Robert E. Avery   Based on FOVRLP.FOR by
C-                                              Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER HALF1,UNIT1,QUAD1,SECT1,HALF2,UNIT2,QUAD2,SECT2
      LOGICAL CHKOVL
C
      INTEGER QUADTB(0:7),QUADTE(0:7),QUADPB(0:7),QUADPE(0:7)
      INTEGER SECTT(2,0:5)
      DATA QUADTB / 4,5,6,7,3,0,1,2/
      DATA QUADTE / 5,6,7,4,0,1,2,3/
      DATA QUADPB / 34,7,16,24,29,1,11,19/
      DATA QUADPE / 10,20,28,2,6,16,24,34/
      DATA SECTT  / 0,3,0,4,0,5,1,5,2,5,3,5 /
C----------------------------------------------------------------------
      CHKOVL = .FALSE.
C
C   Check for bad starting sector location
C
      IF(HALF1.GT.1) GOTO 999
      IF(QUAD1.GT.7) GOTO 999
      IF(UNIT1.EQ.0 .AND. SECT1.GT.5) GOTO 999
      IF(UNIT1.EQ.1 .AND. SECT1.GT.35) GOTO 999
C
C   Just check if the input possible sector overlaps the
C            starting sector.
C
      IF(UNIT1.EQ.0) THEN
        IF(UNIT2.EQ.0) THEN
          IF(QUAD2.EQ.QUADTB(QUAD1) .OR. QUAD2.EQ.QUADTE(QUAD1)) THEN
            IF (SECT2.GE.SECTT(1,SECT1) .AND. 
     &          SECT2.LE.SECTT(2,SECT1)) THEN
              CHKOVL=.TRUE.
            ENDIF
          ENDIF
        ELSEIF(UNIT2.EQ.1) THEN
          IF(QUADPB(QUAD1).LT.QUADPE(QUAD1)) THEN
            IF (SECT2.GE.QUADPB(QUAD1) .AND.
     &          SECT2.LE.QUADPE(QUAD1)) THEN
              CHKOVL=.TRUE.
            ENDIF
          ELSE
            IF (SECT2.GE.QUADPB(QUAD1) .OR.
     &          SECT2.LE.QUADPE(QUAD1)) THEN
              CHKOVL=.TRUE.
            ENDIF
          ENDIF
        ENDIF
      ELSEIF (UNIT1.EQ.1) THEN
        IF(UNIT2.EQ.0) THEN
          IF(QUADPB(QUAD2).LT.QUADPE(QUAD2)) THEN
            IF (SECT1.GE.QUADPB(QUAD2) .AND.
     &          SECT1.LE.QUADPE(QUAD2)) THEN
              CHKOVL=.TRUE.
            ENDIF
          ELSE
            IF (SECT1.GE.QUADPB(QUAD2) .OR.
     &          SECT1.LE.QUADPE(QUAD2)) THEN
              CHKOVL=.TRUE.
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
