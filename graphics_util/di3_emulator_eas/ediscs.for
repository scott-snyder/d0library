      SUBROUTINE EDISCS (NINTEG, ILIST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Disconnects the segments from transformation
C-                         network.
C-
C-   Inputs  : ninteg   no. of items
C-             ilist    list of items
C-
C-   Created   8-FEB-1989   SHAHRIAR ABACHI
C-   UPDATED   11-AUG-1992   SHAHRIAR ABACHI   fine scale added to knob 8
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  NINTEG, ILIST(*), I, ISEG
      CHARACTER*4 TRNSF(50)
      COMMON /ESCAPC/ TRNSF
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:NEWDI3.INC/LIST'
      INCLUDE 'D0$INC:GDIVWT.INC/LIST'
      REAL DN
      EXTERNAL ERRHND
C- Local variables
      REAL  EYEPN(3), UPV(3), VUPN(3), TVEC(3), DIST
      CHARACTER*4 SEGN, SSTR
      LOGICAL COUNT
C
      IDIVWT = 1
C
C--------------------------
C
      IF(NUDI3) THEN
C
      CALL PDI(TRNSF(1), 1, 1, DISPL//'.IRX"', ERRHND)
      CALL PDI(TRNSF(2), 1, 1, DISPL//'.IRY"', ERRHND)
      CALL PDI(TRNSF(3), 1, 1, DISPL//'.IRZ"', ERRHND)
      CALL PDI(TRNSF(4), 1, 1, DISPL//'.IS"', ERRHND)
      CALL PDI(TRNSF(5), 1, 1, DISPL//'.IF"', ERRHND) !NEW
      CALL PDI('ACCM5', 1, 1, DISPL//'.IT"', ERRHND)
      CALL KFUND
C
      ELSE
C
        ISEG = 0
        DO 2 I = 1,NINTEG
          COUNT = .TRUE.
          DO 3 WHILE(COUNT)
          ISEG = ISEG + 1
          IF ( ISEG .GE. NSEGS) COUNT = .FALSE.
          IF ( SEGINF(1,ISEG) .EQ. ILIST(I) ) COUNT = .FALSE.
    3   CONTINUE
        CALL KBLDN(SEGINF(6,ISEG), SSTR)
        SEGN = 'R' // SSTR(1:3)
C
        CALL PDI(TRNSF(1), 1, 1, SEGN//'.IRX', ERRHND)
        CALL PDI(TRNSF(2), 1, 1, SEGN//'.IRY', ERRHND)
        CALL PDI(TRNSF(3), 1, 1, SEGN//'.IRZ', ERRHND)
        CALL PDI(TRNSF(4), 1, 1, SEGN//'.IS', ERRHND)
CC        CALL PDI(TRNSF(5), 1, 1, SEGN//'.IS', ERRHND)
        CALL PDI(TRNSF(5), 1, 1, SEGN//'.IF', ERRHND)
        CALL PDI(TRNSF(6), 1, 1, SEGN//'.IL', ERRHND)
        CALL PDI('ACCM5', 1, 1, SEGN//'.IT"', ERRHND)
    2 CONTINUE
C
      ENDIF
C
C--------------------------
C
      CALL PPURGE(ERRHND)
C
  999 RETURN
      END
