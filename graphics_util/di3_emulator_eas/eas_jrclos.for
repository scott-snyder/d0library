      SUBROUTINE JRCLOS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
CD   This routine closes the current open a segment.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  26-OCT-1988   A. VIRGO
C-   UPDATED  15-MAY-1990   S. ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL ERRHND
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:PLGATT.INC/LIST'
      INCLUDE 'D0$INC:PRIMVR.INC/LIST'
      INCLUDE 'D0$INC:NEWDI3.INC/LIST'
      REAL SEC
      LOGICAL FIRST
      DATA FIRST, SEC /.TRUE., 2.0/
C
      IF (SEGOPN) THEN
        CALL KUPDV
        SEGINF(5,NSEGS) = NPRIM
        CPX = 0.0
        CPY = 0.0
        CPZ = 0.0
C
        IF(IREND .EQ. 2 .AND. CPINTR .GT. 0) THEN
          IF(FIRST) THEN
            CALL PFN('TURNONACP"', 'CONSTANT', ERRHND)
            CALL PSNFIX(0, 2, 'TURNONACP"', ERRHND)
            FIRST = .FALSE.
          ENDIF
          CALL PCONN('TURNONACP"', 1, 1, 'TURNONDISPLAY', ERRHND)
          CALL PSNFIX(7, 1, SEGNAM//'.SURRN"', ERRHND)
          CALL PPURGE(ERRHND)
          CALL LIB$WAIT(SEC)
        ENDIF
        IF(.NOT. NUDI3) THEN
          CALL PPURGE(ERRHND)
        ENDIF
      ELSE
        CALL ERROR('JCLOSE: A SEGMENT IS NOT OPEN')
      ENDIF
C
      SEGOPN = .FALSE.
      NOROT = .FALSE.
      SEGNUM = -1
C
      RETURN
      END
