      SUBROUTINE KVW
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To perform the viewing transformation
C-                          for new DI3000 emulator.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  26-JUN-1989   SHAHRIAR ABACHI
C-   updated  10-JUN-1990   SHAHRIAR ABACHI     KVIEWP was introdued.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:LINATT.INC/LIST'
      INCLUDE 'D0$INC:NEWDI3.INC/LIST'
      REAL D0, D1, WIDEV
      REAL HCLP, YCLP, ZMIN, ZMAX
      REAL VUPN(3), NORM(3), UPV(3), EYEP(3), DIST
      REAL TVUX(3), TVUY(3), TVUI(3)
      REAL TTRV(3), TSCV(3)
      INTEGER I
      LOGICAL CLIP
      EXTERNAL ERRHND
C
      CALL KVIEWP(VUPN,EYEP,UPV,WIDEV,HCLP,YCLP,CLIP)
C
      TVUX(1) = UVIEW(1)
      TVUX(2) = UVIEW(2)
      TVUX(3) = 0.0
      TVUY(1) = UVIEW(3)
      TVUY(2) = UVIEW(4)
      TVUY(3) = 0.0
      TVUI(1) = UVIEW(5)
      TVUI(2) = UVIEW(6)
      TVUI(3) = 0.0
      CALL PSNV3D(TVUX, 1, 'MX33"', ERRHND)
      CALL PSNV3D(TVUY, 2, 'MX33"', ERRHND)
      CALL PSNV3D(TVUI, 3, 'MX33"', ERRHND)
C
      CALL PSNV3D(EYEP, 2, 'LKAT"', ERRHND)
      CALL PSNV3D(UPV, 3, 'LKAT"', ERRHND)
C
      GOTO (1,2,3,4), PRJTYP
C
    1 CONTINUE
C
C   2D and 3D orthographic (parallel) view.
C
      CALL PSNREA(UWIND(1), 2, 'WIND"', ERRHND)
      CALL PSNREA(UWIND(2), 3, 'WIND"', ERRHND)
      CALL PSNREA(UWIND(3), 4, 'WIND"', ERRHND)
      CALL PSNREA(UWIND(4), 5, 'WIND"', ERRHND)
      CALL PSNREA(HCLP, 6, 'WIND"', ERRHND)
      CALL PSNREA(YCLP, 7, 'WIND"', ERRHND)
      GOTO 9900
C
    2 CONTINUE
C
C    3D parallel oblique.
C
      CALL PEYEBK('"', PAROBZ, PAROBX, PAROBY, WIDEV,
     +               HCLP, YCLP, '"', ERRHND)
      GOTO 9900
C
    3 CONTINUE
C
C   3D Perspective orthographic view.
C
      D1 = 2.0 * ATAND( WIDEV / (2.0 * PERSP) )
      CALL PSNREA(D1, 2, 'FOVU"', ERRHND)
      CALL PSNREA(HCLP, 3, 'FOVU"', ERRHND)
      CALL PSNREA(YCLP, 4, 'FOVU"', ERRHND)
      GOTO 9900
C
    4 CONTINUE
C
C    3D Perspective oblique.
C
      CALL PEYEBK('"', PEROBZ, PEROBX, PEROBY, WIDEV,
     +               HCLP, YCLP, '"', ERRHND)
      GOTO 9900
C
 9900 CONTINUE
C
      CALL PSNBOO(CLIP, 1, DISPL//'.ICLP"', ERRHND)
      IF(PRJTYP .EQ. 1) THEN
        CALL PSNREA(1.0, 1, 'WIND"', ERRHND)
      ENDIF
      IF(PRJTYP .EQ. 3) THEN
        CALL PSNREA(1.0, 1, 'FOVU"', ERRHND)
      ENDIF
      CALL PSNV3D(VUPN, 1, 'LKAT"', ERRHND)
C
CCCC      CALL PPURGE(ERRHND)
C
      RETURN
      END
