       SUBROUTINE KVWTR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  To calculate and introduce viewing parameters.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  01-MAR-1989   SHAHRIAR ABACHI, A. VIRGO
C-   UPDATED :
C-  21-JUN-90  S. ABACHI   Introduced KVIEWP subroutine to calculate parametrs.
C-  27-MAR-89  S. ABACHI   Correct formula for the UP vector was devised.
C-  13-MAR-89  S. ABACHI   Now unnecessary XMIN,XMAX,YMIN,YMAX removed.
C-  12-MAR-89  S. ABACHI   Parrarell oblique and perspective oblique views
C-                         corrected.
C-  11-MAR-89  S. ABACHI   Third components of VUPNT and NORML were corrected
C-                         in JCVIEW and JSVIEW and were removed from here.
C-  10-MAR-89  S. ABACHI   VIEW-PORT operation was correctly applied to all
C-                         projection types.
C-  09-MAR-89  S. ABACHI   Added ZMIN, ZMAX, and Persp. view angle corrected.
C-  08-MAR-89  S. ABACHI   Correct Z windows were replaced
C-  07-MAR-89  S. ABACHI   Formula for the eye point coordinates corrected.
C-  06-MAR-89  S. ABACHI   XMIN, XMAX, YMIN, YMAX corrected.
C-  03-MAR-89  S. ABACHI   Depth clipping parameters were corrected.
C-   Updated  24-MAR-2004   sss - compile with g77.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:LINATT.INC/LIST'
      INCLUDE 'D0$INC:NEWDI3.INC/LIST'
      INCLUDE 'D0$INC:pi.def'
      REAL VUPN(3), EYEP(3), UPV(3), WIDEV, HCLP, YCLP
      LOGICAL CLIP
      REAL D0, D1
      REAL ZMIN, ZMAX
      REAL NORM(3), DIST
      INTEGER I
      EXTERNAL ERRHND
C
      CALL KVIEWP(VUPN, EYEP, UPV, WIDEV, HCLP, YCLP, CLIP)
C
      CALL PVIEWP('"', UVIEW(1), UVIEW(2), UVIEW(3), UVIEW(4),
     +               UVIEW(5), UVIEW(6), '"', ERRHND)
C
      GOTO (1,2,3,4), PRJTYP
C
    1 CONTINUE
C
C   2D and 3D orthographic (parallel) view.
C
      CALL PWINDO('IF"', UWIND(1), UWIND(2), UWIND(3), UWIND(4),
     +               HCLP, YCLP, '"', ERRHND)
      CALL PLOOKA('IL"', VUPN, EYEP, UPV, '"', ERRHND)
C
      GOTO 9900
C
    2 CONTINUE
C
C    3D parallel oblique.
C
      CALL PEYEBK('"', PAROBZ, PAROBX, PAROBY, WIDEV,
     +               HCLP, YCLP, '"', ERRHND)
      CALL PLOOKA('"', VUPN, EYEP, UPV, '"', ERRHND)
      GOTO 9900
C
    3 CONTINUE
C
C   3D Perspective orthographic view.
C
      D1 = 2.0 * ATAN( WIDEV / (2.0 * PERSP) )*180/pi
      CALL PFOV('"', D1, HCLP, YCLP, '"', ERRHND)
      CALL PLOOKA('"', VUPN, EYEP, UPV, '"', ERRHND)
      GOTO 9900
C
    4 CONTINUE
C
C    3D Perspective oblique.
C
      CALL PEYEBK('"', PEROBZ, PEROBX, PEROBY, WIDEV,
     +               HCLP, YCLP, '"', ERRHND)
      CALL PLOOKA('"', VUPN, EYEP, UPV, '"', ERRHND)
      GOTO 9900
C
 9900 CONTINUE
C
      CALL PSNBOO(CLIP, 1, DISPL//'.ICLP"', ERRHND)
C
      RETURN
      END
