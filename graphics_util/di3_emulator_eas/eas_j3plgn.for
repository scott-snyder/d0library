      SUBROUTINE J3PLGN(XVRT, YVRT, ZVRT, NVRT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
CD   This modules generates a 3-D polygon with NVRT vertices. The 
CD   parameters XVRT, YVRT, and ZVRT are real one dimensional arrays 
CD   that contain the vertices of the polygon.
C-
C-   Inputs  : XVRT, YVRT, ZVRT, NVRT   List and number of vertices
C-   Outputs : 
C-   Controls: 
C-
C-   Created  15-NOV-1988   A. VIRGO
C-   UPDATED  10-JUL-1990   S. ABACHI     Real polgons were used instead of
C-                                        poly-lines
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER NVRT
      REAL XVRT(NVRT), YVRT(NVRT), ZVRT(NVRT)
      INTEGER NVERP(1000)
C
      INTEGER I
      REAL    LINTEN
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:PRIMVR.INC/LIST'
      INCLUDE 'D0$INC:LINATT.INC/LIST'
      INCLUDE 'D0$INC:PLGATT.INC/LIST'
C
      IF (.NOT. SEGOPN) THEN
         CALL ERROR('J3PLGN: NO SEGMENT IS OPEN')
      ENDIF
      IF (NVRT .LT. 3) THEN
         CALL ERROR('J3PLGN: NUMBER OF VERTICES < 3')
      ENDIF
      LINTEN = MAX(MINTEN, FLOAT(CINTEN) / 32767.0)
      DO 10 I=1,NVRT
         VERTIC(1,I) = XVRT(I)
         VERTIC(2,I) = YVRT(I)
         VERTIC(3,I) = ZVRT(I) * RIGHT
         VERTIC(4,I) = LINTEN
   10 CONTINUE
      NVERT = NVRT + 1
      VERTIC(1,NVERT) = XVRT(1)
      VERTIC(2,NVERT) = YVRT(1)
      VERTIC(3,NVERT) = ZVRT(1) * RIGHT
      VERTIC(4,NVERT) = LINTEN
      NVERT = NVRT                      !SA
      NVERP(1) = NVRT
      CALL KPLGN(1,NVERP)
      CPX = XVRT(1)
      CPY = YVRT(1)
      CPZ = ZVRT(1) * RIGHT
      RETURN
      END
