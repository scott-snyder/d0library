      SUBROUTINE JPOLGN(XVRT, YVRT, NVRT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
CD   This modules generates a 2-D polygon with NVRT vertices. The 
CD   parameters XVRT and YVRT are real one dimensional arrays 
CD   that contain the vertices of the polygon.
C-
C-   Inputs  : XVRT, YVRT, NVRT
C-   Outputs : 
C-   Controls: 
C-
C-   Created  15-NOV-1988   A. VIRGO
C-   UPDATED  10-JUL-1990   S. ABACHI    Real polygons instead of poly-lines
C-                                       used. (KPLGN completely modified).
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NVRT
      REAL XVRT(NVRT), YVRT(NVRT)
      INTEGER I, NVERP(1000)
      REAL    LINTEN
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:PRIMVR.INC/LIST'
      INCLUDE 'D0$INC:LINATT.INC/LIST'
      INCLUDE 'D0$INC:PLGATT.INC/LIST'
C
      IF (.NOT. SEGOPN) THEN
         CALL ERROR('JPOLGN: NO SEGMENT IS OPEN')
      ENDIF
      IF (NVRT .LT. 3) THEN
         CALL ERROR('JPOLGN: NUMBER OF VERTICES < 3')
      ENDIF
      LINTEN = MAX(MINTEN, FLOAT(CINTEN) / 32767.0)
      DO 10 I=1,NVRT
         VERTIC(1,I) = XVRT(I)
         VERTIC(2,I) = YVRT(I)
         VERTIC(3,I) = CPZ
         VERTIC(4,I) = LINTEN
   10 CONTINUE
C
      NVERT = NVRT + 1
      VERTIC(1,NVERT) = XVRT(1)
      VERTIC(2,NVERT) = YVRT(1)
      VERTIC(3,NVERT) = CPZ
      VERTIC(4,NVERT) = LINTEN
      NVERT = NVRT       !SA
      NVERP(1) = NVRT
      CALL KPLGN(1,NVERP)
      CPX = XVRT(1)
      CPY = YVRT(1)
      RETURN
      END
