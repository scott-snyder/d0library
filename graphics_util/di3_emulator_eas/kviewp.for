      SUBROUTINE KVIEWP(VUPN, EYEP, UPV, WIDEV, HCLP, YCLP, CLIP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To calculate the viewing parameters
C-
C-   Inputs  :NONE
C-   Outputs :   VUP,EYEP,UPV,WIDEV,HCLP,YCLP,CLIP
C-   Controls:NONE
C-
C-   Created   18-JUN-1990   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL VUPN(3), EYEP(3), UPV(3), WIDEV, HCLP, YCLP
      LOGICAL CLIP
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:LINATT.INC/LIST'
      REAL D0, D1, DN
      REAL ZMIN, ZMAX
      REAL NORM(3), DIST
      INTEGER I
      EXTERNAL ERRHND
C
C   Create the view scaling vector and new window using whole screen.
C
      SCV(1) = (UVIEW(2) - UVIEW(1)) / (UWIND(2) - UWIND(1))
      SCV(2) = (UVIEW(4) - UVIEW(3)) / (UWIND(4) - UWIND(3))
      SCV(3) = 2.0 / (UWIND(6) - UWIND(5))
C
      D0     = ABS(UWIND(2) - UWIND(1))
      D1     = ABS(UWIND(4) - UWIND(3))
      WIDEV  = AMAX1(D0, D1)
C
      DIST = 1.0E5
      IF(PRJTYP .EQ. 2) THEN
        DIST = ABS(PAROBX * NORML(1) + PAROBX * NORML(2) +
     &              PAROBX * NORML(3))
      ELSEIF (PRJTYP .EQ. 3) THEN
        DIST = PERSP
      ELSEIF(PRJTYP .EQ. 4) THEN
        DIST = ABS(PEROBX * NORML(1) + PEROBX * NORML(2) +
     &              PEROBX * NORML(3))
      ENDIF
C
      DN = SQRT(NORML(1)**2 + NORML(2)**2 + NORML(3)**2)
C
      DO 11 I=1,3
        EYEP(I) = VUPNT(I) - DIST * NORML(I) / DN
        UPV(I)  = UPVEC(I) + VUPNT(I)
        VUPN(I) = VUPNT(I)
        NORM(I) = NORML(I)
   11 CONTINUE
C
      EYEP(3) = EYEP(3) * RIGHT
      UPV(3)  = UPV(3) * RIGHT
      VUPN(3) = VUPN(3) * RIGHT
      NORM(3) = NORML(3) * RIGHT
C
      ZMIN   = ( DIST + UWIND(5) )
      ZMAX   = ( DIST + UWIND(6) )
      CLIP = .FALSE.
      HCLP = DIST
      YCLP = 2.E5
      IF (HCLIP) THEN
        CLIP = .TRUE.
        HCLP = ZMIN
      ENDIF
      IF (YCLIP) THEN
        CLIP = .TRUE.
        YCLP = ZMAX
      ENDIF
      IF(PRJTYP .NE. 1) THEN
        HCLP = AMAX1(1.0E-5, HCLP)
      ENDIF
C
  999 RETURN
      END
