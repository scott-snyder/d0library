      SUBROUTINE FGETVERT(ZVERT,IVERT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determine which vertex ZVERT belongs to.
C-
C-   Inputs  : ZVERT = Vertex Z location
C-   Outputs : IVERT = vertex number
C-   Controls:
C-
C-   Created  22-APR-1993   Susan K. Blessing
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER I
      INTEGER IVERT
      INTEGER VCONT(10),NVERT
C
      REAL ZVERT
      REAL DIFF
      REAL VINFO(18)
C
C----------------------------------------------------------------------
C
      IVERT = 0
      DIFF = 99999.
C
      CALL GTVERH(VCONT)
      NVERT = VCONT(2)
C
      DO I = 1, NVERT
        CALL GTVERT(I,VINFO)
        IF (ABS(ZVERT - VINFO(5)).LT. DIFF) THEN
          DIFF = ABS(ZVERT - VINFO(5))
          IVERT = I
        ENDIF
      END DO
C
  999 RETURN
      END
