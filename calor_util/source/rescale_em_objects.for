      SUBROUTINE RESCALE_EM_OBJECTS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CHANGE THE CASH AND PELC,PPHO BANKS TO REFLECT THE
C-                        PHI UNIFORMITY CALIBRATION
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  14-NOV-1995   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INTEGER GZPELC,GZPPHO
      INTEGER GZVERT
      REAL    VERT(3)
      INTEGER LVERT
C----------------------------------------------------------------------
      LVERT = GZVERT(1) !PRIMARY VERTEX
C
      CALL UCOPY(Q(LVERT+3),VERT,3)
C
      LPELC = GZPELC()
      DO WHILE (LPELC.GT.0)
        CALL RESCALE_EM(LPELC,VERT,3)
        LPELC = LQ(LPELC)
      ENDDO
C
      LPPHO = GZPPHO()
      DO WHILE (LPPHO.GT.0)
        CALL RESCALE_EM(LPPHO,VERT,3)
        LPPHO = LQ(LPPHO)
      ENDDO
C
  999 RETURN
      END
