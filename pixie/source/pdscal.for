      SUBROUTINE PDSCAL(SCALX,SCALY,SCALSZ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : draw a scale
C-
C-   Inputs  : SCALX, SCALY: the x and y position for the left edge 
C-                           of the scale
C-             SCALSZ: scale length
C-   Outputs : none
C-
C-   Created  15-MAY-1990   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER SCALSZ
      REAL    SCALX, SCALY, SCALS1, HEIGHT, PRCENT, YXRATI
      PARAMETER( HEIGHT = 2.0 )
      CHARACTER*8 TEXT
C----------------------------------------------------------------------
C
C   set character size
C
      PRCENT = 1.0
      YXRATI = 2.0
C
C   draw a scale
C
      SCALY = SCALY - HEIGHT
      CALL JMOVE(SCALX,SCALY)
      SCALY = SCALY + 2 * HEIGHT
      CALL JDRAW(SCALX,SCALY)
      SCALY = SCALY - HEIGHT
      CALL JMOVE(SCALX, SCALY)
      SCALS1 = FLOAT(SCALSZ)
      SCALX = SCALX + SCALS1
      CALL JDRAW(SCALX,SCALY)
      SCALY = SCALY + HEIGHT
      CALL JMOVE(SCALX,SCALY)
      SCALY = SCALY - 2 * HEIGHT
      CALL JDRAW(SCALX,SCALY)
      SCALY = SCALY - HEIGHT
      WRITE(TEXT,1001) SCALSZ
 1001 FORMAT (I2,' cm')
      CALL PUVSTR(SCALX,SCALY,PRCENT, YXRATI, TEXT)
C        CALL JMOVE(SCALX,SCALY)
C        CALL J1STRG(TEXT)
      SCALX = SCALX - SCALS1/2.
      WRITE(TEXT,1002) SCALSZ/2
 1002 FORMAT(I2)
      CALL PUVSTR(SCALX,SCALY,PRCENT, YXRATI, TEXT)
C        CALL JMOVE(SCALX,SCALY)
C        CALL J1STRG(TEXT)
      SCALY = SCALY + HEIGHT
      CALL JMOVE(SCALX,SCALY)
      SCALY = SCALY + 2. * HEIGHT
      CALL JDRAW(SCALX,SCALY)
      SCALX = SCALX - SCALS1/2.
      SCALY = SCALY - 3. * HEIGHT
      WRITE(TEXT,1003) 
 1003 FORMAT('0')
      CALL PUVSTR(SCALX,SCALY,PRCENT, YXRATI, TEXT)
C        CALL JMOVE(SCALX,SCALY)
C        CALL J1STRG(TEXT)
C
  999 RETURN
      END
