      SUBROUTINE PZ_VERTEX_3D
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw primary verties in 3-D
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  13-JUN-1992   Qizhong Li-Demarteau
C-   Updated  25-NOV-1992   Nobuaki Oshima
C-      Use EZPICK to select correct RCP Bank.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER DRAW_VERTEX,IER
      INTEGER LVERH, GZVERH, LVERT, NVERT
      REAL    XPOS, YPOS, ZPOS, SIZE
      PARAMETER( SIZE = 3. )
      LOGICAL EZERROR
C----------------------------------------------------------------------
C
C-
C--- Select correct RCP bank
      CALL EZPICK('PX_ZTRAKSDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PCSVEN',
     &    'Unable to pick RCP bank PX_ZTRAKSDIS_RCP','W')
        GOTO 999
      ENDIF
C-
      CALL PUGETV('ZTRAKS DRAW VERTEX',DRAW_VERTEX)
      CALL EZRSET
      IF (DRAW_VERTEX .LE. 0) GOTO 999
C
      NVERT = 0
      LVERH = GZVERH()
      IF (LVERH .LE. 0) GOTO 999
      CALL PUOPEN
      CALL PXCOLR('RED')
      LVERT = LQ(LVERH - 1)
  100 IF (LVERT .LE. 0) GOTO 900
      XPOS = Q(LVERT+3)
      YPOS = Q(LVERT+4)
      ZPOS = Q(LVERT+5)
      CALL PZDRAW_3DHIT(XPOS,YPOS,ZPOS,SIZE)
      NVERT = NVERT + 1
      IF (DRAW_VERTEX .LE. NVERT) GOTO 900
      LVERT = LQ(LVERT)
      GOTO 100
  900 CALL PUCLOSE
C
  999 RETURN
      END
