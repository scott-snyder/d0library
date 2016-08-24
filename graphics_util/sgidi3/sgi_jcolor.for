      SUBROUTINE JCOLOR(ICC)
C  SET THE CURRENT COLOR PRIMITIVE ATTRIBUTE.
C
C  DEFAULT COLOR INDICES COMMON TO DI3000 AND GL:
C    1-RED, 2-GREEN, 3-YELLOW, 4-BLUE, 5-MAGENTA, 6-CYAN, 7-WHITE
C  DIFFERING CONVENTIONS:
C    FOR DI3000 0-NORMAL (USUALLY WHITE)
C               8-BLACK
C               9-COMPLEMENT (USUALLY BLACK)
C    FOR GL     0-BLACK
C               8,9-UNDEFINED INITIALLY
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      IC=ICC
      ICOLOR=IC
      IF(IC.EQ.CUR_COLOR)RETURN
      CUR_COLOR=IC
      IF(IC.EQ.0) IC=7
      IF(IC.EQ.8.OR.IC.EQ.9) IC=0
      IF(HCPY) THEN
        IF(IDEBUG.GE.20) TYPE *,' Calling DEV_COLOR:',IC
        CALL DEV_COLOR(IC)
      ELSE
        IF(IDEBUG.GE.20) TYPE *,' Calling COLOR:',IC
        CALL COLOR(IC)
      ENDIF
      END