      SUBROUTINE JRMARK(X,Y)
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      XNOW=X+XPOSN
      YNOW=Y+YPOSN
      PUTSUP=.TRUE.
      CALL JMARK(XNOW,YNOW)
      PUTSUP=.FALSE.
      IF(.NOT.PUTS)RETURN
      CALL J_PUTSG(IRMARK,X)
      CALL J_PUTSG(-IRMARK,Y)
      END
