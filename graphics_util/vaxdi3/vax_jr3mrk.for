      SUBROUTINE JR3MRK(X,Y,Z)
      INCLUDE 'D0$INC:DI3INC.INC'
      XNOW=X+XPOSN
      YNOW=Y+YPOSN
      ZPOSN=ZPOSN+Z
      PUTSUP=.TRUE.
      CALL JMARK(XNOW,YNOW)
      PUTSUP=.FALSE.
      IF(.NOT.PUTS)RETURN
      CALL J_PUTSG(IR3MRK,X)
      CALL J_PUTSG(IR3MRK,Y)
      CALL J_PUTSG(-IR3MRK,Z)
      IF(IPICID.NE.0) CALL J_PIKDO(XNOW,YNOW,ZPOSN)
      END
