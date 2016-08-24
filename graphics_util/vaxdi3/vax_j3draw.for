      SUBROUTINE J3DRAW(X,Y,Z)
C
      REAL*4 X,Y,Z,XOLD,YOLD,ZOLD,XNEW,YNEW,ZNEW
      REAL*4 XOL,YOL,ZOL,XNE,YNE,ZNE
      INCLUDE 'D0$INC:DI3INC.INC'
C
      CALL J_TR3XYZ(XPOSN,YPOSN,ZPOSN,XOLD,YOLD,ZOLD)
      CALL J_TR3XYZ(X,Y,Z,XNEW,YNEW,ZNEW)
      CALL J_LINE2(VD_ID,ATB,XOLD,YOLD,XNEW,YNEW)
      XPOSN=X
      YPOSN=Y
      ZPOSN=Z
      IF(IDEBUG.GT.5) write (*,888),X,Y,Z,XOLD,YOLD,ZOLD,XNEW,YNEW,ZNEW
C&IF LINUX
C&  888 FORMAT(' J3DRAW:',9F6.2)
C&ELSE
  888 FORMAT(' J3DRAW:',9F)
C&ENDIF
      IF(.NOT.PUTS)RETURN
      IF(PUTSUP)RETURN
      CALL J_PUTSG(I3DRAW,X)
      CALL J_PUTSG(I3DRAW,Y)
      CALL J_PUTSG(-I3DRAW,Z)
      IF(IPICID.NE.0) CALL J_PIKDO(X,Y,Z)
      END