      SUBROUTINE JLSTYL(LSTYL)
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      SAVE IFIRST
      ILSTYL=LSTYL
      LL=MOD(LSTYL,10)+1
      IF(IFIRST.EQ.1) GO TO 10
      CALL DEFLIN(1,'FFFF'X)
      CALL DEFLIN(2,'5252'X)
      CALL DEFLIN(3,'5959'X)
      CALL DEFLIN(4,'5F5F'X)
      CALL DEFLIN(5,'3333'X)
      CALL DEFLIN(6,'00FF'X)
      CALL DEFLIN(7,'6FFF'X)
      CALL DEFLIN(8,'5FFF'X)
      CALL DEFLIN(9,'0FFF'X)
      CALL DEFLIN(10,'FFFF'X)
      IFIRST=1
   10 IF(HCPY) THEN
        CALL DEV_SETLIN(LSTYL)
      ELSE
        CALL SETLIN(LL)
      ENDIF
      END
