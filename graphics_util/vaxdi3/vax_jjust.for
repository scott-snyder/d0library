      SUBROUTINE JJUST(IHORIZ,IVERT)
      INCLUDE 'D0$INC:DI3INC.INC'
      IHJUST=IHORIZ
      IVJUST=IVERT
      IF(.NOT.PUTS)RETURN
      CALL J_PUTSG(IJJUST,IHORIZ)
      CALL J_PUTSG(-IJJUST,IVERT)
      END
