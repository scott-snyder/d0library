      SUBROUTINE CLUSTER_CONNECT(I,J)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CONNECTS CATE TOWERS I AND J
C-
C-   Inputs  : I,J CATE TOWER NUMBERS
C-   Outputs : NONE
C-
C-   Controls:
C-
C-   Created :  28-JUL-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:CATENM.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:CLUPAR.INC'
      INTEGER I,J,K
      INTEGER CLASS,NEXT,CATW
      INTEGER IHADR
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C ****  defining statement functions class,next
C
      CATW(J) = (J-1)*NREP+LCATE
C
C NREP,LCATE are the rep. num + addr of the CATE bank and will be
C defined later.
C
      CLASS(J) = CATW(J)+ICLASS
C
C gives the class number address of  tower J
C
      NEXT(J) = CATW(J)+INEXT
C
C gives the connection to tower J
C
C ****  statement function definitions complete
C
C------------------------------------------------------------------------
C
C ****  connect I AND J.
C
      IF(IQ(CLASS(I)).NE.IQ(CLASS(J)))THEN
        K = J
        DO WHILE (IQ(NEXT(K)).NE.J)
          IQ(CLASS(K)) = IQ(CLASS(I))
          K = IQ(NEXT(K))
        ENDDO
        IQ(CLASS(K)) = IQ(CLASS(I))
        IQ(NEXT(K)) = IQ(NEXT(I))
        IQ(NEXT(I)) = J
      ENDIF
  999 RETURN
      END
