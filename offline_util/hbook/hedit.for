C DEC/CMS REPLACEMENT HISTORY, Element HEDIT.FOR
C *2    21-SEP-1987 12:22:23 RAJA ""
C *1    21-SEP-1987 12:16:16 RAJA ""
C DEC/CMS REPLACEMENT HISTORY, Element HEDIT.FOR
*VAXFOR HEDIT
      SUBROUTINE HEDIT(B,N1,M,KABEL)
C
C     ******************************************************************
C     *                                                                *
C     *       PRINTS CONTENTS OF N1 LOCATIONS STARTING AT ADDRESS M    *
C     *       IN //                                                    *
C     *                                                                *
C     ******************************************************************
C
      COMMON/HTAPIO/IOUT,IERMES,LDISC
C
      COMMON/HFLAG /ID    ,IDBADD,LID   ,IDLAST,IDHOLD,NBIT  ,NBITCH,
     +       NCHAR ,NX0   ,NX1   ,NX2   ,INTER ,INDEX ,LAST  ,LIMIT ,
     +       LFIEL ,NEWHIS,NRLENG,NWLIB ,NWFLAG,NBFLAG,NWSTAT,NRHIST,
     +       IDISC ,LFHIST,LLHIST,NWHIST,IERR  ,NV    ,NRDIS ,IA2
C
      COMMON/HCTIT/LTIT(40)
C
      COMMON/HFORM/IA(127),IDU
      DIMENSION KABEL(1),B(1)
      EQUIVALENCE (IB1,BB1),(IB2,BB2)
C
C     ------------------------------------------------------------------
C
      J=M
      CALL HFORMA(2)
C
      CALL UCTOH(KABEL,LTIT,10,20)
      CALL HBTIT(LTIT,ITLAST,NWTI,IT1)
C
      IF(NWTI.EQ.0)GO TO 10
      K=NCHAR*(NWTI-1)
      IF(K.GT.118)K=118
      CALL VBLANK(IA,128)
      CALL UBLOW(LTIT,IA(11),K)
      CALL UBLOW(ITLAST,IA(K+11),NCHAR)
      CALL HFORMA(1)
C
   10 CONTINUE
      DO 20 K=1,N1,2
        J=M+K-1
        BB1=B(K)
        BB2=B(K+1)
        WRITE(IOUT,1000)J,IB1,BB1,IB1,IB1,IB2,BB2,IB2,IB2
   20 CONTINUE
C
 1000 FORMAT(I12,2(5X,I12,2X,E12.4,3X,Z8,2X,A4))
C
      RETURN
      END
