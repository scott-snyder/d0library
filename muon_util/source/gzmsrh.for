C DEC/CMS REPLACEMENT HISTORY, Element GZMSRH.FOR
C *1    02-AUG-1990        SILVIA "FROM ZEBRA_UTIL"
      INTEGER FUNCTION GZMSRH(IDUMMY)
C----------------------------------------------------------------
C     Function returns pointer to MSRH bank.   Input argument
C IDUMMY is a dummy.
C
C  Input:    (None)
C
C  Output:   GZMSRH -- pointer to MSRH bank.
C
C  S.T.Repond   02-Aug-1990
C----------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'        
      INCLUDE 'D0$LINKS:IZSTPC.LINK/LIST'
      INCLUDE 'D0$LINKS:IZSMUO.LINK/LIST'
      INCLUDE 'D0$LINKS:IZMGEH.LINK/LIST'
      INCLUDE 'D0$LINKS:IZMSRH.LINK/LIST'
      INTEGER LSTPC
C
      INTEGER IDUMMY
C
      GZMSRH=0                
C -- go through chain of banks in tree structure... (temporary)
      LSTPC=LC(LSTPH-IZSTPC)
         IF(LSTPC.EQ.0) GO TO 900
      LSMUO=LC(LSTPC-IZSMUO)
         IF(LSMUO.EQ.0) GO TO 900
      GZMSRH=LC(LSMUO-IZMSRH)
C
900   CONTINUE
      RETURN
      END                
