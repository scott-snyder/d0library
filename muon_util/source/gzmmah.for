C DEC/CMS REPLACEMENT HISTORY, Element GZMMAH.FOR
C *1    16-MAY-1987 17:17:36 KUNORI ""
C DEC/CMS REPLACEMENT HISTORY, Element GZMMAH.FOR
      INTEGER FUNCTION GZMMAH(IDUMMY)
C----------------------------------------------------------------
C     Function returns pointer to MMAH bank.   Input argument
C IDUMMY is a dummy.
C
C  Input:    (None)
C
C  Output:   GZMMAH -- pointer to MMAH bank.
C
C  S.Kunori  7-May-87
C----------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'        
      INCLUDE 'D0$LINKS:IZSTPC.LINK/LIST'
      INCLUDE 'D0$LINKS:IZSMUO.LINK/LIST'
      INCLUDE 'D0$LINKS:IZMMAH.LINK/LIST'
      INTEGER LSTPC
C
      INTEGER IDUMMY
C
      GZMMAH=0                
C -- go through chain of banks in tree structure... (temporary)
      LSTPC=LC(LSTPH-IZSTPC)
         IF(LSTPC.EQ.0) GO TO 900
      LSMUO=LC(LSTPC-IZSMUO)
         IF(LSMUO.EQ.0) GO TO 900
      GZMMAH=LC(LSMUO-IZMMAH)
C
900   CONTINUE
      RETURN
      END
