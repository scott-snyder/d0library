C DEC/CMS REPLACEMENT HISTORY, Element GZMGEH.FOR
C *1    16-MAY-1987 17:16:39 KUNORI ""
C DEC/CMS REPLACEMENT HISTORY, Element GZMGEH.FOR
      INTEGER FUNCTION GZMGEH(IDUMMY)
C----------------------------------------------------------------
C     Function returns pointer to MGEH bank.   Input argument
C IDUMMY is a dummy.
C
C  Input:    (None)
C
C  Output:   GZMGEH -- pointer to MGEH bank.
C
C  S.Kunori  7-May-87
C----------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'        
      INCLUDE 'D0$LINKS:IZSTPC.LINK/LIST'
      INCLUDE 'D0$LINKS:IZSMUO.LINK/LIST'
      INCLUDE 'D0$LINKS:IZMGEH.LINK/LIST'
      INTEGER LSTPC
C
      INTEGER IDUMMY
C
      GZMGEH=0                
C -- go through chain of banks in tree structure... (temporary)
      LSTPC=LC(LSTPH-IZSTPC)
         IF(LSTPC.EQ.0) GO TO 900
      LSMUO=LC(LSTPC-IZSMUO)
         IF(LSMUO.EQ.0) GO TO 900
      GZMGEH=LC(LSMUO-IZMGEH)
C
900   CONTINUE
      RETURN
      END                
