C DEC/CMS REPLACEMENT HISTORY, Element GZMMAG.FOR
C *1    16-MAY-1987 17:17:22 KUNORI ""
C DEC/CMS REPLACEMENT HISTORY, Element GZMMAG.FOR
      INTEGER FUNCTION GZMMAG(NMOD)
C----------------------------------------------------------------
C     Functuion to retrun pointer to MMAG bank for a iron slub 
C  number, NMOD. 
C
C  Input:
C     NMOD      iron slub number.
C
C  Output:
C     GZMMAG    bank pointer to MMAG bank.
C
C  S.Kunori     30-Apr-1987
C---------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'        
      INCLUDE 'D0$LINKS:IZSTPC.LINK/LIST'
      INCLUDE 'D0$LINKS:IZSMUO.LINK/LIST'
      INCLUDE 'D0$LINKS:IZMMAH.LINK/LIST'
      INCLUDE 'D0$LINKS:IZMMAG.LINK/LIST'
      INTEGER LSTPC
C
      INTEGER NMOD
      INTEGER LMMAG,LMMAH
C
      GZMMAG=0                
C -- go through chain of banks in tree structure... (temporary)
      LSTPC=LC(LSTPH-IZSTPC)
      IF(LSTPC.EQ.0) GO TO 900
      LSMUO=LC(LSTPC-IZSMUO)
      IF(LSMUO.EQ.0) GO TO 900
      LMMAH=LC(LSMUO-IZMMAH)
C -- check if MMAH bank exists... 
      IF(LMMAH.NE.0) THEN
C     -- check the number of links...
         IF(IC(LMMAH-3).GE.IZMMAG) THEN
             LMMAG = LC (LMMAH-IZMMAG)
100          CONTINUE
                IF(LMMAG.EQ.0) GO TO 900
                IF(IC(LMMAG+12).EQ.NMOD) THEN
                   GZMMAG=LMMAG
                   GO TO 900
                ENDIF
                LMMAG=LC(LMMAG)
             GO TO 100 
        ENDIF
      ENDIF
C                             
900   CONTINUE
      RETURN
      END
                              
