C DEC/CMS REPLACEMENT HISTORY, Element GZMMAP.FOR
C *1    16-MAY-1987 17:17:54 KUNORI ""
C DEC/CMS REPLACEMENT HISTORY, Element GZMMAP.FOR
      INTEGER FUNCTION GZMMAP(NMAP)
C----------------------------------------------------------------
C     Functuion to retrun pointer to MMAP bank for a field map
C  number, NMAP. 
C
C  Input:
C     NMAP      magnetic field map number.
C
C  Output:
C     GZMMAP    bank pointer to MMAP bank.
C
C  S.Kunori     30-Apr-1987
C---------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'        
      INCLUDE 'D0$LINKS:IZSTPC.LINK/LIST'
      INCLUDE 'D0$LINKS:IZSMUO.LINK/LIST'
      INCLUDE 'D0$LINKS:IZMMAH.LINK/LIST'
      INCLUDE 'D0$LINKS:IZMMAP.LINK/LIST'
      INTEGER LSTPC
C
      INTEGER NMAP
      INTEGER LMMAP,LMMAH
C
      GZMMAP=0                
C -- go through chain of banks in tree structure... (temporary)
      LSTPC=LC(LSTPH-IZSTPC)
      IF(LSTPC.EQ.0) GO TO 900
      LSMUO=LC(LSTPC-IZSMUO)
      IF(LSMUO.EQ.0) GO TO 900
      LMMAH=LC(LSMUO-IZMMAH)
C -- check if MMAH bank exists... 
      IF(LMMAH.NE.0) THEN
C     -- check the number of links...
         IF(IC(LMMAH-3).GE.IZMMAP) THEN
             LMMAP = LC (LMMAH-IZMMAP)
100          CONTINUE
                IF(LMMAP.EQ.0) GO TO 900
                IF(IC(LMMAP+9).EQ.NMAP) THEN
                   GZMMAP=LMMAP
                   GO TO 900
                ENDIF
                LMMAP=LC(LMMAP)
             GO TO 100 
        ENDIF
      ENDIF
C                             
900   CONTINUE
      RETURN
      END
