      INTEGER FUNCTION GZMMAP_R(NMAP)
C----------------------------------------------------------------
C     Functuion to retrun pointer to MMAP bank for a field map
C  number, NMAP.  For bank hanging under STPO
C
C  Input:
C     NMAP      magnetic field map number.
C
C  Output:
C     GZMMAP_R    bank pointer to MMAP bank.
C
C  J.Green  April 89    modification of GZMMAP
C---------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'        
      INCLUDE 'D0$LINKS:IZSTPO.LINK/LIST'
      INCLUDE 'D0$LINKS:IZSMUO.LINK/LIST'
      INCLUDE 'D0$LINKS:IZMMAH.LINK/LIST'
      INCLUDE 'D0$LINKS:IZMMAP.LINK/LIST'
      INTEGER LSTPO
C
      INTEGER NMAP
      INTEGER LMMAP,LMMAH
C
      GZMMAP_R=0                
C -- go through chain of banks in tree structure... (temporary)
      LSTPO=LC(LSTPH-IZSTPO)
      IF(LSTPO.EQ.0) GO TO 900
      LSMUO=LC(LSTPO-IZSMUO)
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
                   GZMMAP_R=LMMAP
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
