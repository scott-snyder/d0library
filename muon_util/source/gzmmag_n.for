      INTEGER FUNCTION GZMMAG_N(NMOD) 
C----------------------------------------------------------------
C     Functuion to retrun pointer to MMAG bank for a iron slub 
C  number, NMOD. For Bank Hanging Under STPN
C
C  Input:
C     NMOD      iron slub number.
C
C  Output:
C     GZMMAG_N    bank pointer to MMAG bank.
C
C J.GREEN  APRIL 89  MODIFICATION OF GZMMAG
C---------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'        
      INCLUDE 'D0$LINKS:IZSTPN.LINK/LIST'
      INCLUDE 'D0$LINKS:IZSMUO.LINK/LIST'
      INCLUDE 'D0$LINKS:IZMMAH.LINK/LIST'
      INCLUDE 'D0$LINKS:IZMMAG.LINK/LIST'
      INTEGER LSTPN
C
      INTEGER NMOD
      INTEGER LMMAG,LMMAH
C
      GZMMAG_N=0                
C -- go through chain of banks in tree structure... (temporary)
      LSTPN=LC(LSTPH-IZSTPN)
      IF(LSTPN.EQ.0) GO TO 900
      LSMUO=LC(LSTPN-IZSMUO)
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
                   GZMMAG_N=LMMAG
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
                              
