C DEC/CMS REPLACEMENT HISTORY, Element GZMGEO.FOR
C *1    16-MAY-1987 17:17:08 KUNORI ""
C DEC/CMS REPLACEMENT HISTORY, Element GZMGEO.FOR
      INTEGER FUNCTION GZMGEO (NMOD)
C----------------------------------------------------------------------------
C
C Function to return pointer to MGEO geometry bank for module NMOD
C
C D.M.Kaplan   29-Dec-86
C S.Kunori     01-Apr-87    modified.
C----------------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'        
      INCLUDE 'D0$LINKS:IZSTPC.LINK/LIST'
      INCLUDE 'D0$LINKS:IZSMUO.LINK/LIST'
      INCLUDE 'D0$LINKS:IZMGEH.LINK/LIST'
      INTEGER LSTPC
C
      INTEGER NMOD
C
      GZMGEO=0                
C -- go through chain of banks in tree structure... (temporary)
      LSTPC=LC(LSTPH-IZSTPC)
      IF(LSTPC.EQ.0) GO TO 900
      LSMUO=LC(LSTPC-IZSMUO)
      IF(LSMUO.EQ.0) GO TO 900
      LMGEH=LC(LSMUO-IZMGEH)
C -- check if MGEH bank exists... 
      IF(LMGEH.NE.0) THEN
C     -- check the number of structual links...
         IF(IC(LMGEH-2).GE.NMOD) THEN
             GZMGEO = LC (LMGEH-NMOD)
        ENDIF
      ENDIF
C                             
900   CONTINUE
      RETURN
      END
