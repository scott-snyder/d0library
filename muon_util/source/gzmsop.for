C DEC/CMS REPLACEMENT HISTORY, Element GZMSOP.FOR
C *1     6-DEC-1989 00:05:29 HARRISON "Rajendran Raja: zbank utility"
C DEC/CMS REPLACEMENT HISTORY, Element GZMSOP.FOR
      INTEGER FUNCTION GZMSOP(NMOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the POINTER to MSOP bank
C-                         for module NMOD
C-
C-   Returned value  : Link to 1st element of MSOP linear structure
C-   Inputs  :    NMOD    module number
C-   Outputs :    GZMSOP  pointer for MSOP bank of module NMOD
C-   Controls:
C-
C-   Created  27-june-90     S.T.Repond
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'        
      INCLUDE 'D0$LINKS:IZSTPC.LINK/LIST'
      INCLUDE 'D0$LINKS:IZSMUO.LINK/LIST'
      INCLUDE 'D0$LINKS:IZMGEH.LINK/LIST'
      INCLUDE 'D0$LINKS:IZMSRH.LINK/LIST'
      INTEGER LSTPC,LMSRH
C
      INTEGER NMOD
C
      GZMSOP=0                
C -- go through chain of banks in tree structure... (temporary)
      LSTPC=LC(LSTPH-IZSTPC)
      IF(LSTPC.EQ.0) GO TO 900
      LSMUO=LC(LSTPC-IZSMUO)
      IF(LSMUO.EQ.0) GO TO 900
      LMSRH=LC(LSMUO-IZMSRH)
C -- check if MSRH bank exists... 
      IF(LMSRH.NE.0) THEN
C     -- check the number of structual links...
         IF(IC(LMSRH-2).GE.NMOD) THEN
             GZMSOP = LC (LMSRH-NMOD)
        ENDIF
      ENDIF
C                             
900   CONTINUE
      RETURN
      END
