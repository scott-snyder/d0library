      INTEGERFUNCTION GZMDTH(I)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns pointer to muon delta time header
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  18-OCT-1988   Jim Green
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSMUO.LINK'
      INCLUDE 'D0$LINKS:IZMDTH.LINK'
C----------------------------------------------------------------------
      INTEGER KSTPC,KSMUO,KMDTH,I   
C----------------------------------------------------------------------
C
      GZMDTH=0
      KSTPC=LC(LSTPH-IZSTPC)
      IF(KSTPC.NE.0) THEN
        KSMUO=LC(KSTPC-IZSMUO)
        IF(KSMUO.NE.0) THEN
          GZMDTH=LC(KSMUO-IZMDTH)
        ENDIF
      ENDIF
C
  999 RETURN
      END
