      INTEGER FUNCTION GZMSTC(NMOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get pointer to MSTP bank for module NMOD
C-
C-   Returned value  : ZEBRA pointer in ZEBSTP to MSTC
C-   Inputs  : module number
C-   Outputs :
C-   Controls:
C-
C-   Created  21-JUL-1993   J.Green
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:MU_ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSMUO.LINK'
      INCLUDE 'D0$LINKS:IZMSTH.LINK'
      INTEGER KSTPC,KSMUO,KMSTH,NMOD
C----------------------------------------------------------------------
      GZMSTC=0
      IF (LSTPH .NE. 0) THEN
        KSTPC=LC(LSTPH-IZSTPC)
        IF(KSTPC.NE.0) THEN
          KSMUO=LC(KSTPC-IZSMUO)
          IF(KSMUO.NE.0) THEN
            KMSTH=LC(KSMUO-IZMSTH)
            IF(KMSTH.NE.0) THEN
              GZMSTC=LC(KMSTH-NMOD)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
  999 RETURN
      END
