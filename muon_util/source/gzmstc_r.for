      INTEGER FUNCTION GZMSTC_R(NMOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get pointer to MSTP bank for module NMOD under STPO
C-
C-   Returned value  : ZEBRA pointer in ZEBSTP to MSTC under STPO
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
      INCLUDE 'D0$LINKS:IZSTPO.LINK'
      INCLUDE 'D0$LINKS:IZSMUO.LINK'
      INCLUDE 'D0$LINKS:IZMSTH.LINK'
      INTEGER KSTPO,KSMUO,KMSTH,NMOD
C----------------------------------------------------------------------
      GZMSTC_R=0
      IF (LSTPH .NE. 0) THEN
        KSTPO=LC(LSTPH-IZSTPO)
        IF(KSTPO.NE.0) THEN
          KSMUO=LC(KSTPO-IZSMUO)
          IF(KSMUO.NE.0) THEN
            KMSTH=LC(KSMUO-IZMSTH)
            IF(KMSTH.NE.0) THEN
              GZMSTC_R=LC(KMSTH-NMOD)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
  999 RETURN
      END
