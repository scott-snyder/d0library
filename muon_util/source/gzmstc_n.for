      INTEGER FUNCTION GZMSTC_N(NMOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get pointer to MSTP bank for module NMOD under STPN
C-
C-   Returned value  : ZEBRA pointer in ZEBSTP to MSTC under STPN
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
      INCLUDE 'D0$LINKS:IZSTPN.LINK'
      INCLUDE 'D0$LINKS:IZSMUO.LINK'
      INCLUDE 'D0$LINKS:IZMSTH.LINK'
      INTEGER KSTPN,KSMUO,KMSTH,NMOD
C----------------------------------------------------------------------
      GZMSTC_N=0
      IF (LSTPH .NE. 0) THEN
        KSTPN=LC(LSTPH-IZSTPN)
        IF(KSTPN.NE.0) THEN
          KSMUO=LC(KSTPN-IZSMUO)
          IF(KSMUO.NE.0) THEN
            KMSTH=LC(KSMUO-IZMSTH)
            IF(KMSTH.NE.0) THEN
              GZMSTC_N=LC(KMSTH-NMOD)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
  999 RETURN
      END
