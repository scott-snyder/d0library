      SUBROUTINE CL2_GET_SFTVSN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get SFTVSN and D0VSN from CAGS bank and alert the low
C-      level unpacking utilities used in table building
C-        IF NO CAGS BANK, ASSUME THE CURRENT REAL DATA FORMAT
C-
C-   Inputs  : CAGS bank: SFTVSN and D0VSN words
C-   Outputs : these, unpacked into /CUNFLG/
C-   Controls:
C-
C-   Created   3-MAR-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZ2CAGS.LINK'
      INCLUDE 'D0$INC:CUNFLG.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER GZCAGS,GZSL2H,LSL2H,LCAGS,IWORD
      INTEGER*2 SHORT(2)
      BYTE BYTES(4)
      EQUIVALENCE (IWORD,SHORT), (IWORD,BYTES)
C----------------------------------------------------------------------
      LSL2H = GZSL2H()
      IF (LSL2H.GT.0) THEN
        LCAGS = LC(LSL2H-IZ2CAGS)
        IF (LCAGS.GT.0) THEN
          IWORD = IC(LCAGS+9)
          SFTVSN = SHORT(1)
          CALVSN = BYTES(3)
          D0VSN = BYTES(4)
        ENDIF
      ENDIF
  999 RETURN
      END
