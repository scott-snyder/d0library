      INTEGER FUNCTION GZZTMP(IZTMP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank ZTMP
C-
C-   Returned value  : pointer to Zebra bank ZTMP
C-
C-   Created  02-JAN-1992   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IZTMP, LZTMP
      INTEGER LZTRH, GZZTRH, LZFIND
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZZTMP.LINK'
C----------------------------------------------------------------------
      GZZTMP = 0
      LZTRH = GZZTRH()
      IF (LZTRH .GT. 0) THEN
        LZTMP = LQ(LZTRH - IZZTMP) 
        IF (LZTMP .GT. 0) THEN
          IF (IZTMP .GT. 0) THEN
            GZZTMP = LZFIND(IXCOM,LZTMP,IZTMP,-5)
          ELSE
            GZZTMP = LZTMP
          ENDIF
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
