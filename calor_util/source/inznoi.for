      SUBROUTINE INZNOI
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Initialize /ZEBNOI/, working area not written out
C-
C-   Created  13-JAN-1991   ALLEN I. MINCER
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBNOI.INC'
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        CALL MZSTOR (IXNOI,'/ZEBNOI/','Q',FENNOI,LNOIH,LRNOI,ZNOISY(1),
     &   ZNOISY(10000),ENDZN)
        IDVNOI=IXNOI+2
        FIRST=.FALSE.
      ENDIF
  999 RETURN
      END
