      SUBROUTINE BKZTMP(LZTMP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book ZTMP bank used by TRD analysis
C-
C-   Inputs  : none
C-   Outputs : LZTMP: ZTMP bank address
C-
C-   Created  20-NOV-1990   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZZTMP.LINK/LIST'
      INTEGER  GZZTRH,LZTRH,LZTMP
      INTEGER IXZTMP,MBOOKT(5),IBOOKT 
      LOGICAL FIRST 
      SAVE FIRST
      DATA IBOOKT/4HZTMP/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL MZFORM('ZTMP','1I 1B 3I 3F',IXZTMP)
        MBOOKT(1)=IBOOKT
        MBOOKT(2)=8      
        MBOOKT(3)=1
        MBOOKT(4)=8
        MBOOKT(5)=IXZTMP
        FIRST=.FALSE.
      ENDIF
      LZTRH = GZZTRH() 
      IF (LZTRH .EQ. 0) CALL BKZTRH(LZTRH)
      CALL MZLIFT(IXMAIN,LZTMP,LZTRH,-IZZTMP,MBOOKT,0)
C
  999 RETURN
      END
