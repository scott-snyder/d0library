      SUBROUTINE BKZTRK(LZTRK)
C-----------------------------------------------------------------------
C  Subroutine BKZTRK books bank ZTRK - central track
C
C  Output:
C    LZTRK       location of the booked bank in ZEBCOM.
C
C  Daria Zieminska Nov 1989
C-   Updated  02-JAN-1990   Qizhong Li-Demarteau  fix wrong format for ZTRK
C-                                              bank from "3I 4F" to "4I 3F" 
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZZTRK.LINK/LIST'
      INTEGER  GZZTRH,LZTRH,LZTRK
      INTEGER IXZTRK,MBOOKT(5),IBOOKT 
      LOGICAL FIRST 
      DATA IBOOKT/4HZTRK/
      DATA FIRST/.TRUE./
C
      IF (FIRST) THEN
C
        CALL MZFORM('ZTRK','1B 4I 3F',IXZTRK)
        MBOOKT(1)=IBOOKT
        MBOOKT(2)=9      
        MBOOKT(3)=1
        MBOOKT(4)=8
        MBOOKT(5)=IXZTRK
        FIRST=.FALSE.
      END IF
      LZTRH = GZZTRH() 
      IF ( LZTRH .EQ. 0 ) CALL BKZTRH(LZTRH)
      CALL MZLIFT(IXMAIN,LZTRK,LZTRH,-IZZTRK,MBOOKT,0)
 1000 RETURN
      END
