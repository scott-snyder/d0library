      SUBROUTINE GTPARH(VERSION,NPMUO,NPELC,NPPHO,NPNUT,NPVES,NTAUS,
     1                  NPDIL)
C----------------------------------------------------------------------
C 
C    Get contents of PARH (particle header bank) 
C 
C    20-JUL-1990 Daria Zieminska
C 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER GZPARH,LPARH,VERSION,NPMUO,NPELC,NPPHO,NPNUT,NPVES,NTAUS
      INTEGER NPDIL
C----------------------------------------------------------------------
C
      LPARH=GZPARH()
      IF (LPARH.GT.0) THEN
        VERSION=IQ(LPARH+1)
        NPMUO=IQ(LPARH+2)
        NPELC=IQ(LPARH+3)
        NPPHO=IQ(LPARH+4)
        NPNUT=IQ(LPARH+5)
        NPVES=IQ(LPARH+6)
        NTAUS=IQ(LPARH+7)
        NPDIL=IQ(LPARH+8)
      ELSE
        VERSION=0 
        NPMUO=0
        NPELC=0
        NPPHO=0
        NPNUT=0
        NPVES=0
        NTAUS=0
        NPDIL=0
      END IF
 1000 RETURN
      END
