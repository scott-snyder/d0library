C                  VERT.INC
C
C ** Common containing general run info and vertex variables**
C
      COMMON/VERT/RUN,NVERT,ZVERT
C
      REAL    ZVERT
      INTEGER RUN(9),NVERT
C
C       RUN(1) = Run number
C       RUN(2) = Event number
C       RUN(3) = L1 bits
C       RUN(4) = L2 bits 31-0
C       RUN(5) = L2 bit2 63-32
C       RUN(6) = L2 bit3 95-64
C       RUN(7) = L2 bit4 127-96
C       RUN(8) = Crossing number
C       RUN(9) = Flag word
CC
