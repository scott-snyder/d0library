      SUBROUTINE DETTRD
C
C     ******************************************************************
C     *
C     *     DEFINITION OF TRD DETECTORS AND HITS
C     *     SLL 18 JUN 1986
C-          UPDATED   8-JAN-1988   A. ZYLBERSTEJN   :NOT USED BUT KEPT
C-                  BECAUSE I DON't know how to make the TRD to be
C-                  sensitive.
C     *
C     ******************************************************************
C
C                   GSDET parameters
C
      INTEGER I,NL,ISET,IDET
      PARAMETER(NL=1)
      CHARACTER*4 NAMESV(NL)
      INTEGER NBITSV(NL),IDTYPE,NWHI,NWDI
C
C                   GSDETH parameters
C
      INTEGER NH,NBIT
      PARAMETER (NH=4)
      INTEGER NBITSH(NH)
      CHARACTER*4 NAMESH(NH)
      REAL ORIG(NH),FACT(NH)
C
      CHARACTER*1 CH1

      DATA
     +              NBITSV/8/,IDTYPE/1/,NWHI/256/,NWDI/256/
     +              ,NBITSH/4*32/
     +              ,NAMESH/'XPOS','YPOS','ZPOS','ELOS'/
     +              ,ORIG/3*1000.,0./,FACT/3*1000.,1000000./
C
C     ------------------------------------------------------------------
C
C                   Define TEC as sensitive detector
C
      DO 10 I=1,3
        CH1 = CHAR(48+I)
        NAMESV(1)='TXC'//CH1
        CALL GSDET('TRD ',NAMESV(1),NL,NAMESV,NBITSV,IDTYPE
     +                ,NWHI,NWDI,ISET,IDET)
        CALL GSDETH('TRD ',NAMESV(1),NH,NAMESH,NBITSH,ORIG,FACT)
   10 CONTINUE
      END
