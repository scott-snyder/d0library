      SUBROUTINE PMHITC(IVIEW,IFLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-             IVIEW - 1 for Z-Y
C-                     2 for X-Y
C-                     3 for Z-X
C-             IFLAG - 0 all layers
C-             IFLAG - 1 A layer only
C-             IFLAG - 2 cen only  
C-   Outputs : 
C-   Controls: 
C-
C-   Modified 19-DEC-1993   N. Oshima - Fixed IPHITS problem for both
C-                          RECO version < V12 and >= V12.
C-   Created  23-JUN-1990   S. Hagopian
C-     4/20/93 - added HITFLG,DTIMFLG
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER IVIEW,IFLAG
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER LVIEW,HITFLG,DTIMFLG
      INTEGER LMUHT,GZMUHT
      INTEGER LMUOF,LMUOH
      INTEGER PNTMUOF,PNTMUOH
      INTEGER NMOD,IMOD,NHIT
      INTEGER I1,I2,IPHITS
      INTEGER NUMMOD(3)
      INTEGER FIRST,NMUSRT,NWD,L
      INTEGER IVER,IXX,VERSION,PASS
      REAL XMOD(60),YMOD(60),ZMOD(120)
      DATA FIRST,NWD/0,28/
      DATA NUMMOD/60,60,120/
      DATA XMOD/10,13,20,23,30,33,14*0,                 ! END A LAYER
     X     100,110,120,130,140,107,117,127,137,147,
     X     103,113,123,133,143,104,114,124,134,144,     ! END B LAYER
     X     200,210,220,230,240,207,217,227,237,247,
     X     203,213,223,233,243,204,214,224,234,244 /    ! END C LAYER
      DATA YMOD/11,12,15,16,21,22,25,26,31,32,35,36,8*0,! END A LAYER
     X     101,111,121,131,141,102,112,122,132,142,
     X     105,115,135,145,106,116,136,146, 2*0,         ! END B LAYER
     X     201,211,221,231,241,202,212,222,232,242,
     X     205,215,235,245,206,216,236,246, 2*0/        ! END C LAYER       
      DATA ZMOD/61,62,64,65,67,91,92,94,95,97,30*0,     ! END A LAYER
     X     150,153,160,161,162,163,164,165,166,167,
     X     180,183,190,191,192,193,194,195,196,197,20*0,! END B LAYER
     X     250,251,253,255,
     X     260,261,262,263,264,265,266,267,
     X     270,271,272,273,274,275,276,277,
     X     280,281,283,285,
     X     290,291,292,293,294,295,296,297,
     X     300,301,302,303,304,305,306,307/              ! END C LAYER

C---------------------------------------------------------------------
      CALL PUGETV('MUON DRAW HITS',HITFLG)
      CALL PUGETV('MUON DRIFT TIME',DTIMFLG)
      IF(HITFLG.LT.1.OR.HITFLG.GT.3)GO TO 999
      LVIEW=IVIEW
      IF(IVIEW.EQ.3)LVIEW=13
      LMUHT=GZMUHT(0)
      IF(LMUHT.LE.0)GO TO 999
      LMUOF=LQ(LMUHT-2)
      IF(LMUOF.LE.0)GO TO 999
      LMUOH=LQ(LMUHT-1)
      IF(LMUOH.LE.0)GO TO 999
C Fix size so can read old MC
      IF(FIRST.EQ.0) THEN
        NMUSRT=IQ(LMUHT+2)
        IF(LMUHT.NE.0.AND.NMUSRT.NE.0.AND.LMUOH.NE.0) THEN
          NWD=28
          L=IQ(LMUOH-1)/NMUSRT
          IF(L.EQ.25) NWD=25
          FIRST=1
        ENDIF
      ENDIF
CC Fix iIVER diff for RUn 1 a and I b
C      CALL MUDVER(IVER)
C      IXX=IVER/10
C      IVER=IVER-IXX*10
C-
C--- Check RECO Version to set correct IPHITS
C-
      IVER = 2
      CALL RECO_VERSION(VERSION,PASS)
      IF ( VERSION.GT.0 .AND. VERSION.LT.12 ) IVER = 1
C Find hits in X chambers 
      IF(IVIEW.EQ.2.OR.IVIEW.EQ.3)THEN
         NMOD=NUMMOD(1)        
         IF(IFLAG.EQ.1)NMOD=NMOD/3
         DO 15 I1=1,NMOD
         IMOD=XMOD(I1)
         IF(IMOD.EQ.0)GO TO 15
C Find hit number in MUOF bank for module IMOD
         PNTMUOF=IQ(LMUHT+10+IMOD)
         IF(PNTMUOF.EQ.0)GO TO 15
C FIND number of processed hits for this module
C 10 words/module hit
         PNTMUOF=10*(PNTMUOF-1)+1
         NHIT=IQ(LMUOF+PNTMUOF+3)
         IF(NHIT.LE.0)GO TO 15
         PNTMUOH=IQ(LMUOF+PNTMUOF+4)
         DO 10 I2=1,NHIT
         IF(IVER.EQ.1)THEN
           IPHITS=(PNTMUOH-1)/NWD +I2
         ELSE
           IPHITS=(PNTMUOH-1) +I2
         ENDIF
         CALL PMHITS(IPHITS,LVIEW,HITFLG,DTIMFLG)
   10    CONTINUE
   15    CONTINUE  
      ENDIF
C Find hits in Y chambers 
      IF(IVIEW.EQ.2.OR.IVIEW.EQ.1)THEN
         NMOD=NUMMOD(2)
         IF(IFLAG.EQ.1)NMOD=NMOD/3
         DO 25 I1=1,NMOD
         IMOD=YMOD(I1)
         IF(IMOD.EQ.0)GO TO 25
C Find hit number in MUOF bank for module IMOD
         PNTMUOF=IQ(LMUHT+10+IMOD)
         IF(PNTMUOF.EQ.0)GO TO 25
C FIND number of processed hits for this module
C 10 words/module hit
         PNTMUOF=10*(PNTMUOF-1)+1
C FIND number of processed hits for this module
         NHIT=IQ(LMUOF+PNTMUOF+3)
         IF(NHIT.LE.0)GO TO 25
         PNTMUOH=IQ(LMUOF+PNTMUOF+4)
         DO 20 I2=1,NHIT
         IF(IVER.EQ.1)THEN
           IPHITS=(PNTMUOH-1)/NWD +I2
         ELSE
           IPHITS=(PNTMUOH-1) +I2
         ENDIF
         CALL PMHITS(IPHITS,LVIEW,HITFLG,DTIMFLG)
   20    CONTINUE
   25    CONTINUE  
      ENDIF  
         IF(IFLAG.EQ.2)GO TO 999
C Find hits in Z chambers 
      IF(IVIEW.EQ.1.OR.IVIEW.EQ.3)THEN
         NMOD=NUMMOD(3)
         IF(IFLAG.EQ.1)NMOD=NMOD/3
         DO 35 I1=1,NMOD
         IMOD=ZMOD(I1)
         IF(IMOD.EQ.0)GO TO 35
C Find hit number in MUOF bank for module IMOD
         PNTMUOF=IQ(LMUHT+10+IMOD)
         IF(PNTMUOF.EQ.0)GO TO 35
C FIND number of processed hits for this module
C 10 words/module hit
         PNTMUOF=10*(PNTMUOF-1)+1
C FIND number of processed hits for this module
         NHIT=IQ(LMUOF+PNTMUOF+3)
         IF(NHIT.LE.0)GO TO 35
         PNTMUOH=IQ(LMUOF+PNTMUOF+4)
         DO 30 I2=1,NHIT
         IF(IVER.EQ.1)THEN
           IPHITS=(PNTMUOH-1)/NWD +I2
         ELSE
           IPHITS=(PNTMUOH-1) +I2
         ENDIF
         CALL PMHITS(IPHITS,LVIEW,HITFLG,DTIMFLG)
   30    CONTINUE
   35    CONTINUE  

      ENDIF
  999 RETURN
      END
