      SUBROUTINE MTRGFL(ITRGR,ICRATE,ITRAIL)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Fill MTRG bank for one module
C-
C-   Inputs  :  ITRGR  - pointer to beginning of crate in TRGR
C-              ICRATE - number of crate in bank MTRG
C-              ITRAIL - pointer to end of data in crate
C-
C-    Output :  D0 Zebra output bank.   (MTRG)
C-
C-    Created :  19-JAN-94  M. Fortner (combined gttrgr3 & flmtrg)
C-    Modified : 5/95 MF Fill IMTRG+6 with hipt/lopt info
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER ITRGR,ICRATE,ITRAIL,JERR
      INTEGER LMTRG,GZMTRG
      EXTERNAL GZMTRG
      INTEGER JTRGR,KTRGR,IMTRG,IMOTR,DATA(2)
      INTEGER J,K,JSTAT,JOTC,JQUAD,IEND,DONE,VERSION,ILATCH
      INTEGER ILOPT,IHIPT
C
C	Book MTRG bank if it doesn't exist
C
      JERR = 0
      LMTRG = GZMTRG(0)
      IF (LMTRG.EQ.0) CALL BKMTRG(0,0,LMTRG)
C
C	Fill otc crate and otcmgr words
C
      IMTRG = LMTRG + 89*(ICRATE-1)
      IQ(IMTRG+1) = IBITS(IQ(ITRGR+3),24,8)
C
C       version number
      IQ(IMTRG+2) = IBITS(IQ(ITRGR+4),0,16)
      VERSION = 0
      IF (IQ(IMTRG+2).GE.11028) VERSION=1               ! hex 2B14
      IF (IQ(IMTRG+2).GE.11301) VERSION=2               ! hex 2C25
      IF (IQ(IMTRG+2).GE.12549) VERSION=3               ! hex 3105
      IF (IQ(IMTRG+2).GE.15139) VERSION=11              ! hex 3B23
      IF (IQ(IMTRG+2).GE.16680) VERSION=12              ! hex 4128
      JTRGR = ITRGR + 8
      IF (VERSION.GE.2) JTRGR=ITRGR+9
      IF (VERSION.GE.10) JTRGR=ITRGR+12
C
C	reply words (run 1b only)
      IQ(IMTRG+3) = 0
      IF (VERSION.GT.10) IQ(IMTRG+3)=IQ(ITRGR+6)
C
C       manager status bits
      IQ(IMTRG+4)=IBITS(IQ(JTRGR+1),8,8)
      IF (VERSION.GT.10) THEN
          JSTAT = IQ(IMTRG+4)
          IF (IBITS(IQ(ITRGR+11),24,8).NE.0) JSTAT=IBSET(JSTAT,16)
          IF (IBITS(IQ(ITRGR+12),24,8).NE.0) JSTAT=IBSET(JSTAT,16)
          IF (IBITS(IQ(ITRGR+7),16,16).NE.0) JSTAT=IBSET(JSTAT,24)
          IF (IBITS(IQ(ITRGR+7),0,16).NE.0) JSTAT=IBSET(JSTAT,25)
          CALL MVBITS(IQ(ITRGR+8),16,1,JSTAT,26)
          CALL MVBITS(IQ(ITRGR+8),17,1,JSTAT,27)
          CALL MVBITS(IQ(ITRGR+8),18,1,JSTAT,28)
          IQ(IMTRG+4) = JSTAT
      ENDIF
C
C	spare
      IQ(IMTRG+5) = 0
C
C	otcmgr trigger bits
      IF (VERSION.EQ.0) IQ(IMTRG+7)=IBITS(IQ(ITRGR+6),0,16)
      IF (VERSION.GT.0) IQ(IMTRG+7)=IBITS(IQ(ITRGR+6),0,3)
      IF (VERSION.GT.10) IQ(IMTRG+7)=IBITS(IQ(ITRGR+8),0,4)
      ILOPT = IBITS(IQ(IMTRG+7),0,2)
      IHIPT = IBITS(IQ(IMTRG+7),2,1)
      IF (ICRATE.EQ.1.AND.VERSION.LT.10) IHIPT=ILOPT
      IF (ICRATE.EQ.1.AND.VERSION.EQ.11) THEN
        IHIPT = ILOPT/2
        ILOPT = ILOPT - IHIPT*2
      END IF
      IF (ICRATE.EQ.2.OR.ICRATE.EQ.3) THEN
        IF (VERSION.LE.10) ILOPT=ILOPT+IHIPT
        IF (VERSION.EQ.11) ILOPT=MOD(ILOPT,2)+ILOPT/2
      END IF
      IQ(IMTRG+6) = IHIPT*16 + ILOPT
C
C       cct latch bits
      ILATCH = IQ(ITRGR+8)
      IF (VERSION.GT.10) ILATCH=IQ(ITRGR+11)
      IQ(IMTRG+8) = ILATCH
      CALL MOTMOD(2,ICRATE,ILATCH)
      ILATCH = 268435456
      IF (VERSION.GE.2) ILATCH=IQ(ITRGR+9)
      IF (VERSION.GT.10) ILATCH=IQ(ITRGR+12)
      IQ(IMTRG+9) = ILATCH
      IF (ICRATE.EQ.2.OR.ICRATE.EQ.3) CALL MOTMOD(2,ICRATE+4,ILATCH)
C
C       Fill otc card information
C
      DO K = 0,15
          J = 9 + (K*5)
          CALL MOTNUM(ICRATE,K,JOTC,JQUAD)
          IQ(IMTRG+J+1) = JOTC
          IQ(IMTRG+J+2) = JQUAD
          IF (JOTC.NE.0) THEN
              IQ(IMTRG+J+3)=IBITS(IQ(JTRGR+1+K),0,8)
              CALL MVBITS(IQ(ITRGR+3),K,1,IQ(IMTRG+J+3),16)
              CALL MVBITS(IQ(ITRGR+5),K+16,1,IQ(IMTRG+J+3),17)
              IF (VERSION.GT.10) THEN
                  CALL MVBITS(IQ(ITRGR+11),24,8,DONE,0)
                  CALL MVBITS(IQ(ITRGR+12),24,8,DONE,8)
                  CALL MVBITS(DONE,K,1,IQ(LMTRG+J+3),18)
                  CALL MVBITS(IQ(ITRGR+7),K+16,1,IQ(LMTRG+J+3),24)
                  CALL MVBITS(IQ(ITRGR+7),K,1,IQ(LMTRG+J+3),25)
              ENDIF
          ENDIF
      END DO                      
C
C       Loop through individual OTC triggers
C
      KTRGR = JTRGR + 16 + 2
      IEND = ITRAIL + 1
      IF (VERSION.GE.2) IEND = IEND - 1
      DO WHILE (KTRGR.LT.IEND)
          DATA(1) = IQ(KTRGR)
          DATA(2) = IQ(KTRGR+1)
          IF (VERSION.GT.10) THEN
              CALL MVBITS(IQ(KTRGR+2),24,8,DATA(1),24)
          END IF
          JOTC = IBITS(DATA(1),24,4)
          IF (VERSION.LE.10) THEN
              JOTC = IBITS(DATA(1),24,8) + 599
              DO K = 0,15
                  J = IMTRG + 10 + (K*5)
                  IF(IQ(J).EQ.JOTC.AND.IQ(J).NE.0) JOTC = K
              ENDDO
          ENDIF
          IF (JOTC.GE.0.AND.JOTC.LT.16) THEN
              CALL MOTRFL(VERSION,ICRATE,JOTC,DATA,IMOTR)
              J = 9 + JOTC*5
              IF (IQ(IMTRG+J+4).EQ.0) IQ(IMTRG+J+5)=IMOTR
              IQ(IMTRG+J+4) = IQ(IMTRG+J+4) + 1
          ENDIF
          KTRGR = KTRGR + 2
      END DO
C
C       set modules in octants with triggers
C
      IF (VERSION.LT.12.OR.ICRATE.GT.1) THEN
        DO K = 0,15
          J = 9 + (K*5)
          IF (IQ(IMTRG+J+1).GE.700.AND.IQ(IMTRG+J+4).GT.0) 
     1      CALL MOTMOD(3,ICRATE,K)
        END DO
      ELSE
        IF (ILOPT.GT.0) THEN
          ILATCH = IQ(IMTRG+8)
          DO K = 1,8
            IF (BTEST(ILATCH,K+11)) THEN
              J = MOD(K,8)
              CALL MOTMOD(3,ICRATE,J)
            END IF
          END DO
        END IF
      END IF
C
  999 RETURN                                              
      END
