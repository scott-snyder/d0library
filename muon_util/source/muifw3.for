      SUBROUTINE MUIFW3(ITRAK)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Fill flag word 3 in MUOT with
C-                         trigger tag information
C-
C-    Created :  4-MAR-94  M. Fortner
C-    Modified : 6/94 MF   tag single tracks
C-    Modified:  04-FEB-95 I.Mandrichenko Changed format of STTH
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER IATR,IBTR,ICTR,ITRIG,JTRIG,JTRIG2,JAB,JAC,JBC
      INTEGER LMUOT,LMUHM,NWAHIT,IMOD
      INTEGER ITRAK,IH,IWADD,IHMUOH,ITSIGN,IDELT,IPAD
      INTEGER NSAHIT,ISTA,ISEC,IADDR
      INTEGER GZMUOT,GZMUHM,GZSTTH
      INTEGER LSTTH
      EXTERNAL GZMUOT,GZMUHM,GZSTTH
      REAL    SAMGEO(6),SAMDST
C
      LMUOT = GZMUOT(ITRAK)
      IF (LMUOT.EQ.0) RETURN
      IATR = 0
      IBTR = 0
      ICTR = 0
C
C    Loop over wide angle hits
C
      NWAHIT = IQ(LMUOT+1)
      DO IH=1,NWAHIT
        CALL GTMHTT(ITRAK,IH,IWADD,IHMUOH,ITSIGN,IDELT,IPAD)
        IMOD = IBITS(IWADD,8,9)
        LMUHM = GZMUHM(IMOD)
        ITRIG = 0
        IF (LMUHM.NE.0) ITRIG=IQ(LMUHM+13)
        IF (IMOD.GT.0.AND.IMOD.LT.100) IATR=IOR(ITRIG,IATR)
        IF (IMOD.GE.100.AND.IMOD.LT.200) IBTR=IOR(ITRIG,IBTR)
        IF (IMOD.GE.200.AND.IMOD.LT.400) ICTR=IOR(ITRIG,ICTR)
      ENDDO
C
C    Loop over small angle hits
C
      LSTTH = 0
      NSAHIT = IQ(LMUOT+2)
      DO IH=1,NSAHIT
        CALL GTSTTH(ITRAK,LSTTH,IH,ISTA,ISEC,IADDR,SAMGEO,SAMDST)
        ITRIG = 0
        DO ISEC=1,6
          CALL SATOPM(ISTA,ISEC,IMOD)
          LMUHM = GZMUHM(IMOD)
          IF (LMUHM.NE.0) ITRIG=IOR(ITRIG,IQ(LMUHM+13))
        ENDDO
        IF (ISTA.EQ.1.OR.ISTA.EQ.4) IATR=IOR(ITRIG,IATR)
        IF (ISTA.EQ.2.OR.ISTA.EQ.5) IBTR=IOR(ITRIG,IBTR)
        IF (ISTA.EQ.3.OR.ISTA.EQ.6) ICTR=IOR(ITRIG,ICTR)
      ENDDO
C
C    Check for trigger condition and set bits
C
      JAB = IAND(IATR,IBTR)
      JAC = IAND(IATR,ICTR)
      JBC = IAND(IBTR,ICTR)
      JTRIG2 = IOR(JAB,JAC)
      IF (IQ(LMUOT+3).LE.4) THEN
        JTRIG = JTRIG2
      ELSE
        JTRIG = IAND(JAB,JAC)
      ENDIF
      JTRIG2 = IOR(JTRIG2,JBC)
      CALL MVBITS(JTRIG,0,4,IQ(LMUOT+6),16)
      CALL MVBITS(JTRIG2,0,4,IQ(LMUOT+6),20)
C
  999 RETURN
      END
