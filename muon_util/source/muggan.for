c DEC/CMS REPLACEMENT HISTORY, Element MUGGAN.FOR
c *4    20-JUN-1989 16:35:56 TAMI "JGREEN fix"
c *3     1-MAY-1989 15:05:36 TAMI "Setup for MC"
c *2    10-APR-1989 13:23:50 TAMI "Do gains"
c *1    19-DEC-1986 18:26:58 HEDIN "Muon Utilities initial versions"
c DEC/CMS REPLACEMENT HISTORY, Element MUGGAN.FOR
      SUBROUTINE MUGGAN(NMOD,NPLN,NWIR,GAIN1,GAIN2,GAIN3,GAIN4,PED1,
     A PED2,PED3,PED4)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      ROUTINE TO GET MUON PAD PEDESTALS AND GAINS FROM MGAN BANK
C      INPUT:NMOD,NPLN,NWIR    MUON MODULE,PLANE,WIRE
C      OUTPUT:GAIN1-4,PED1-4      GAINS AND PEDSFOR LEFT AND RIGHT PADS
C                                 EVEN AND THEN ODD    
C      DH 10-6-86    DUMMY FOR KNOW
C      J.Green   1-4-89        Still need to handle default = mean gain
C      J.Green   5-24-89       Change offset +2 to get gains, not peds
C      DH 4/91 HARDWAIRE DEFAULTS
C      S.Igarashi 11/91 For MC digitization version 2
C     DH 4/92 skip out if MC
C     DH 6/92 GET PEDS
C     td 2/93 call errmsg on zeroes on data
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER NMOD,NPLN,NWIR
      REAL GAIN1,GAIN2,GAIN3,GAIN4,PED1,PED2,PED3,PED4
      INTEGER FIRST,STATUS
      REAL AVE
      INTEGER LMGAN, GZMGAN,GZMUD1,L,MC
      INTEGER IADD, NCON,NP,MGAIN,PED0
      INTEGER NUMVER
      DATA NUMVER/536875808/              ! '20001320'X
      DATA FIRST,MC/0,0/
      IF(FIRST.EQ.0) THEN
        L=GZMUD1(0)
        IF(IQ(L+4).EQ.1) MC=1      ! MONTE CARLO
        IF(IQ(L+4).EQ.NUMVER) MC=2 ! NEW MONTE CARLO
        IF(MC.EQ.1) THEN           ! OLD MONTE CARLO
          MGAIN=1
          PED0=0
        ELSEIF(MC.EQ.2) THEN       ! NEW MONTE CARLO
          MGAIN=1
          PED0=0
        ELSE
          MGAIN=1
          PED0=300
        ENDIF
        FIRST=1
      ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      LMGAN = GZMGAN(NMOD)
      IF (LMGAN.NE.0.AND.MC.EQ.0) THEN
        NCON = IC(LMGAN+10)
        NP = IC(LMGAN+11)
        STATUS=IC(LMGAN+1)/100
        STATUS=IC(LMGAN+1)-STATUS*100
        IF(STATUS.GE.10) THEN    !L2
          IADD = NWIR*4 + 2*NPLN*NCON/NP
          GAIN1 = C(LMGAN + 18 + IADD)
          GAIN2 = C(LMGAN + 20 + IADD)
          GAIN3 = C(LMGAN + 22 + IADD)
          GAIN4 = C(LMGAN + 24 + IADD)
          PED1 = C(LMGAN + 17 + IADD)
          PED2 = C(LMGAN + 19 + IADD)
          PED3 = C(LMGAN + 21 + IADD)
          PED4 = C(LMGAN + 23 + IADD)
        ELSE
          IADD = NWIR*8 + 4*NPLN*NCON/NP
          GAIN1 = C(LMGAN + 19 + IADD)
          GAIN2 = C(LMGAN + 23 + IADD)
          GAIN3 = C(LMGAN + 27 + IADD)
          GAIN4 = C(LMGAN + 31 + IADD)
          PED1 = C(LMGAN + 17 + IADD)
          PED2 = C(LMGAN + 21 + IADD)
          PED3 = C(LMGAN + 25 + IADD)
          PED4 = C(LMGAN + 29 + IADD)
          IF(C(LMGAN+20).LT.0.) THEN
            GAIN1=MGAIN
            PED1=PED0
          ENDIF
          IF(C(LMGAN+24).LT.0.) THEN
            GAIN2=MGAIN
            PED2=PED0
          ENDIF
          IF(C(LMGAN+28).LT.0.) THEN
            GAIN3=MGAIN
            PED3=PED0
          ENDIF
          IF(C(LMGAN+32).LT.0.) THEN
            GAIN4=MGAIN
            PED4=PED0
          ENDIF
          IF (GAIN1.EQ.0.0)               GAIN1 = MGAIN
          IF (GAIN2.EQ.0.0)               GAIN2 = MGAIN
          IF (GAIN3.EQ.0.0)               GAIN3 = MGAIN
          IF (GAIN4.EQ.0.0)               GAIN4 = MGAIN
        ENDIF
      ELSE
        GAIN1 = MGAIN
        GAIN2 = MGAIN
        GAIN3 = MGAIN
        GAIN4 = MGAIN
        PED1=PED0
        PED2=PED0
        PED3=PED0
        PED4=PED0
      ENDIF
      IF(MC.EQ.0) THEN
        IF(GAIN1.EQ.0..OR.GAIN2.EQ.0..OR.GAIN3.EQ.0..OR.GAIN4.EQ.0.) 
     $  THEN
          CALL ERRMSG('L2_MUON','MUGGAN','ZERO GAINS ALERT EXPERT','W')
        ENDIF
        IF(PED1.EQ.0..OR.PED2.EQ.0..OR.PED3.EQ.0..OR.PED4.EQ.0.) 
     $  THEN
          CALL ERRMSG('L2_MUON','MUGGAN','ZERO PEDS ALERT EXPERT','W')
        ENDIF
      ENDIF
      AVE=(GAIN1+GAIN2)/2.
      GAIN1=GAIN1/AVE        !
      GAIN2=GAIN2/AVE        ! NORMALIZE TO 1     
      AVE=(GAIN3+GAIN4)/2.
      GAIN3=GAIN3/AVE        !
      GAIN4=GAIN4/AVE        ! NORMALIZE TO 1     
C
      RETURN
      END
