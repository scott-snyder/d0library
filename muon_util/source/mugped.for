c DEC/CMS REPLACEMENT HISTORY, Element MUGPED.FOR
c *5     1-MAY-1989 15:08:09 TAMI "Setup for MC"
c *4    28-APR-1989 17:42:51 TAMI "JGREEN Fix"
c *3     5-JUL-1988 13:24:54 HEDIN "real routine"
c *2    13-APR-1988 15:42:29 TAMI "INCLUDE T,DT APPROX VALUES FOR BASEMENT"
c *1    19-DEC-1986 18:27:00 HEDIN "Muon Utilities initial versions"
c DEC/CMS REPLACEMENT HISTORY, Element MUGPED.FOR
      SUBROUTINE MUGPED(NMOD,NPLN,NWIR,PED)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      ROUTINE TO GET MUON PEDESTALS
C      INPUT:NMOD,NPLN,NWIR    MUON MODULE,PLANE,WIRE
C      OUTPUT:PED    8  PEDESTALS FOR GIVEN PADS AND TIMES
C
C      DH 10-6-86    DUMMY FOR KNOW
C      DH 3/88 INCLUDE T,DT. APPROX VALUES FOR BASEMENT
C      DH 4/88 GET FROM ZEBRA BANK
C      JG 4/88 FIX OFFSET BUG
C      DH 5/88 SETUP WITH DEFAULTS
C      DH 4/91 HARDWAIRE DEFAULTS
C      SI 11/91 For MC digitization version 2
C      DH 4/92 skip out if MC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INTEGER NMOD,NPLN,NWIR
      REAL PED(8)
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER MC,GZMUD1,L,I,N,NCON,GZMPED,LCON,NP,TIME,PED0
      INTEGER NUMVER
      DATA NUMVER/536875808/              ! '20001320'X
      DATA MC,TIME/0,0/
      IF(TIME.EQ.0) THEN
        L=GZMUD1(0)
        IF(IQ(L+4).EQ.1) MC=1      ! MONTE CARLO
        IF(IQ(L+4).EQ.NUMVER) MC=2 ! NEW MONTE CARLO
        IF(MC.EQ.1) THEN           ! OLD MONTE CARLO
          PED0=0
        ELSEIF(MC.EQ.2) THEN       ! NEW MONTE CARLO
          PED0=0
        ELSE
          PED0=300
        ENDIF
        TIME=1
      ENDIF
      LCON=GZMPED(NMOD)
      IF(LCON.EQ.0.OR.MC.GT.0) THEN ! NO NEW CONSTANTS
        PED(1)=PED0        ! PAD1
        PED(2)=PED0       ! PAD2
        PED(3)=PED0       ! TIME1
        PED(4)=PED0       ! TIME2
        PED(5)=PED0       ! PAD3
        PED(6)=PED0       ! PAD4
        PED(7)=PED0       ! DT1
        PED(8)=PED0       ! DT2
      ELSE
        NCON=IC(LCON+10)
        NP=IC(LCON+11)
        N = LCON + 16 + NWIR*8 + (2*NCON/NP)*NPLN
        DO I=1,8
          PED(I)=C(N+2*I-1)
        ENDDO
      ENDIF
      RETURN
      END
