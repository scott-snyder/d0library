      SUBROUTINE MUGTCN(NMOD,NPLN,MWIR,T01,TRES1,T02,TRES2,
     A   DT01,DTSLP1,DT02,DTSLP2)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      ROUTINE TO GET MUON TIME CONSTANTS
C      INPUT:NMOD,NPLN,NWIR    MUON MODULE,PLANE,WIRE
C      OUTPUT:T01,T02  drift time = 0 in adc counts  1,2 time
C             TRES1,TRES2  ns/adc counts   1,2 time
C             DT01,DT02   where T1=T2 0 on time division 1,2 time
C             DTSLP1,DTSLP2  adc counts/ns on 1,2 time division
C
C      DH 10-6-86    DUMMY FOR KNOW
C      DH 8-88 change arguements, put in constants 'by hand'
C      J.Green 1-89     change arguments, get DT constants from bank DTIM
C      JG 4/89 fix index bug
C      DH 5/89 make into standard version; single default value
C              used if no value in ZEBRA for a given constant
C     DH 4/91 hardwaire default values; use MUD1 header
C     DH 6/91 add read for times; rearrange delta time ordering
C     SI 11/91 For MC digitization version 2
C     DH 4/92 change defaults, skip out if MC
C     DH 5/92 change defaults again
C     JBa. 6/92 Handle channel error flag/do L2 compressed
C     DH 9/92 allow odd wire input
C     DH 9/92 fix channel error handling
C     AT 12/92 modify protection against ZERO value constant
C     TD 2/93 ERRMSG ON ZERO CONSTANT VALUE
C     DW 2/93 restore flag checking to old (pre-12/92) covention
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER MC,GZMUD1,GZMDTM,LMDTM,IADD,L
      INTEGER NMOD,NPLN,NWIR,FIRST,LMTIM,GZMTIM
      INTEGER NP,NCON,STATUS,MWIR
      REAL T01,T02,TRES1,TRES2,DT01,DT02,DTSLP1,DTSLP2
      REAL T0,DT0,TRES,DTSLP,T_CHK,SL_CHK
      INTEGER NUMVER
      DATA NUMVER/536875808/              ! '20001320'X
      DATA FIRST,MC/0,0/
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF(FIRST.EQ.0) THEN
        L=GZMUD1(0)
        IF(IQ(L+4).EQ.1) MC=1      ! MONTE CARLO
        IF(IQ(L+4).EQ.NUMVER) MC=2 ! NEW MONTE CARLO
        IF(MC.EQ.1) THEN           ! OLD MONTE CARLO
          CALL INTMSG(' Old muon Monte Carlo: Use MUO_STPFILE.DAT ')
          DT0=80.
          T0=0.
          TRES=-1.
          DTSLP=1.
        ELSEIF(MC.EQ.2) THEN       ! NEW MONTE CARLO
          DT0=1434.
          T0=3000.
          TRES=0.488
          DTSLP=0.0488
        ELSE
          DT0=1900.
          T0=2800.
          TRES=.35
          DTSLP=.036
        ENDIF
        FIRST=1
      ENDIF
      NWIR=MWIR-MOD(MWIR,2)    ! MAKE EVEN WIRE
C
      T01=T0
      T02=T0
      DT01=DT0
      DT02=DT0
      TRES1=TRES             ! RAW TIME UNITS
      TRES2=TRES
      DTSLP1=DTSLP
      DTSLP2 = DTSLP
      IF(MC.GT.0) RETURN
C
C     delta times
      LMDTM = GZMDTM(NMOD)
      IF (LMDTM.NE.0) THEN
        STATUS=IC(LMDTM+1)/100
        STATUS=IC(LMDTM+1)-STATUS*100
        NCON = IC(LMDTM+10)
        NP = IC(LMDTM+11)
        IF(STATUS.GE.10) THEN   ! Compressed !
          IADD = NWIR*2 + 2*NPLN*NCON/NP
          DT02 = C(LMDTM+17+IADD)
          DTSLP2 = C(LMDTM+18+IADD)
          DT01 = C(LMDTM+19+IADD)
          DTSLP1 = C(LMDTM+20+IADD)
        ELSE                    ! Regular, from DB !
          IADD = NWIR*4 + 4*NPLN*NCON/NP
          DT02 = C(LMDTM+17+IADD)
          DTSLP2 = C(LMDTM+18+IADD)
          DT01 = C(LMDTM+21+IADD)
          DTSLP1 = C(LMDTM+22+IADD)
          IF(C(LMDTM+20+IADD).LT.0.) THEN
            DT02=C(LMDTM+13)
            DTSLP2=C(LMDTM+15)
          ENDIF
          IF(C(LMDTM+24+IADD).LT.0.) THEN
            DT01=C(LMDTM+14)
            DTSLP1=C(LMDTM+16)
          ENDIF
        ENDIF
      ENDIF
C        Times
      LMTIM = GZMTIM(NMOD)
      IF (LMTIM.NE.0) THEN
        STATUS=IC(LMTIM+1)/100
        STATUS=IC(LMTIM+1)-STATUS*100
        NCON = IC(LMTIM+10)
        NP = IC(LMTIM+11)
        IF(STATUS.GE.10) THEN     ! Compressed !
          IADD = NWIR*2 + 2*NPLN*NCON/NP
          T02 = C(LMTIM+19+IADD)
          TRES2 = C(LMTIM+20+IADD)
          T01 = C(LMTIM+17+IADD)
          TRES1 = C(LMTIM+18+IADD)
        ELSE                      ! Regular DB !
          IADD = NWIR*4 + 4*NPLN*NCON/NP
          IF(C(LMTIM+24+IADD).GE.0.) THEN
            T02 = C(LMTIM+21+IADD)
            TRES2 = C(LMTIM+22+IADD)
          ELSE
            T02=C(LMTIM+14)    ! USE AVERGAE IF BAD
            TRES2=C(LMTIM+16)
          ENDIF
          IF(C(LMTIM+20+IADD).GE.0.) THEN
            T01 = C(LMTIM+17+IADD)
            TRES1 = C(LMTIM+18+IADD)
          ELSE
            T01=C(LMTIM+13)
            TRES1=C(LMTIM+15)
          ENDIF
        ENDIF
      ENDIF
      IF(MC.EQ.0) THEN
        IF(T01.EQ.0..OR.T02.EQ.0..OR.TRES1.EQ.0..OR.TRES2.EQ.0.) THEN
          CALL ERRMSG('L2_MUON','MUGTCN','ZERO TIMES CALL EXPERT','W')
        ENDIF
        IF(DT01.EQ.0..OR.DT02.EQ.0..OR.DTSLP1.EQ.0..OR.DTSLP2.EQ.0.)
     $    THEN
          CALL ERRMSG('L2_MUON','MUGTCN','ZERO TIMES CALL EXPERT','W')
        ENDIF
      ENDIF

  999 RETURN
      END
