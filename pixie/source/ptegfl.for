      SUBROUTINE PTEGFL 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ADDS UP ENERGY FROM DIFFERNT GEAN TRACKS
C-             FILLING UP THE COMMON BLOCKS
C-
C-   Inputs  : GTLY ZEBRA BANK (TRD USER'S BANKS)
C-   Outputs :QUANTITIES IN COMMON /TRHITW/
C-
C-   Created  3-JAN-1989   LUPE ROAS 
C-   (Based on ANLTRD - A. ZYLBERSTEJN)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      INCLUDE 'D0$INC:GTRHLN.INC/LIST'
      INCLUDE 'D0$INC:TRHITW.INC/LIST'
      INCLUDE 'D0$INC:WORKSP.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:PRNEVT.INC/LIST'
C-----------------------------------------------------------------------
      INTEGER I,IE,IFOIS,IST,IW,LL,NSECT,NW,J,TOT
      INTEGER IZ
      INTEGER ANODE
      REAL EN,ENSECT(16)
C-----------------------------------------------------------------------
      DATA IFOIS/0/
      DATA ANODE/1/
C=======================================================================
C      IFOIS=IFOIS+1
C  COMPUTE THE NUMBER OF HIT WIRES IN EACH LAYER
C      CALL VZERO(NBTHIT,3)
C      CALL VZERO(ENSECT,16)
C      DO 100 IST=1,3
C        LL=LGTLY(IST)
C        DO 10 IZ=1,256
C          IWS(IZ)=0
C   10   CONTINUE
C        NW=0
C   20   IF(LL.LE.0)GO TO 36
C        DO 30 I=15,18
C          IW=MOD(IQ(LL+I),1000)
C          IF(IW.LE.0)GO TO 32
C          IE=IQ(LL+I)/1000
C          EN=FLOAT(IE)/100.
C          IF(IST.EQ.3)IW=(IW-1)/2+1   
C          NSECT=(IW-1)/16+1           
C          IF(NSECT.GT.16) GO TO 30   ! OVERFLOW
C          ENSECT(NSECT)=ENSECT(NSECT)+EN
C          IF(IW.GT.LENGWS) GO TO 30  ! OVERFLOW
C          IF(IWS(IW).LE.0)THEN      !  DEFINE A NEW WIRE
C            IF ((NW+1).GT.NMWHIT) THEN
C              GO TO 30  ! OVERFLOW
C            ENDIF
C            NW=NW+1
C            IWS(IW)=NW
C            NUMTWH(NW,IST,ANODE)=IW
C            ENTWH(NW,IST,ANODE)=EN
C          ELSE                      !ADD UP ENERGY ON SAME WIRE
C            IF (IWS(IW).GT.NMWHIT) GO TO 30   ! OVERFLOW
C            ENTWH(IWS(IW),IST,ANODE)=ENTWH(IWS(IW),IST,ANODE)+EN
C          END IF
C   30   CONTINUE
C   32   NBTHIT(IST,ANODE)=NW
C        LL=LQ(LL)
C        GO TO 20
C   36   CONTINUE
C        EN=0.
C        IF (NW.GT.NMWHIT) GO TO 100  !OVERFLOW
C        DO 38 I=1,NW
C          EN=EN+ENTWH(I,IST,ANODE)
C   38   CONTINUE
C  100 CONTINUE
C  860 CONTINUE
      CALL TRDHIT
  999 RETURN
      END
