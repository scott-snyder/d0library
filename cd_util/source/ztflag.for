      SUBROUTINE ZTFLAG(LZTRK,PART)
C-----------------------------------------------------------------------
C  Flag CD track ZTRK in lepton road. 
C  Set BIT on in ZTRK status word IQ(LZTRK) and in the status word of
C  corresponding banks VTXT, DTRK, FDCT
C
C  Input:   LZTRK track bank location
C
C  Daria Zieminska 4-Feb-1991 
C-   Updated   5-SEP-1991   Qizhong Li-Demarteau   use bits definition from
C-                                                params file instead of RCP 
C-----------------------------------------------------------------------
      IMPLICIT NONE           
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'
      INTEGER LZTRK,LVTXT,LDTRK,LFDCT,BIT
      CHARACTER*3 PART
C-----------------------------------------------------------------------
      IF (PART.EQ.'MUO') BIT=MUROAD
      IF (PART.EQ.'ELE') BIT=ELROAD
      IF (PART.EQ.'TAU') BIT=TAUROAD
      IF (LZTRK.LE.0 ) GO TO 1000
      IQ(LZTRK)=IBSET(IQ(LZTRK),BIT)
      LVTXT=LQ(LZTRK-6)
      LDTRK=LQ(LZTRK-7)
      LFDCT=LQ(LZTRK-8)
      IF (LVTXT.GT.0) IQ(LVTXT)=IBSET(IQ(LVTXT),BIT)
      IF (LDTRK.GT.0) IQ(LDTRK)=IBSET(IQ(LDTRK),BIT)
      IF (LFDCT.GT.0) IQ(LFDCT)=IBSET(IQ(LFDCT),BIT)
 1000 RETURN
      END
