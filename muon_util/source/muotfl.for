      SUBROUTINE MUOTFL(IMUOT,NHW,NHS,IQUAD,F1,F2,F3,F4,XI,YI,ZI,
     & XO,YO,ZO,DXI,DYI,DZI,DXO,DYO,DZO,XB,XNB,BDL,P,DP,ECAL,EFE,XDEL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book and fill MUOT bank
C-
C-   Inputs : 
C==========================================================================
C    BANK MUOT    --     MUON TRACK BANK
C    ==== ====
C  LQ    Q/IQ
C____________________________________________________________________________
C  L-3   struc  to scint hits on track  (MSHT)
C  L-2   struc  to SAMUS hits on track  (STTH)
C  L-1   struc  to wide angle hits on track (MHTT)
C    0   next   to next MUOT
C   +1   UP     TO MTRH
C   +2   origin to MTRH for first; previous MUOT for others
C............................................................................
C         1  I  Number of wide angle points on track
C         2  I  Number of small angle points on track
C         3  I  Quadrant 
C         4  I  FLAG WORD 1 
C         5  I  FLAG WORD 2
C         6  I  FLAG WORD 3 
C         7  I  FLAG WORD 4 
C         8  F  X inside magnet: this will be at the closest a-layer hit to
C         9  F  Y inside       : the calorimeter. if there are no a layer
C        10  F  Z inside       : hits in the fit, the vertex point is used
C        11  F  X outside magnet: NOMINALLY AT CENTER OF MAGNET
C        12  F  Y outside
C        13  F  Z outside
C        14  F  X direction cosine inside magnet
C        15  F  Y direction cosine
C        16  F  Z direction cosine
C        17  F  X direction cosine outside magnet
C        18  F  Y direction cosine
C        19  F  Z direction cosine
C        20  F  Bend view quality of fit (in CM=RMS of drift residuals)
C        21  F  Nonbend view quality of fit (in CM=RMS of pad and delta T)
C        22  F  BDL 
C        23  F  Momentum (signed)
C        24  F  Momentum error
C        25  F  Eloss in calorimeter
C        26  F  Eloss in iron
C        27  F  Time division quality of fit (in CM=RMS of delta T points)
C========================================================================
C-
C-   Outputs : IMUOT    Track number
C-
C-   Created : 29-APR-1994   M. Fortner
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LMTRH,GZMTRH,LMUOT,IMUOT
      INTEGER NHW,NHS,IQUAD,F1,F2,F3,F4
      REAL XI,YI,ZI,XO,YO,ZO,DXI,DYI,DZI,DXO,DYO,DZO
      REAL XB,XNB,BDL,P,DP,ECAL,EFE,XDEL
C
      LMTRH=GZMTRH(0)
      IQ(LMTRH+1)=IQ(LMTRH+1)+1   ! INDEX NUMBER OF TRACKS
      IMUOT=IQ(LMTRH+1)
      CALL BKMUOT(0,0,LMUOT)
C
      IQ(LMUOT+1)=NHW
      IQ(LMUOT+2)=NHS 
      IQ(LMUOT+3)=IQUAD
      IQ(LMUOT+4)=F1
      IQ(LMUOT+5)=F2
      IQ(LMUOT+6)=F3
      IQ(LMUOT+7)=F4
      Q(LMUOT+ 8)=XI
      Q(LMUOT+ 9)=YI
      Q(LMUOT+10)=ZI
      Q(LMUOT+11)=XO
      Q(LMUOT+12)=YO
      Q(LMUOT+13)=ZO
      Q(LMUOT+14)=DXI
      Q(LMUOT+15)=DYI
      Q(LMUOT+16)=DZI
      Q(LMUOT+17)=DXO
      Q(LMUOT+18)=DYO
      Q(LMUOT+19)=DZO
      Q(LMUOT+20)=XB
      Q(LMUOT+21)=XNB
      Q(LMUOT+22)=BDL
      Q(LMUOT+23)=P
      Q(LMUOT+24)=DP
      Q(LMUOT+25)=ECAL
      Q(LMUOT+26)=EFE
      Q(LMUOT+27)=XDEL
C
      RETURN
      END
