C****************************************************************
      SUBROUTINE TOP_TO_STOP
      implicit none
      include 'D0$SPYTHIA$INC:LUDAT1.INC'
      include 'D0$SPYTHIA$INC:LUDAT2.INC'
      include 'D0$SPYTHIA$INC:LUDAT3.INC'
      include 'D0$SPYTHIA$INC:SUSYPAR.INC'
      REAL GAMT
      COMMON/TWID/GAMT
      real FL,FR,TANW,PCM,XMT,XMN,XMS,ULALEM,ET
C.....Add on additional decay modes for the higgs, if needed
C.....Add top -> stop + chi01
C.....Modify decay channel 47
      XMT=PMAS(6,1)
      XMN=PMAS(66,1)
      XMS=PMAS(51,1)
      GAMT=0.0
      IF(XMT.gt.XMN+XMS) THEN
       ET=KCHG(6,1)/3.
       TANW=SQRT(PARU(102)/(1.-PARU(102)))
       FL=TANW*ET*Z(1,1)*SINST
     $ -PMAS(6,1)*Z(1,4)/(2.*PMAS(24,1)*sin(beta))*COSST
       FR=-(.5*Z(1,2)-TANW*(.5-ET)*Z(1,1))*COSST
     $ -PMAS(6,1)*Z(1,4)/(2.*PMAS(24,1)*sin(beta))*SINST
       PCM=SQRT((XMT**2-(XMN+XMS)**2)*(XMT**2-(XMN-XMS)**2))/2./XMT
       GAMT=.5*ULALEM(XMT**2)/PARU(102)*PCM/XMT**2
       GAMT=GAMT*((FL**2+FR**2)*(XMT**2+XMN**2-XMS**2)+4.*XMT*XMN*FL*FR)
c Correction added in later in pyofsh
c       DO I1=MDCY(6,2),MDCY(6,2)+MDCY(6,3)-1
c        BRAT(I1)=BRAT(I1)*PMAS(6,2)/(PMAS(6,2)+GAMT)
c       ENDDO
       MDME(47,1)=1
c       PMAS(6,2)=PMAS(6,2)+GAMT
c       BRAT(47)=GAMT/PMAS(6,2)
       KFDP(47,1)=66
       KFDP(47,2)=51
       print*,' GAMT = ',GAMT,' for (t,t~,chi0) = (',XMT,XMS,XMN,')'
      ENDIF
      return
      end
