      REAL FUNCTION inv_SSPOLE(MGMS,MUSQ,AS,amtp,amqkss,amt1ss,amt2ss,
     &        amblss,ambrss)
c
c     Following routines taken and altered from ISASUSY package.
c
      implicit none
      REAL amtp,amqkss,amblss,ambrss
      REAL*8 MGMS,MUSQ,AS,amt1ss,amt2ss
      real mgsq,fac
      COMPLEX SSB1
      include 'D0$SPYTHIA$INC:SSINF.INC'
      XLAM = LOG(MUSQ)
      MGSQ = MGMS*MGMS
      FAC = -REAL(SSB1(MGSQ,0.,REAL(AMQKSS)))
      FAC = 8.0*FAC-REAL(SSB1(MGSQ,REAL(AMTP),REAL(AMT1SS))+
     $SSB1(MGSQ,REAL(AMTP),REAL(AMT2SS))+SSB1(MGSQ,4.0,REAL(AMBLSS))+
     $SSB1(MGSQ,4.0,REAL(AMBRSS)))
      FAC = FAC + 12.0 + 9.0*LOG(MUSQ/MGSQ)
      inv_SSPOLE = MGMS/(1.0 + .0796*AS*FAC )
      RETURN
      END
