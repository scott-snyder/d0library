      SUBROUTINE PRHTLY(ILAY)
C ----------------------------------------------------------------------
C -
C -   PURPOSE AND METHODS : PRINT OF TRD ZEBRA BANK HTLY (IDEALIZED HITS
C-                          IN TRD LAYER ILAY)
C -
C -   INPUTS  :  ILAY=TRD LAYER NUMBER
C -   OUTPUTS :
C -
C-   Created  11-JAN-1988   A. ZYLBERSTEJN
C-
C-
C ----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:FADCCN.INC'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:TRDHLN.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER I,ILAY,J,K,LL,ND,NM
      REAL DEDT(NMFADC)
C      ------------------------------------------------------------------
      LL=LQ(LTLYR(ILAY))
   20 IF(LL.LE.0)GO TO 999
      CALL TRDUNP(LL,DEDT)  !UN-PACK THE INFORMATION
      ND=IQ(LL-1)
      WRITE(LOUT,1000)IQ(LL+ND),IQ(LL+ND-1)/100,MOD(IQ(LL+ND-1),100),
     +ILAY
      NM=NFADC/10
      DO 40 I=1,NM
        J=(I-1)*10+1
        WRITE(LOUT,1010)J,(DEDT(K),K=J,J+9)
   40 CONTINUE
      NM=(NM+1)*10+1
      WRITE(LOUT,1010)NM,(DEDT(K),K=NM,NFADC)
      LL=LQ(LL)
      IF(PTRD.GE.10)GO TO 20
  999 RETURN
 1000 FORMAT(' dE/dT for wire number',I4,' for GEANT track number:',
     +I4,' stack number: ',I3,' layer ',I2)
 1010 FORMAT(I4,10F7.2)
      END
