      SUBROUTINE PRTLYR(LOUT,ILAY)
C ----------------------------------------------------------------------
C -
C -   PURPOSE AND METHODS : PRINT OF TRD ZEBRA BANK TLYR (IDEALIZED HITS
C-                          IN TRD LAYER ILAY)
C -
C -   INPUTS  :  ILAY=TRD LAYER NUMBER
C-               LOUT=OUTPUT LOGICAL UNIT
C -   OUTPUTS :
C -
C-   Created  11-JAN-1988   A. ZYLBERSTEJN
C-   Updated  15-JUN-1989   A. Zylberstejn
C-
C-
C ----------------------------------------------------------------------
      IMPLICIT NONE
C
C      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:FADCCN.INC'
C      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
C      INCLUDE 'D0$INC:TRDLNK.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER I,ILAY,J,K,LL,LOUT,ND,NM,NPR
      INTEGER GZTRDH,LTLYR,LTRDH
      REAL DEDT(NMFADC)
C      ------------------------------------------------------------------
C      IF(IDEBUG.EQ.0)RETURN
      IF(ILAY.LE.0 .OR. ILAY.GT.6)THEN
        CALL INTMSG(' Wrong layer number for PRTLYR')
        CALL EXIT
      END IF
C  book TRDH if needed
      LTRDH=GZTRDH()
      IF(LTRDH.LE.0) THEN
        CALL INTMSG(' Problem_trd in PRTLYR: BANK LTRDH NOT BOOKED')
        GO TO 999
      END IF
      LTLYR = LQ (LTRDH-ILAY )
      IF(LTLYR.LE.0)THEN
        WRITE(LOUT,*)' Problem_trd in PRTLYR: bank LTLYR',ILAY,
     + ' NOT BOOKED'
        GO TO 999
      END IF
      LL=LQ(LTLYR)
      NPR=0
   20 IF(LL.LE.0)GO TO 999
      CALL TRDUNP(LL,DEDT)  !UN-PACK THE INFORMATION
      ND=IQ(LL-1)
      IF(IQ(LL+ND).GT.1000)GO TO 46  ! Select anodes
      WRITE(LOUT,1000)IQ(LL+ND),ILAY,IQ(LL+ND-1)
      NM=NFADC/10
      NM=(NM+1)*10+1
      NPR=NPR+1
      DO 40 I=1,NM
        J=(I-1)*10+1
        WRITE(LOUT,1010)J,(DEDT(K)*100.,K=J,J+9)
   40 CONTINUE
      WRITE(LOUT,1010)NM,(DEDT(K)*100.,K=NM,NFADC)
   46 LL=LQ(LL)
      IF(NPR.GT.10)GO TO 20
  999 RETURN
 1000 FORMAT(' dE/dT*100. for wire number',I4,' layer ',I2,
     +' for GEANT track number:',I4)
 1010 FORMAT(I4,10F7.2)
      END
