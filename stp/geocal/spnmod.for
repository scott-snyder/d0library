       SUBROUTINE SPNMOD(L,NSEG)
C-----------------------------------------------------------------------------------
C
C      THIS SUBROUTINE CREATES THE OTHER NSEG-1 ROTATED MODULE BANKS FROM
C      THE FIRST BANK
C
C      ARGUMENTS:    L         LOCATION OF FIRST BANK (INPUT)
C                    NSEG      NUMBER OF BANKS CREATED (INCLUDING THE FIRST)
C                                   (INPUT)
C
C      ZEBRA BANKS CREATED:    CLGA
C
C      AUTHOR:       S KAHN          18 JUNE 1987
C
C-----------------------------------------------------------------------------
       IMPLICIT NONE
       INCLUDE 'D0$INC:ZEBSTP.INC'
       INCLUDE 'D0$INC:CLINKS.INC'
       INCLUDE 'D0$INC:CALGEO.INC'
       INCLUDE 'D0$INC:CLGA.DEF'
       INCLUDE 'D0$INC:REGION.DEF'
       INCLUDE 'D0$INC:PI.DEF'
C
       COMMON / LCLGA / LCLGA, LFIRST, LTUBE, LORIG
       INTEGER LCLGA(2), LFIRST, LTUBE, LORIG, MCLGA(5), NSEG, L, N
       REAL DPSI, PAX(3)
C
C
       MCLGA(1) = IQ(L-4)             ! bank name
       MCLGA(2) = IQ(L-3)             ! # links
       MCLGA(3) = IQ(L-2)             ! # structural links
       MCLGA(4) = IQ(L-1)             ! # data words
       MCLGA(5) = IOCLGA
C
       LFIRST = L                     ! Save first bank - it is in temp link
       LQCLGA = L
       DPSI = Q(LFIRST+IGDPHI)
C
       DO 100 N = 2, NSEG
       CALL MZLIFT(IXCDV,LQCLGA,LQCLGA,0,MCLGA,0)
       CALL UCOPY(Q(LFIRST+1),Q(LQCLGA+1),MCLGA(4))
       IQ(LQCLGA) = IQ(LFIRST)
       IQ(LQCLGA+IGIDEN) = IQ(LFIRST+IGIDEN) + (N-1)*ICMINC
       Q(LQCLGA+IGPHIC) = Q(LFIRST+IGPHIC) + (N-1)*DPSI
       PAX(1) = 0.
       PAX(2) = 0.
       PAX(3) = (N-1)*DPSI
       CALL ROTMLT(PAX,Q(LFIRST+IGTHTE),Q(LQCLGA+IGTHTE))
       LQ(LQCLGA-IZCMAT) = LQ(LFIRST-IZCMAT)
       LQ(LQCLGA-IZCLGI) = LQ(LFIRST-IZCLGI)
       LQ(LQCLGA-IZCSHA) = LQ(LFIRST-IZCSHA)
  100  CONTINUE
C
       RETURN
       END

