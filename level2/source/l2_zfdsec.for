      SUBROUTINE L2_ZFDSEC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : temporarily fill part of DSEC bank in Level 2
C-
C-   Inputs  : in CDLOCA : layer and sector
C-   Outputs : part of DSEC bank is filled
C-
C-   Created  11-AUG-1992   Qizhong Li-Demarteau   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CDCLNK.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$INC:CDLOCA.INC'

      INTEGER KPDCDA, KPDSEC, LPUL, J,NHITS,IPDSEC,IPDCDA
      INTEGER I,LAY,SEC,NWHIT, NDCDA, NDSEC, LHIT
      INTEGER LDTMW, IPTM, P1, P2
C
      REAL TIME, TZER, VELO, SOFF
C----------------------------------------------------------------------
C
      KPDCDA = LDCDA( SECTOR, LAYER )
      KPDSEC = LDSEC( SECTOR, LAYER )
      NDCDA = IQ( KPDCDA+2 )
      LPUL  = IQ( KPDCDA+3 )
      NHITS=0
      NDSEC = IQ( KPDSEC + 2 )
      LHIT  = IQ( KPDSEC + 3 )
      LDTMW = LC( LDTMH -(LAYER+1) )
      IPDSEC= 2*NDSEC + 3
      DO 10 WIRE=0,NDSEC-1
        NWHIT = IQ( KPDCDA + 4+WIRE )
        NHITS = NHITS + NWHIT
        IQ (KPDSEC+4+WIRE) = NWHIT
        IF (NWHIT .NE. 0) THEN
          IQ(KPDSEC+4+NDSEC+WIRE) = IPDSEC      ! Insert pointers in DSEC
          IPDCDA = IQ( KPDCDA+4+NDCDA+WIRE )
          SOFF = C( LC( LDGEH-3 ) + 26 + WIRE )
          IPTM = LDTMW + (SECTOR*IC(LDTMW+4)+WIRE)*IC(LDTMW+3) + 4
          TZER = C( IPTM + 1 )
          VELO = C( IPTM + 2 )
          DO 30 J = 1, NWHIT
            P1 = KPDCDA + IPDCDA
            P2 = KPDSEC + IPDSEC
            IQ(P2+1) = IQ(P1+1)
            TIME     = Q(P1+2)-TZER
            Q (P2+2) = SOFF + VELO * TIME
            Q (P2+3) = SOFF - VELO * TIME
            Q (P2+5) = VELO * Q(P1+6)
            Q (P2+6) = 9999.
            CALL MVBITS( IQ(P1+8), 0, 8, IQ(P2+9), 8 )    ! status byte
            IQ(P2+10)= IPDCDA
            IPDSEC = IPDSEC  + LHIT
            IPDCDA = IPDCDA  + LPUL
   30     CONTINUE
        ELSE
          IQ(KPDSEC+4+NDSEC+WIRE) = 0
        ENDIF
   10 CONTINUE
      IQ (KPDSEC+1)=NHITS            ! Insert Total # Of Hits In Sector
C
  999 RETURN
      END
