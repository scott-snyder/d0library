      SUBROUTINE ZFDSEC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill DSEC bank from DCDA datas
C-
C-   Inputs  : in CDLOCA : layer and sector
C-   Outputs : DSEC bank is filled
C-
C-   Created  ???????????
C-   Updated  27-JUL-1987   Olivier Callot
C-   Updated  10-AUG-1992   Qizhong Li-Demarteau  use a factor from RCP
C-                                  file to adjust velocities and gains
C-   Updated  28-OCT-1992   Domenico Pizzuto  Added switches from RCP: 
C-                                            VPLVMI, SWDERR and D0CFUN
C-   Updated  17-MAY-1993   Qizhong Li-Demarteau   fixed the crashing on
C-                                          MC data due to VPLVMI switch
C-                                                  
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CDCLNK.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$INC:CDLOCA.INC'
C
      INTEGER KPDCDA, KPDSEC, LPUL, J,NHITS,IPDSEC,IPDCDA
      INTEGER CONT,Z,P1,P2,LSW,LSW1,T1,T2,LDTVA,IPVA,ERR
      INTEGER I,LAY,SEC,NWHIT, NDCDA, NDSEC, LHIT,GZDTVA
      INTEGER LDGNL, LDTMW, IPGN, IPTM, IER
      REAL TIME, GAIN, TZER, VELO, SOFF, VFACTR, GFACTR
      REAL VELP, VELM
      LOGICAL FIRST, EZERROR, MCDATA
      LOGICAL VPLVMI,SWDERR,D0CFUN
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','ZFDSEC',
     &    'Unable to find bank DTRAKS_RCP','W')
        ENDIF
        CALL EZGET('VFACTR',VFACTR,IER)
        IF (IER .NE. 0) VFACTR = 1.0
        CALL EZGET('GFACTR',GFACTR,IER)
        IF (IER .NE. 0) GFACTR = 1.0
        IF (IQ(LHEAD+1) .GT. 1000) MCDATA = .TRUE.
        IF (.NOT. MCDATA) THEN
          CALL EZGET_l('VPLVMI',VPLVMI,ERR)
          CALL EZGET_l('SWDERR',SWDERR,ERR)
          CALL EZGET_l('D0CFUN',D0CFUN,ERR)            
        ENDIF
        CALL EZRSET
      ENDIF
C
C ****  Fill DSEC bank from DCDA, with time => cm and FADC => M.I.P.
C ****  conversion .
C
      KPDCDA = LDCDA( SECTOR, LAYER )
      KPDSEC = LDSEC( SECTOR, LAYER )
      NDCDA = IQ( KPDCDA+2 )
      LPUL  = IQ( KPDCDA+3 )
      NHITS=0
      NDSEC = IQ( KPDSEC + 2 )
      LHIT  = IQ( KPDSEC + 3 )
      LDGNL = LC( LDGNH -(LAYER+1) )
      LDTMW = LC( LDTMH -(LAYER+1) )
      IF (.NOT. MCDATA) LDTVA = GZDTVA (LAYER)
      IPDSEC= 2*NDSEC + 3
      DO 10 WIRE=0,NDSEC-1
        NWHIT = IQ(KPDCDA + 4+WIRE)
        NHITS = NHITS + NWHIT
        IQ (KPDSEC+4+WIRE) = NWHIT
        IF (NWHIT .NE. 0) THEN
          IQ(KPDSEC+4+NDSEC+WIRE) = IPDSEC      ! Insert pointers in DSEC
          IPDCDA = IQ(KPDCDA+4+NDCDA+WIRE)
          SOFF = C(LC(LDGEH-3) + 26 + WIRE)
          IPGN = LDGNL + (SECTOR*IC(LDGNL+4)+WIRE)*IC(LDGNL+3) + 4
          GAIN = C(IPGN + 1) * GFACTR
          IPTM = LDTMW + (SECTOR*IC(LDTMW+4)+WIRE)*IC(LDTMW+3) + 4
          TZER = C(IPTM + 1)
          VELO = C(IPTM + 2) * VFACTR
          IF (.NOT. MCDATA) THEN
            IPVA = LDTVA+IC (LDTVA+2)*(IC (LDTVA+1)*SECTOR+WIRE)+2
            VELP = C (IPVA+1)                       ! + side SW velocity
            VELM = C (IPVA+2)                       ! - side SW velocity
          ENDIF
          DO 30 J = 1, NWHIT
            P1 = KPDCDA + IPDCDA
            P2 = KPDSEC + IPDSEC
            IQ(P2+1) = IQ(P1+1)
            TIME     = Q(P1+2) - TZER
            IF (VPLVMI .AND. (.NOT.MCDATA)) THEN
              Q (P2+2) = SOFF + VELP * TIME         ! + side
              Q (P2+3) = SOFF - VELM * TIME         ! - side
            ELSE
              Q (P2+2) = SOFF + VELO * TIME
              Q (P2+3) = SOFF - VELO * TIME
            END IF
            IF (.NOT. MCDATA .AND. D0CFUN) 
     &          CALL DNLCOR (WIRE,LAYER,Q (P2+2),Q (P2+3))
            IF (.NOT. MCDATA .AND. SWDERR) THEN
              CALL DXYERR (WIRE,Q (P2+2),Q (P2+5))
            ELSE
              Q (P2+5) = VELO * Q(P1+6)
            END IF
            Q (P2+6) = 9999.
            Q (P2+7) = GAIN * Q(P1+3)
            Q (P2+8) = GAIN * Q(P1+7)
            CALL MVBITS( IQ(P1+8), 0, 8, IQ(P2+9), 8 )    ! status byte
            IQ(P2+10)= IPDCDA
            IPDSEC = IPDSEC + LHIT
            IPDCDA = IPDCDA + LPUL
   30     CONTINUE
        ELSE
          IQ(KPDSEC+4+NDSEC+WIRE) = 0
        ENDIF
   10 CONTINUE
      IQ (KPDSEC+1)=NHITS            ! Insert Total # Of Hits In Sector
  999 RETURN
      END
