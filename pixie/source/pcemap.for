      SUBROUTINE PCEMAP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make Colored Energy Map with DTRK and FDCT
C-                         Banks information.
C-
C-   Created  28-APR-1994   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
C-
      INTEGER NX,NY,IMARK
      REAL    XMIN,XMAX,YMIN,YMAX,ZMAX
      CHARACTER*24 XLAB,YLAB,ZLAB
      CHARACTER*27 PLTITL
      INTEGER NXMIN,NYMIN,N
      INTEGER NXG,NYG
      INTEGER GZCAEP, LCAEP,LDCAEP,INDCES,NRP,NCH
      REAL    ZSCAL
      REAL    ARRAY(NPHIL,2*NETAL)
      INTEGER IARRAY(NPHIL,2*NETAL)
      REAL    ENER,ESUM,EMIN,TK_PHI,TK_THE,TANTHE,ETA
      REAL    MXEMBN,MXHDBN,MXIMBN,MXTOTBN
      REAL    MXEMHS,MXHDHS,MXIMHS,MXTOTHS
      INTEGER IL,IP,IE,IFLAG,I, IETA, IER
      INTEGER LAYMIN, LAYMAX, IX, JBIT
      INTEGER LDTRK,LFDCT,GZDTRK,GZFDCT
      CHARACTER*32 MESS1
      CHARACTER*52 EXMESS
      CHARACTER*48 TMPMESS
      LOGICAL CALPLOT, ICDMG, BTEST
      LOGICAL EZERROR, FIXEMAX,LSUPHOT
      BYTE BYTES(4)
      EQUIVALENCE (BYTES(1),INDCES)
C-
      DATA IMARK/0/
C----------------------------------------------------------------------
C-
C--- Get minimum energy and min./max layer for cell to be plotted...
C--- Select correct RCP bank
C-
      CALL EZPICK('PX_CALDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PCALEG',
     &    'Unable to pick RCP bank PX_CALDIS_RCP','W')
        GOTO 999
      ENDIF
C-
      EMIN = 0.
      CALL PUGET_i('CAL LAYMIN',LAYMIN)
      CALL PUGET_i('CAL LAYMAX',LAYMAX)
      CALL PUGET_l('CAL FIXEMAX',FIXEMAX)
      CALL PUGET_l('CAL SUPHOTC',LSUPHOT)
C-
      CALL EZRSET
C-
      IF (LAYMIN .LT. 1)  LAYMIN = 1
      IF (LAYMAX .GT. 17) LAYMAX = 17
      IF (LAYMIN .GT. LAYMAX) LAYMIN = LAYMAX
      ICDMG = .FALSE.
      IF(LAYMIN.GE.MNLYMG .AND. LAYMAX.LE.MXLYMG) ICDMG = .TRUE.
C-
C--- Scan CAEP Bank
C-
      IF(GZCAEP().LE.0) THEN
        CALL PUMESS(' CAEP BANK DOES NOT EXIST')
        GO TO 999
      ENDIF
      LCAEP = GZCAEP()
      NRP   = IQ(LCAEP+2)
      NCH   = IQ(LCAEP+3)
      IF(LCAEP.GT.0 .AND. NCH.GT.0) THEN
        IX=JBIT(IQ(LCAEP+4),6)
      ENDIF
      CALL VZERO(ARRAY,NPHIL*2*NETAL)
      ESUM = 0.
      DO 100 I=1,NCH
        LDCAEP = LCAEP+(I-1)*NRP
        INDCES = IQ(LDCAEP+4)
        ENER   = Q(LDCAEP+5)
        IE     = BYTES(BYTE4)
        IP     = BYTES(BYTE3)
        IL     = BYTES(BYTE2)
        IFLAG  = BYTES(BYTE1)
        IF ( BTEST(IFLAG,7) .AND. LSUPHOT) THEN
          WRITE(TMPMESS,1000) IE,IP,IL,ENER
          CALL INTMSG(TMPMESS)
          GO TO 100
        ENDIF
        IF (IL.LT.LAYMIN .OR. IL.GT.LAYMAX) GO TO 100
        ESUM = ESUM + ENER
        IF (IE .LT. 0) THEN
          IETA = IE + NETAL + 1
        ELSE
          IETA = IE + NETAL
        ENDIF
C-
        IF(IL.GE.MNLYEM .AND. IL.LE.MXLYCH) THEN
          ARRAY(IP,IETA) = ARRAY(IP,IETA) + ENER
        ENDIF
C-
  100 CONTINUE
C-
 1000 FORMAT(1X,'IETA=',I3,' IPHI=',I3,' ILAYER=',I3,' ENERGY=',F7.2)
C-
C--- Scan DTRK and FDCT Banks
C--- CDC
      CALL VZERO_i(IARRAY,NPHIL*2*NETAL)
      LDTRK = GZDTRK(0)
  200 IF (LDTRK .LE. 0)   GO TO 250
      TK_PHI = Q(LDTRK+6)
      TK_THE = Q(LDTRK+9)
      IP = (TK_PHI/TWOPI)*64. + 1.
      TANTHE = TAN(TK_THE*.5)
      IF (TANTHE .LE. 0.)   TANTHE = .001
      ETA  = -ALOG(TANTHE)
      IETA = 10.*(ETA+3.7) + 1.
      IF(IP.GT.0.AND.IP.LE.NPHIL.AND.IETA.GT.0.AND.IETA.LE.2*NETAL) THEN
        IARRAY(IP,IETA) = 1
      ENDIF
      LDTRK = LQ(LDTRK)
      GO TO 200
C--- FDC
  250 LFDCT = GZFDCT(0)
  300 IF (LFDCT .LE. 0)   GO TO 400
      TK_PHI = Q(LFDCT+6)
      TK_THE = Q(LFDCT+22)
      IP = (TK_PHI/TWOPI)*64. + 1.
      TANTHE = TAN(TK_THE*.5)
      IF (TANTHE .LE. 0.)   TANTHE = .001
      ETA  = -ALOG(TANTHE)
      IETA = 10.*(ETA+3.7) + 1.
      IF(IP.GT.0.AND.IP.LE.NPHIL.AND.IETA.GT.0.AND.IETA.LE.2*NETAL) THEN
        IARRAY(IP,IETA) = 2
      ENDIF
      LFDCT = LQ(LFDCT)
      GO TO 300
C-
C--- Make Lego plot
C-
  400 NY=NETAL*2
      NX=NPHIL
      YMIN=-NETAL
      YMAX=NETAL
      XMIN=1.
      XMAX=NPHIL
      ZMAX=-1.
      XLAB='IPHI'
      YLAB='IETA'
      ZLAB='DTRK-FDCT'
      PLTITL='COLORED CAEP ENERGY ETA-PHI'
      NXMIN=1
      NYMIN=1
      NXG=1
      NYG=1
      N=NX
      ZSCAL=.2
      CALPLOT = .TRUE.                 ! Cal Plot E
C-
      IF ( FIXEMAX )   THEN
        CALL PC_GET_MAXEORET(1,MXEMBN,MXHDBN,MXIMBN,MXTOTBN,
     &                         MXEMHS,MXHDHS,MXIMHS,MXTOTHS)
        ZMAX = MXTOTBN
      ENDIF
C-
      CALL PCCOLL(NX,XMIN,XMAX,NY,YMIN,YMAX,EMIN,ZMAX,PLTITL,XLAB,
     &  YLAB,ZLAB,ARRAY,IARRAY,NXMIN,NYMIN,NXG,NYG,N,ZSCAL,IMARK)
C-
C--- Draw messages
      IF (IX .EQ. 0) THEN
        WRITE(MESS1,202) ESUM
      ELSE
        WRITE(MESS1,212) ESUM
      ENDIF
      CALL PCTEXT(1,MESS1)
C-
      IF (LAYMIN.NE.1 .OR. LAYMAX.NE.17) THEN
        IF ( ICDMG ) THEN
          WRITE(EXMESS,302) LAYMIN,LAYMAX
          CALL PCTEXT(2,EXMESS)
        ELSEIF(LAYMAX.LT.MNLYMG .OR. LAYMIN.GT.MXLYMG) THEN
          WRITE(EXMESS,302) LAYMIN,LAYMAX
          CALL PCTEXT(2,EXMESS)
        ELSE
          WRITE(EXMESS,310) LAYMIN,LAYMAX
          CALL PCTEXT(2,EXMESS)
        ENDIF
      ENDIF
C-
  202 FORMAT(' CAEP E SUM =',F6.1,' GeV')
  212 FORMAT(' CAEP E SUM =',F8.0,' ADC counts')
  302 FORMAT(' Layer ',I2,'-',I2,' are selected')
  310 FORMAT(' Layer ',I2,'-',I2,' are selected',
     &       '[8-10(ICD&MG) are excluded]')
C---
C-
  999 RETURN
      END
