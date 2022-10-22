      SUBROUTINE PCALEG
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make LEGO plot for D0 calorimeter
C-                         EM energy in red, hadronic energy in blue
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  13-NOV-1988   Sharon Hagopian
C-   Updated  19-DEC-1989   Lupe Rosas  Using PIXIE color table
C-   Updated  01-MAR-1990   Nobu. Oshima (Clean up local INC's files)
C-   Updated  10-SEP-1990   Harrison B. Prosper
C-       Use routine PU_SET_RCP_BANK
C-   Modified 12-JAN-1992   Nobu Oshima - Use BYTE_ORDER.PARAMS.
C-   Modified 12-APR-1992   Nobu Oshima
C-       Ignore RUN#/EVT# for Online Mode, add layer handling + ICD/MG.
C-   Modified 15-JUL-1992   Nobu Oshima
C-       Use the param 'FIXEMAX' in the combined views
C-   Modified 02-MAR-1994   Nobu Oshima
C-       Remove "IF (ENER .LE. 0.) GO TO 100" before getting ESUM
C-   Modified 18-APR-1994   Nobu Oshima
C-       Added 'CALEGO EMAX' parameter to look at energy flow in CAL.
C-   Modified 25-MAY-1994   Nobu Oshima
C-       Scan CAID Bank to find hot cell candidates and tell you
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
C-
      INTEGER NX,NY,IMARK
      REAL XMIN,XMAX,YMIN,YMAX,ZMAX
      CHARACTER*3 COL1, COL2
      CHARACTER*24 XLAB,YLAB,ZLAB
      CHARACTER*21 PLTITL
      INTEGER NXMIN,NYMIN,N
      INTEGER NXG,NYG
      INTEGER GZCAEP,LCAEP,LDCAEP,GZCAID,LCAID,LDCAID
      INTEGER INDCES,NRP,NCH
      REAL ZSCAL
      REAL ARRAY1(NPHIL,2*NETAL),ARRAY2(NPHIL,2*NETAL)
      REAL ENER,ESUM,EMIN,EMAX,ETWSUM,ESUBT
      REAL MXEMBN,MXHDBN,MXIMBN,MXTOTBN
      REAL MXEMHS,MXHDHS,MXIMHS,MXTOTHS
      INTEGER IL,IP,IE,IFLAG,I, IETA, IER
      INTEGER LAYMIN, LAYMAX, IX, JBIT, IPIQ
      CHARACTER*32 MESS1,MESS2
      CHARACTER*52 EXMESS
      CHARACTER*65 TMPMESS
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
      CALL PUGETV('CALEGO EMIN',EMIN)
      CALL PUGETV('CALEGO EMAX',EMAX)
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
      IPIQ = 0
      LCAEP = GZCAEP()
      IF(LCAEP.LE.0) THEN
        CALL INTMSG(' CAEP Bank does not exist, try CAEQ ...')
        CALL CAEQ_TO_CAEP
        LCAEP = GZCAEP()
        IPIQ = 1
        IF(LCAEP.LE.0) THEN
          CALL PUMESS(' CAEQ Bank is not available, too ...')
          GO TO 999
        ENDIF
      ENDIF
C-
      NRP   = IQ(LCAEP+2)
      NCH   = IQ(LCAEP+3)
      IF(LCAEP.GT.0 .AND. NCH.GT.0) THEN
        IX=JBIT(IQ(LCAEP+4),6)
      ENDIF
      CALL VZERO(ARRAY1,NPHIL*2*NETAL)
      CALL VZERO(ARRAY2,NPHIL*2*NETAL)
      ESUM = 0.
      DO 100 I=1,NCH
        LDCAEP = LCAEP+(I-1)*NRP
        INDCES = IQ(LDCAEP+4)
        ENER   = Q(LDCAEP+5)
        IE     = BYTES(BYTE4)
        IP     = BYTES(BYTE3)
        IL     = BYTES(BYTE2)
        IFLAG  = BYTES(BYTE1)
C-
        IF ( BTEST(IFLAG,7) ) THEN
          WRITE(TMPMESS,1000) IE,IP,IL,ENER
          CALL INTMSG(TMPMESS)
        ENDIF
C-
        IF (IL.LT.LAYMIN .OR. IL.GT.LAYMAX) GO TO 100
        ESUM = ESUM + ENER
        IF (IE .LT. 0) THEN
          IETA = IE + NETAL + 1
        ELSE
          IETA = IE + NETAL
        ENDIF
C-
C- ICD/MG begin
        IF( ICDMG ) THEN
          IF(IL.EQ.MNLYMG .OR. IL.EQ.MXLYMG) THEN
            ARRAY1(IP,IETA) = ARRAY1(IP,IETA) + ENER
          ELSEIF(IL .EQ. LYICD) THEN
            ARRAY2(IP,IETA) = ARRAY2(IP,IETA) + ENER
          ENDIF
        ENDIF
C- ICD/MG end
        IF(IL.GE.MNLYEM .AND. IL.LE.MXLYEM) THEN
          ARRAY1(IP,IETA) = ARRAY1(IP,IETA) + ENER
        ELSEIF(IL.GE.MNLYFH .AND. IL.LE.MXLYCH) THEN
          ARRAY2(IP,IETA) = ARRAY2(IP,IETA) + ENER
        ENDIF
C- Cut by EMAX, if EMAX > 0.
        ETWSUM = ARRAY1(IP,IETA) + ARRAY2(IP,IETA)
        IF (ETWSUM.GT.EMAX .AND. EMAX.GT.0.) THEN
          IF (ARRAY1(IP,IETA) .GT. EMAX) THEN
            ARRAY1(IP,IETA) = EMAX
            ARRAY2(IP,IETA) = 0.
          ELSEIF (ARRAY2(IP,IETA) .GT. EMAX) THEN
            ARRAY2(IP,IETA) = EMAX - ARRAY1(IP,IETA)
          ELSE
            ESUBT = ETWSUM - EMAX
            ARRAY2(IP,IETA) = ARRAY2(IP,IETA) - ESUBT
          ENDIF
        ENDIF
  100 CONTINUE
C-
C--- Scan CAID Bank to find hot cell candidates
C-
      IF(GZCAID() .GT. 0) THEN
        LCAID = GZCAID()
        NRP   = IQ(LCAID+2)
        NCH   = IQ(LCAID+4)
        DO 200 I=1,NCH
          LDCAID = LCAID + (I-1)*NRP
          INDCES = IQ(LDCAID+8)
          ENER   = Q(LDCAID+9)
          IE     = BYTES(BYTE4)
          IP     = BYTES(BYTE3)
          IL     = BYTES(BYTE2)
          IFLAG  = BYTES(BYTE1)
          WRITE(TMPMESS,1100) IE,IP,IL,ENER
          CALL INTMSG(TMPMESS)
  200   CONTINUE
      ENDIF
C-
 1000 FORMAT(1X,'%Run by Run Defined-Hot Cell at IE=',I3,
     &  ' IP=',I3,' IL=',I3,' E=',F7.2) 
 1100 FORMAT(1X,'%AIDA(Event by Event)-Hot Cell at IE=',I3,
     &  ' IP=',I3,' IL=',I3,' E=',F7.2) 
C---
C-
      NY=NETAL*2
      NX=NPHIL
      YMIN=-NETAL
      YMAX=NETAL
      XMIN=1.
      XMAX=NPHIL
      ZMAX=-1.
      XLAB='IPHI'
      YLAB='IETA'
      ZLAB='E'
      IF( ICDMG ) THEN
        COL1= 'MAG'   ! Magenta for MG
        COL2= 'YEL'   ! Yellow for ICD
        PLTITL='ENERGY ICD/MG ETA-PHI'
      ELSE
        COL1= 'RED'   ! Red for EM
        COL2= 'CYA'   ! Cyan for HAD
        PLTITL='ENERGY CAEP ETA-PHI'
        IF (IPIQ .EQ. 1) THEN
          PLTITL='ENERGY CAEQ ETA-PHI'
        ENDIF
      ENDIF
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
        IF(LAYMIN.GE.MNLYEM .AND. LAYMAX.LE.MXLYEM) THEN
          ZMAX = MXEMBN
        ELSEIF(LAYMIN.GE.MNLYFH .AND. LAYMAX.LE.MXLYFH) THEN
          ZMAX = MXHDBN
        ELSE
          ZMAX = MXTOTBN
        ENDIF
      ENDIF
      IF (EMAX .GT. 0) ZMAX = EMAX
C-
      CALL P2LEGO(NX,XMIN,XMAX,NY,YMIN,YMAX,EMIN,ZMAX,PLTITL,
     X     XLAB,YLAB,ZLAB,COL1,COL2,ARRAY1,ARRAY2,
     X     NXMIN,NYMIN,NXG,NYG,N,ZSCAL,IMARK,CALPLOT)
C-
C--- Draw messages
      IF (IX .EQ. 0) THEN
        WRITE(MESS1,2000) EMIN
        WRITE(MESS2,2020) ESUM
      ELSE
        WRITE(MESS1,2100) EMIN
        WRITE(MESS2,2120) ESUM
      ENDIF
      CALL PCTEXT(1,MESS1)
      CALL PCTEXT(2,MESS2)
C-
      IF (LAYMIN.NE.1 .OR. LAYMAX.NE.17) THEN
        IF ( ICDMG ) THEN
          WRITE(EXMESS,3000) LAYMIN,LAYMAX
          CALL PCTEXT(3,EXMESS)
        ELSEIF(LAYMAX.LT.MNLYMG .OR. LAYMIN.GT.MXLYMG) THEN
          WRITE(EXMESS,3000) LAYMIN,LAYMAX
          CALL PCTEXT(3,EXMESS)
        ELSE
          WRITE(EXMESS,3100) LAYMIN,LAYMAX
          CALL PCTEXT(3,EXMESS)
        ENDIF
      ENDIF
C-
 2000 FORMAT(' CALEGO EMIN =',F4.1,' GeV')
 2020 FORMAT(' CAEP E SUM =',F6.1,' GeV')
 2100 FORMAT(' CALEGO EMIN =',F6.0,' ADC counts')
 2120 FORMAT(' CAEP E SUM =',F8.0,' ADC counts')
 3000 FORMAT(' Layer ',I2,'-',I2,' are selected')
 3100 FORMAT(' Layer ',I2,'-',I2,' are selected',
     &       '[8-10(ICD&MG) are excluded]')
C---
C-
  999 RETURN
      END
