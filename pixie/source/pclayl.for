      SUBROUTINE PCLAYL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make  Layer LEGO plot  for D0 calorimeter
C-                         EM energy in red, FH in green, CH in blue
C-                         and ICD/MG in yellow
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  16-AUG-1994   Nobuaki Oshima
C-   Modified 24-MAR-1995   Nobuaki Oshima
C-      Fixed the bug and added the function to plot CAEP negative Energy.
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
      CHARACTER*3 COL1,COL2,COL3,COL4
      CHARACTER*24 XLAB,YLAB,ZLAB, PLTITL
      INTEGER NXMIN,NYMIN,N
      INTEGER NXG,NYG
      INTEGER GZCAEP,LCAEP,LDCAEP,GZCAID,LCAID,LDCAID
      INTEGER INDCES,NRP,NCH
      REAL ZSCAL
      REAL ARRAY1(NPHIL,2*NETAL),ARRAY2(NPHIL,2*NETAL)
      REAL ARRAY3(NPHIL,2*NETAL),ARRAY4(NPHIL,2*NETAL)
      REAL ENER,ESUM,EMIN
      REAL MXEMBN,MXHDBN,MXIMBN,MXTOTBN
      REAL MXEMHS,MXHDHS,MXIMHS,MXTOTHS
      INTEGER IL,IP,IE,IFLAG,I, IETA, IER
      INTEGER LAYMIN, LAYMAX, IX, JBIT, IPIQ
      CHARACTER*32 MESS1,MESS2
      CHARACTER*65 TMPMESS
      LOGICAL CALPLOT, BTEST
      LOGICAL EZERROR, FIXEMAX,LSUPHOT,LNEGENR
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
        CALL ERRMSG('PIXIE','PCLAYL',
     &    'Unable to pick RCP bank PX_CALDIS_RCP','W')
        GOTO 999
      ENDIF
C-
      CALL PUGETV('CALEGO EMIN',EMIN)
      CALL PUGET_i('CAL LAYMIN',LAYMIN)
      CALL PUGET_i('CAL LAYMAX',LAYMAX)
      CALL PUGET_l('CAL FIXEMAX',FIXEMAX)
      CALL PUGET_l('CAL SUPHOTC',LSUPHOT)
      CALL PUGET_l('CAL NEGENER',LNEGENR)
C-
      CALL EZRSET
C-
      LAYMIN = 1
      LAYMAX = 17
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
      CALL VZERO(ARRAY3,NPHIL*2*NETAL)
      CALL VZERO(ARRAY4,NPHIL*2*NETAL)
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
        IF ( LNEGENR ) THEN
          IF (ENER .LT. 0.) THEN
            ENER = ABS(ENER)
          ELSE
            GO TO 100
          ENDIF
        ENDIF
        ESUM = ESUM + ENER
        IF (IE .LT. 0) THEN
          IETA = IE + NETAL + 1
        ELSE
          IETA = IE + NETAL
        ENDIF
C-
C- EM Layer(red)
        IF(IL.GE.MNLYEM .AND. IL.LE.MXLYEM) THEN
          ARRAY1(IP,IETA) = ARRAY1(IP,IETA) + ENER
C- ICD/MG(yellow) 
        ELSEIF(IL.GE.MNLYMG .AND. IL.LE.MXLYMG) THEN
          ARRAY2(IP,IETA) = ARRAY2(IP,IETA) + ENER
C- FH Layer(green)
        ELSEIF(IL.GE.MNLYFH .AND. IL.LE.MXLYFH) THEN
          ARRAY3(IP,IETA) = ARRAY3(IP,IETA) + ENER
C- CH Layer(blue)
        ELSEIF(IL.GE.MNLYCH .AND. IL.LE.MXLYCH) THEN
          ARRAY4(IP,IETA) = ARRAY4(IP,IETA) + ENER
        ENDIF
C-
  100 CONTINUE
C-
C--- Scan CAID Bank to find hot cell candidates
C-
      IF(GZCAID().GT.0) THEN
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
      COL1= 'RED'   ! Red for EM
      COL2= 'YEL'   ! Yellow for ICD/MG
      COL3= 'GRE'   ! Green for FH
      COL4= 'CYA'   ! Cyan  for CH
      IF ( LNEGENR ) THEN
        PLTITL='NEG. ENERGY CAEP ETA-PHI'
        IF (IPIQ .EQ. 1) THEN
          PLTITL='NEG. ENERGY CAEQ ETA-PHI'
        ENDIF
      ELSE
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
C-
      IF ( LNEGENR ) THEN
        CALL P7LEGO(NX,XMIN,XMAX,NY,YMIN,YMAX,EMIN,ZMAX,PLTITL,
     X     XLAB,YLAB,ZLAB,COL1,COL2,COL3,COL4,ARRAY1,ARRAY2,
     X     ARRAY3,ARRAY4,NXMIN,NYMIN,NXG,NYG,N,ZSCAL,IMARK,CALPLOT)
      ELSE
        CALL P8LEGO(NX,XMIN,XMAX,NY,YMIN,YMAX,EMIN,ZMAX,PLTITL,
     X     XLAB,YLAB,ZLAB,COL1,COL2,COL3,COL4,ARRAY1,ARRAY2,
     X     ARRAY3,ARRAY4,NXMIN,NYMIN,NXG,NYG,N,ZSCAL,IMARK,CALPLOT)
      ENDIF
C-
C--- Draw messages
      IF (IX .EQ. 0) THEN
        WRITE(MESS1,2000) EMIN
        IF ( LNEGENR ) THEN
          ESUM = -1.*ESUM
        ENDIF
        WRITE(MESS2,2020) ESUM
      ELSE
        WRITE(MESS1,2100) EMIN
        WRITE(MESS2,2120) ESUM
      ENDIF
      CALL PCTEXT(1,MESS1)
      CALL PCTEXT(2,MESS2)
C-
 2000 FORMAT(' CALEGO EMIN =',F4.1,' GeV')
 2020 FORMAT(' CAEP E SUM =',F6.1,' GeV')
 2100 FORMAT(' CALEGO EMIN =',F6.0,' ADC counts')
 2120 FORMAT(' CAEP E SUM =',F8.0,' ADC counts')
C---
C-
  999 RETURN
      END
