      SUBROUTINE L2_CDGETZ
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find all Z hits on one sector, and fill bank
C-               DSEC with corresponding information.
C-
C-   Inputs  : in CDLOCA : Layer and sector
C-   Outputs : Bank DSEC is updated with Z information
C-
C-   Created  27-JUL-1987   Olivier Callot
C-   Updated   2-OCT-1987   Rod Engelmann  : Some plot options
C-   Updated  22-MAR-1989   Qizhong Li-Demarteau  use SRCP
C-   Updated  22-FEB-1991   Qizhong Li-Demarteau  added time offset for DL
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated   9-JUN-1992   Qizhong Li-Demarteau  correct Z hit error
C-   Updated  16-DEC-1992   Domenico Pizzuto  added RCP DL errors and
C-                                            nonlinearity corrections
C-   Updated   7-DEC-1993   LIHL, DRC  For LEVEL2 drop nonlinear corrections
C-                          Without alignment banks,clash with PRESET_MEMORY
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$INC:DDEBUG.INC'
      INCLUDE 'D0$INC:CDRESF.INC'
      INCLUDE 'D0$INC:CDCLNK.INC'
      INCLUDE 'D0$INC:CDLOCA.INC'
      INTEGER KPDCDA, KPDSEC, NUMDL, NDSEC, NDCDA
      INTEGER I, J, K, IOCODE, IADCM, IADCP, IADCS, PTDCBD
      INTEGER KPS, NHS, IZS, IPS, KPSEC
C*DC* INTEGER KPDCBD, IOFCBD
      INTEGER KPM, NHM, IZM, IPM, IPM0, IZM0
      INTEGER KPP, NHP, IZP, IPP, IPP0, IZP0
      INTEGER TRFLAG,NTRSEC,IHIT
      INTEGER KPDTMD, KPDTMW, IOFTMD, IOFTMW
      INTEGER MAXZV                     ! Maximum number of candidates
      PARAMETER( MAXZV= 20)
      REAL    ZMINU( MAXZV ), ZPLUS( MAXZV ), ZMAX, ZM, ZP
      PARAMETER( ZMAX= 90.)             ! Maximum Z value for one candidate
      INTEGER JMINU( MAXZV ), JPLUS( MAXZV ), NBMINU, NBPLUS
      REAL TM, TP, TS, ERTM, ERTP, ERTS, TPEXP, ERTEXP, TSUMN
      REAL VELMIN, VELPLU, BESDIF, TZSENS, TZPLUS, TZMINU, DLC2, DLC3
      INTEGER NBZ, ISET, ERR
      REAL    ZCOR, ERRZ
      REAL    CDLTOL, DLTZR1, DLTZR2
      REAL    DLERR1, DLERR2                          !DLERRx needed? C*DC*
      INTEGER IER
      LOGICAL FIRST, EZERROR
      LOGICAL DLERRS                                  !DLERRS needed? C*DC*
C
      COMMON/DELAY_TUNING/CDLTOL, DLTZR1, DLTZR2           !needed to pass C*DC*
C
C ****  Description of the offsets in the banks
C
      INTEGER OFTIME, OFERTI, OFSTDA
      INTEGER OFZCOR, OFERZ, OFPTDA, OFPTZ, OFSTSC
      PARAMETER( OFTIME=2, OFERTI=6, OFSTDA=8)
      PARAMETER( OFZCOR=4, OFERZ=6, OFPTDA=10)
      PARAMETER( OFPTZ=11 , OFSTSC=9)
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C       CDLTOL passed from L2_VERTEX_CDC_PARAMETERS
C       DLTZR1 passed from L2_VERTEX_CDC_PARAMETERS
C       DLTZR2 passed from L2_VERTEX_CDC_PARAMETERS
C*DC*
C*DC*        CDLTOL = 10.0
C*DC*        DLTZR1 = 0.0
C*DC*        DLTZR2 = 0.0
C*DC*        DLERRS=.FALSE.
C*DC*        DLERR1=.65
C*DC*        DLERR2=.31
C*DC*
C
C
C ****  Get pointers on banks : DSEC, DCDA, DTMW, DTMD
C
      KPDSEC = LDSEC( SECTOR, LAYER )
      NDSEC  = IQ( KPDSEC+2)
      KPDCDA = LDCDA( SECTOR, LAYER )
      NDCDA  = IQ( KPDCDA+2)
      KPDTMW = LC( LDTMH - (LAYER+1) )
      IOFTMW = KPDTMW + SECTOR*IC(KPDTMW+4)*IC(KPDTMW+3) +4
      KPDTMD = LC( LDTMH - (LAYER+5) )
      IOFTMD = KPDTMD + SECTOR*IC(KPDTMD+4)*IC(KPDTMD+3) +4
C*DC*      KPDCBD = LC( KPDTMD - 1 )
C*DC*      IOFCBD = KPDCBD + SECTOR*IC(KPDCBD+5)*IC(KPDCBD+4) +5
C
C ****  Work on all the delay lines in the sector
C
      DO 10 NUMDL = 1, NBDELY
        IF ( NUMDL .EQ. 1 ) THEN
          IADCS = 0
          IADCM = NBSENS
        ELSE
          IADCS = MXSENS
          IADCM = NBSENS+2
        ENDIF
        IADCP = IADCM+1
C
C ****  KPxxx are the pointers on the pulses/hits of the good FADC
C ****  Nxxx  are the number of such items
C ****  TZ and VEL are the time_to_position parameters
C
        KPM   = KPDCDA + IQ( KPDCDA + NDCDA+4 + IADCM)
        KPP   = KPDCDA + IQ( KPDCDA + NDCDA+4 + IADCP)
        KPSEC = KPDSEC + IQ( KPDSEC + NDSEC+4 + IADCS)
C- Pointer to DL nonlinearity correction coefficients
C*DC*        PTDCBD= IOFCBD + (NUMDL-1)*IC (KPDCBD+4)
C- DL correction function coefficients
C*DC*        DLC2 = C (PTDCBD+1)
C*DC*        DLC3 = C (PTDCBD+2)
        NHM   = IQ( KPDCDA + 4 + IADCM)
        NHP   = IQ( KPDCDA + 4 + IADCP)
        NHS   = IQ( KPDSEC + 4 + IADCS)
        TZMINU= C( IOFTMD + (2*NUMDL-2)*IC(KPDTMD+3) + 1) + DLTZR1
        TZPLUS= C( IOFTMD + (2*NUMDL-1)*IC(KPDTMD+3) + 1) + DLTZR2
        TZSENS= C( IOFTMW + IADCS * IC(KPDTMW+3) +1)
        VELMIN= C( IOFTMD + (2*NUMDL-2)*IC(KPDTMD+3) + 2)
        VELPLU= C( IOFTMD + (2*NUMDL-1)*IC(KPDTMD+3) + 2)
        IF ( DBGFLG .AND. LVLDBG(5) .NE. 0 ) THEN
          WRITE( LUNDBG,1000 ) LAYER, SECTOR, NUMDL, NHS, NHM, NHP
 1000     FORMAT('0** CDGETZ **       Layer ',I1,' Sector ',I2,
     &           ' Delay line ',I1,
     &           10X,'# of hits on sense wire, -Z, +Z =',3I3)
        ENDIF
C
C ****  Loop on sens wire hits ( delay measure requires a sens wire hit )
C
        DO 20 IZS = 1, NHS
          KPS  = KPDCDA + IQ( KPSEC + OFPTDA )
          TS   = Q( KPS + OFTIME ) - TZSENS
          ERTS = Q( KPS + OFERTI )
          IPM = KPM - IQ( KPDCDA + 3 )
          NBMINU = 0
C
C ****  Loop on all MINUS hits.
C
          DO 30 IZM = 1, NHM
            IPM = IPM + IQ( KPDCDA + 3 )
            TM  = Q( IPM + OFTIME ) - TZMINU - TS
            ERTM= Q( IPM + OFERTI )
            ZM  = TM * VELMIN
            IF( ABS(ZM) .GT. ZMAX ) GOTO 30
            IF( NBMINU .EQ. MAXZV ) GOTO 30
C*DC*            ZM = ZM - (DLC2+DLC3*ZM)*ZM**2
            NBMINU = NBMINU + 1
            ZMINU( NBMINU ) = ZM
            JMINU( NBMINU ) = IPM - KPDCDA
   30     CONTINUE
          NBPLUS = 0
          IPP = KPP - IQ(KPDCDA+3)
C
C ****  Idem on PLUS side
C
          DO 40 IZP = 1, NHP
            IPP = IPP + IQ(KPDCDA+3)
            TP  = Q( IPP + OFTIME ) - TZPLUS - TS
            ERTP= Q( IPP + OFERTI )
            ZP = TP * VELPLU
            IF( ABS( ZP ) .GT. ZMAX ) GOTO 40
            IF( NBPLUS .EQ. MAXZV ) GOTO 40
C*DC*            ZP = ZP - (DLC2+DLC3*ZP)*ZP**2
            NBPLUS = NBPLUS + 1
            ZPLUS( NBPLUS ) = ZP
            JPLUS( NBPLUS ) = IPP - KPDCDA
   40     CONTINUE
C
C ****  List of candidates are built. Debug if needed
C
          IF ( DBGFLG .AND. LVLDBG(5) .GE. 1 ) THEN
            WRITE( LUNDBG, 1090 ) IZS, TS
 1090       FORMAT(' hit number',I4,' time =',F10.1)
            WRITE( LUNDBG, 1100 ) NBMINU, ( ZMINU(IZM), IZM=1,NBMINU)
 1100       FORMAT(5X,'Minus side',I3,' values :',20F6.1)
            WRITE( LUNDBG, 1110 ) NBPLUS, ( ZPLUS(IZP), IZP=1,NBPLUS)
 1110       FORMAT(5X,'Plus  side',I3,' values :',20F6.1)
          ENDIF
C
C ****  Select the best candidate ( if any )
C
          BESDIF = 10000.
          IF( IZS .LE. NHTDLY ) DLRES(IZS,NUMDL,SECTOR,LAYER) = BESDIF
          IF ( NBMINU*NBPLUS .NE. 0 ) THEN
C
C ****  At least one MINUS and one PLUS. Search best, and cut ( criterion
C ****  is the compatibility of the Z from both sides
C
            DO 100 IZM = 1, NBMINU
              DO 110 IZP = 1, NBPLUS
                IF ( ABS( ZPLUS(IZP)-ZMINU(IZM)) .LT. BESDIF ) THEN
                  BESDIF = ABS( ZPLUS(IZP)-ZMINU(IZM) )
                  IZM0 = IZM
                  IZP0 = IZP
                ENDIF
  110         CONTINUE
  100       CONTINUE
            IF( IZS .LE. NHTDLY ) DLRES(IZS,NUMDL,SECTOR,LAYER) =
     &                         ZMINU(IZM0)/VELMIN + ZPLUS(IZP0)/VELPLU
            IF( BESDIF .GE. CDLTOL ) THEN
              GOTO 20   ! disagree... What can we do ????
            ENDIF
          ELSE
C
C ****  If only one side, requires only one solution on this side, because
C ****  no criterion to chose between ambiguous hits.
C
            IF( NBPLUS + NBMINU .NE. 1 ) GOTO 20   ! only one solution
            IZM0 = NBMINU
            IZP0 = NBPLUS
          ENDIF
C
C ****  computes the value and error on Z
C
          NBZ = 0
          ZCOR = 0.
          ERRZ = 0.
          IF ( NBMINU .EQ. 0 ) THEN
            IPM = 0
            ERRZ = ERRZ + ( ERTS * VELPLU ) **2
          ELSE
            IPM  = JMINU( IZM0 )
            IPM0 = KPDCDA + IPM
            NBZ  = NBZ + 1
            ZCOR = ZCOR + ZMINU( IZM0 )
            ERRZ = ERRZ + ( Q( IPM0 + OFERTI ) * VELMIN )**2
          ENDIF
          IF ( NBPLUS .EQ. 0 ) THEN
            IPP = 0
            ERRZ = ERRZ + ( ERTS * VELMIN ) **2
          ELSE
            IPP  = JPLUS( IZP0 )
            IPP0 = KPDCDA + IPP
            NBZ  = NBZ + 1
            ZCOR = ZCOR + ZPLUS( IZP0 )
            ERRZ = ERRZ + ( Q( IPP0 + OFERTI ) * VELPLU )**2
          ENDIF
          IF (NBZ .EQ. 0) GOTO 20
          ERRZ = ERRZ / NBZ
C
C ****  This is a good Z hit. Update info in DSEC for this hit
C
          IQ( KPSEC + OFPTZ  ) = IPM
          IQ( KPSEC + OFPTZ+1) = IPP
          Q( KPSEC + OFZCOR )  = ZCOR / NBZ

C*DC*
C- If requested add RCP delay line errors for 1 side and 2 side hits for
C- tracks at theta = 90 degrees.
C*DC*           IF (DLERRS) THEN
C*DC*            IF (NBPLUS*NBMINU.EQ.0) THEN
C*DC*             Q( KPSEC + OFERZ ) = DLERR1
C*DC*            ELSE
C*DC*             Q( KPSEC + OFERZ ) = DLERR2
C*DC*            END IF
C*DC*           ELSE
            Q( KPSEC + OFERZ )  = SQRT( ERRZ )
C*DC*           END IF
C*DC*

          IF ( DBGFLG .AND. LVLDBG(5) .GT. 0 ) THEN
            WRITE( LUNDBG, 1200 ) Q( KPSEC+OFZCOR ), Q( KPSEC+OFERZ )
 1200       FORMAT(5X,'Z coordinate is ',F10.3,' +/- ',F10.3)
          ENDIF
C
C ****  Adjust STATUS word of DSEC, combination of flags
C
          ISET = 0
          IF ( IPM .NE. 0 ) THEN
            CALL MVBITS(IQ(IPM0+OFSTDA), 0, 8, IQ(KPSEC+OFSTSC), 16 )
            ISET = 1
          ENDIF
          IF ( IPP .NE. 0 ) THEN
            CALL MVBITS(IQ(IPP0+OFSTDA), 0, 8, IQ(KPSEC+OFSTSC), 24 )
            ISET = ISET + 2
          ENDIF
          CALL MVBITS( ISET, 0, 8, IQ(KPSEC+OFSTSC), 0  )
   20   KPSEC = KPSEC + IQ(KPDSEC+3)
   10 CONTINUE
  999 RETURN
      END
