      SUBROUTINE PCSVEN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Picture Calorimeter Side View Energies
C-
C-   Inputs  :
C-   Outputs :
C-   Use Bank: CAEP/CAEH
C-
C-   Modified 28-AUG-1992   N. Oshima - Display Et(by CAEH), too.
C-   Modified  8-APR-1992   N. Oshima - For UNIX Version
C-   Modified 12-JAN-1992   N. Oshima - Use BYTE_ORDER.PARAMS
C-   Updated  18-AUG-1991   N. Oshima (Selected PHI Display & color for EAS)
C-   Updated  16-MAY-1991   N. Oshima (Global PHI handling by PX_SYSTEM_RCP)
C-   Updated  11-APR-1991   N. Oshima ( Mod. for layer by layer )
C-   Updated   4-DEC-1990   Sharon Hagopian, Nobuaki Oshima
C-                          Added call to EZPICK
C-   Modified 26-JUN-1990   N. Oshima (Separate fine and coarse had. energy)
C-   Created   4-APR-1990   Nobuaki Oshima
C-   Updated  14-OCT-1992   Lupe Howell  Move EZRSET to the end so it will not
C-                          be missed 
C-   Modified 02-MAR-1994   Nobu Oshima
C-       Remove "IF (ENER .LE. 0.) GO TO 100" before getting ESUM
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
C
      REAL EEM1(NPHIL,-NETAL:NETAL)
      REAL EEM2(NPHIL,-NETAL:NETAL)
      REAL EEM3(NPHIL,-NETAL:NETAL)
      REAL EEM4(NPHIL,-NETAL:NETAL)
      REAL EFHD1(NPHIL,-NETAL:NETAL)
      REAL EFHD2(NPHIL,-NETAL:NETAL)
      REAL EFHD3(NPHIL,-NETAL:NETAL)
      REAL EFHD4(NPHIL,-NETAL:NETAL)
      REAL ECHD(NPHIL,-NETAL:NETAL)  ! Coarse Had. energy.
      REAL EICD(NPHIL,-NETAL:NETAL)  ! ICD energy.
      REAL ECCMG(NPHIL,-NETAL:NETAL) ! CCMG energy.
      REAL EECMG(NPHIL,-NETAL:NETAL) ! ECMG energy.
      REAL EMPHI1(2,-NETAL:NETAL)
      REAL EMPHI2(2,-NETAL:NETAL)
      REAL EMPHI3(2,-NETAL:NETAL)
      REAL EMPHI4(2,-NETAL:NETAL)
      REAL EFHPHI1(2,-NETAL:NETAL)
      REAL EFHPHI2(2,-NETAL:NETAL)
      REAL EFHPHI3(2,-NETAL:NETAL)
      REAL EFHPHI4(2,-NETAL:NETAL)
      REAL ECHPHI(2,-NETAL:NETAL)    ! Coarse Had. energy.
      REAL EICDPHI(2,-NETAL:NETAL)   ! ICD energy.
      REAL ECCMGPHI(2,-NETAL:NETAL)  ! CCMG energy.
      REAL EECMGPHI(2,-NETAL:NETAL)  ! ECMG energy.
      REAL EMAX,SCL,TH1,TH2,EMSCL,EHSCL,CENTH,DELTH
      REAL XS(2,20),YS(2,20),ZS(2,20)
      REAL ENER,EMIN,CENPHI,WIDPHI, COLRNGE,RLEVEL
      REAL ESUM, XP(3)
      REAL PHIBIT(64)
      INTEGER GZCAEP, LCAEP,LDCAEP,INDCES,NRP,NCH
      INTEGER GZCAEH, LCAEH,LDCAEH, GZVERT,LVERT
      INTEGER JPHI,MNPHI,MXPHI,IETA,IT,IPHI,IDPHI,I
      INTEGER IL,IP,IE, IC,NS,IERR,ARGSOK, NUM
      INTEGER KCOL, KINT, KFIL, KSTY, ICOLOR(5)
      INTEGER JBIT,IX, IER
      LOGICAL LPHITYP,EZERROR
      CHARACTER*3  CC,COLORS(5)
      CHARACTER*32 STR1,STR2,STR3
      CHARACTER*15 LABELS(5)
      LOGICAL LG, CEXIST, CONLY, ETHIST, PU_PICK_ACTIVE
C
      BYTE BYTES(4)
      EQUIVALENCE (BYTES(1),INDCES)
C
      DATA NUM /5/
C----------------------------------------------------------------------
C-
C--- Check do picking...
C-
      IF ( PU_PICK_ACTIVE() ) THEN
        CALL PCPETA
        CALL PX_PICK_QUIT
        GO TO 999
      ENDIF
C-
C--- Select correct RCP bank
      CALL EZPICK('PX_CALDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PCSVEN',
     &    'Unable to pick RCP bank PX_CALDIS_RCP','W')
        GOTO 999
      ENDIF
C-
C--- GET MINIMUM ENERGY FOR CELL TO BE PLOTTED
      CALL PUGETV('CAL EMIN',EMIN)
      CALL PUGETV('CAL E COLORANGE',COLRNGE)
      CALL PUGET_l('CAL ETORE',  ETHIST)
      IF(COLRNGE .EQ. 0.) COLRNGE=1.
C-
C--- GET CAEP/CAEH BANK
      IF ( ETHIST ) THEN
        IF(GZCAEH().LE.0) THEN
          CALL PUMESS(' PCSVEN-CAEH BANK DOES NOT EXIST')
          CALL PUOPEN
          CALL PLCASV          ! DRAW OUTLINE OF CELLS
          CALL JRCLOS
          GO TO 900            ! EXIT from routine
        ENDIF
      ELSE
        IF(GZCAEP().LE.0) THEN
          CALL PUMESS(' PCSVEN-CAEP BANK DOES NOT EXIST')
          CALL PUOPEN
          CALL PLCASV          ! DRAW OUTLINE OF CELLS
          CALL JRCLOS
          GO TO 900            ! EXIT from routine
        ENDIF
      ENDIF
C-
C--- CLEAR ALL BUFFERS
      CALL VZERO(PHIBIT,64)
C
      CALL VZERO(EEM1,NPHIL*(2*NETAL+1))
      CALL VZERO(EEM2,NPHIL*(2*NETAL+1))
      CALL VZERO(EEM3,NPHIL*(2*NETAL+1))
      CALL VZERO(EEM4,NPHIL*(2*NETAL+1))
      CALL VZERO(EFHD1,NPHIL*(2*NETAL+1))
      CALL VZERO(EFHD2,NPHIL*(2*NETAL+1))
      CALL VZERO(EFHD3,NPHIL*(2*NETAL+1))
      CALL VZERO(EFHD4,NPHIL*(2*NETAL+1))
      CALL VZERO(ECHD,NPHIL*(2*NETAL+1))
      CALL VZERO(EICD,NPHIL*(2*NETAL+1))
      CALL VZERO(ECCMG,NPHIL*(2*NETAL+1))
      CALL VZERO(EECMG,NPHIL*(2*NETAL+1))
C-
      CALL VZERO(EMPHI1,2*(2*NETAL+1))
      CALL VZERO(EMPHI2,2*(2*NETAL+1))
      CALL VZERO(EMPHI3,2*(2*NETAL+1))
      CALL VZERO(EMPHI4,2*(2*NETAL+1))
      CALL VZERO(EFHPHI1,2*(2*NETAL+1))
      CALL VZERO(EFHPHI2,2*(2*NETAL+1))
      CALL VZERO(EFHPHI3,2*(2*NETAL+1))
      CALL VZERO(EFHPHI4,2*(2*NETAL+1))
      CALL VZERO(ECHPHI,2*(2*NETAL+1))
      CALL VZERO(EICDPHI,2*(2*NETAL+1))
      CALL VZERO(ECCMGPHI,2*(2*NETAL+1))
      CALL VZERO(EECMGPHI,2*(2*NETAL+1))
C-
      ESUM  = 0.
      IF ( ETHIST ) THEN
        LCAEH = GZCAEH()
        NRP   = IQ(LCAEH+2)
        NCH   = IQ(LCAEH+3)
      ELSE
        LCAEP = GZCAEP()
        NRP   = IQ(LCAEP+2)
        NCH   = IQ(LCAEP+3)
      ENDIF
      DO 10 I=1,NCH
        IF ( ETHIST ) THEN
          LDCAEH = LCAEH+3+(I-1)*NRP
          ENER = Q(LDCAEH+5)
          IE     = IQ(LDCAEH+9)
          IP     = IQ(LDCAEH+10)
          IL     = IQ(LDCAEH+11)
        ELSE
          LDCAEP = LCAEP+(I-1)*NRP
          INDCES = IQ(LDCAEP+4)
          ENER   = Q(LDCAEP+5)
          IE     = BYTES(BYTE4)
          IP     = BYTES(BYTE3)
          IL     = BYTES(BYTE2)
        ENDIF
        ESUM = ESUM + ENER
C--- FOR THE FINE DISPLAY
        IF (IL .EQ. 1) THEN                         ! EM1
          EEM1(IP,IE) = EEM1(IP,IE) + ENER
        ELSEIF (IL .EQ. 2) THEN                     ! EM2
          EEM2(IP,IE) = EEM2(IP,IE) + ENER
        ELSEIF (IL.GE.3 .AND. IL.LE.6) THEN         ! EM3
          EEM3(IP,IE) = EEM3(IP,IE) + ENER
        ELSEIF (IL .EQ. 7) THEN                     ! EM4
          EEM4(IP,IE) = EEM4(IP,IE) + ENER
        ELSEIF (IL .EQ. 8) THEN                     ! CCMG
          ECCMG(IP,IE) = ECCMG(IP,IE) + ENER
        ELSEIF (IL .EQ. 9) THEN                     ! ICD
          EICD(IP,IE) = EICD(IP,IE) + ENER
        ELSEIF (IL .EQ. 10) THEN                    ! ECMG
          EECMG(IP,IE) = EECMG(IP,IE) + ENER
        ELSEIF (IL .EQ. 11) THEN                    ! FH1
          EFHD1(IP,IE) = EFHD1(IP,IE) + ENER
        ELSEIF (IL .EQ. 12) THEN                    ! FH2
          EFHD2(IP,IE) = EFHD2(IP,IE) + ENER
        ELSEIF (IL .EQ. 13) THEN                    ! FH3
          EFHD3(IP,IE) = EFHD3(IP,IE) + ENER
        ELSEIF (IL .EQ. 14) THEN                    ! FH4( EC only )
          EFHD4(IP,IE) = EFHD4(IP,IE) + ENER
        ELSEIF (IL.GE.MNLYCH .AND. IL.LE.MXLYCH) THEN
          ECHD(IP,IE) = ECHD(IP,IE) + ENER
        ENDIF
   10 CONTINUE
C-
      IX=JBIT(INDCES,6)
C-
C--- DETERMINE CURRENT PHI LIMITS W.R.T. LOCAL OR GLOBAL PHI
C-
      CALL PUGET_l('CAL ONLY',CONLY)
      CALL PUGET_i('CAL PHI',IPHI)
      CALL PUGET_i('CAL DPHI',IDPHI)
C-
C--- Impose up and down region in the CAL SIDE view
C-
      IF(IPHI .GT. NPHIL/2) IPHI = IPHI - NPHIL/2
C-
C-
C--- CHECK LOCAL OR GLOBAL
      IF ( .NOT. CONLY ) THEN
        CALL EZPICK('PX_SYSTEM_RCP')          ! Selecting SYSTEM bank
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('PIXIE','PCEVEN','Bank PX_SYSTEM_RCP NOT FOUND',
     &      'W')
          GOTO 199
        ENDIF
        CALL PUGET_l('PHI TYPE',LPHITYP)
        IF(LPHITYP) THEN                     ! GLOBAL Mode
          CALL PUGETV('PHI CENTER',CENPHI)
          CALL PUGETV('PHI WIDTH',WIDPHI)
          CALL PUPHI_DEGTOSEG(CENPHI,WIDPHI,IPHI,IDPHI)
        ENDIF
        CALL EZRSET                       ! Reset SYSTEM RCP file
      ENDIF
C-
  199 MNPHI=IPHI-IDPHI
      IF(MNPHI.LT.1) MNPHI=MNPHI+NPHIL
      MXPHI=IPHI+IDPHI
      IF (IDPHI .EQ. 15) MXPHI = MXPHI + 1 !    Nobu. (23-JAN-1991)
      IF(MXPHI.LT.MNPHI) MXPHI=MXPHI+NPHIL
C-
C--- SUM ENERGY OVER CURRENT PHI LIMITS
C--- UPPER PHI SLICE ON PLOT WILL BE THE CURRENT PHI
      DO 50 IETA=-NETAL,NETAL
        IF(IETA.EQ.0) GO TO 50
        DO 40 IPHI=1,2
          DO 30 I=MNPHI,MXPHI
            IF(IPHI.EQ.1) THEN      ! JPHI:  1-31
              JPHI=MOD(I-1,NPHIL)+1
              IF(IETA.EQ.1) PHIBIT(JPHI) = 1.
            ELSE                    ! JPHI: 32-63
              JPHI=MOD(NPHIL/2+I-1,NPHIL)+1
              IF(IETA.EQ.1) PHIBIT(JPHI) = 1.
            ENDIF
            EMPHI1(IPHI,IETA) = EMPHI1(IPHI,IETA)+EEM1(JPHI,IETA)
            EMPHI2(IPHI,IETA) = EMPHI2(IPHI,IETA)+EEM2(JPHI,IETA)
            EMPHI3(IPHI,IETA) = EMPHI3(IPHI,IETA)+EEM3(JPHI,IETA)
            EMPHI4(IPHI,IETA) = EMPHI4(IPHI,IETA)+EEM4(JPHI,IETA)
            EFHPHI1(IPHI,IETA)= EFHPHI1(IPHI,IETA)+EFHD1(JPHI,IETA)
            EFHPHI2(IPHI,IETA)= EFHPHI2(IPHI,IETA)+EFHD2(JPHI,IETA)
            EFHPHI3(IPHI,IETA)= EFHPHI3(IPHI,IETA)+EFHD3(JPHI,IETA)
            EFHPHI4(IPHI,IETA)= EFHPHI4(IPHI,IETA)+EFHD4(JPHI,IETA)
            ECHPHI(IPHI,IETA) = ECHPHI(IPHI,IETA)+ECHD(JPHI,IETA)
            EICDPHI(IPHI,IETA)= EICDPHI(IPHI,IETA)+EICD(JPHI,IETA)
            ECCMGPHI(IPHI,IETA)= ECCMGPHI(IPHI,IETA)+ECCMG(JPHI,IETA)
            EECMGPHI(IPHI,IETA)= EECMGPHI(IPHI,IETA)+EECMG(JPHI,IETA)
   30     CONTINUE
   40   CONTINUE
   50 CONTINUE
C-
C--- FIND MAXIMUM ENERGY IN A CELL
      EMAX=0.
      DO 100 IETA=-NETAL,NETAL
        IF(IETA.EQ.0) GO TO 100
        DO 99 IPHI=1,2
          EMAX=MAX(EMAX, EMPHI1(IPHI,IETA)+EMPHI2(IPHI,IETA)
     &                   +EMPHI3(IPHI,IETA)+EMPHI4(IPHI,IETA)
     &                   +EFHPHI1(IPHI,IETA)+EFHPHI2(IPHI,IETA)
     &                   +EFHPHI3(IPHI,IETA)+EFHPHI4(IPHI,IETA)
     &                   +ECHPHI(IPHI,IETA)+EICDPHI(IPHI,IETA)
     &                   +ECCMG(IPHI,IETA)+EECMG(IPHI,IETA))
   99   CONTINUE
  100 CONTINUE
C
      CALL JIQDIL(RLEVEL)
C-
C--- DRAW SELECTED PHI SEGMENTS
C-
      CALL PCPHIDIS(PHIBIT)
C-
      CALL PUOPEN
C--- DRAW OUTLINE OF CELLS and Reference of IPHI selection
      CALL PLCASV
      IF(EMAX.LE.0.) GO TO 500
C--- DRAW CELLS
      DO 400 IL = 1,17              ! layer
        IF(IL.EQ.4 .OR. IL.EQ.6)    GO TO 400
        DO 300 I = 1,2              ! phi
          DO 200 IE = -37,37        ! eta
            IF (IE .EQ. 0)          GO TO 200
            IP = 17
            IF (I .EQ. 2)   IP = 49
C-
            IF (IL.LT.MNLYEM .OR. IL.GT.MXLYCH)   GO TO 200
            IF (IL .EQ. 1) THEN                         ! EM1
              ENER = EMPHI1(I,IE)
            ELSEIF (IL .EQ. 2) THEN                     ! EM2
              ENER = EMPHI2(I,IE)
            ELSEIF (IL.GE.3 .AND. IL.LE.6) THEN         ! EM3
              ENER = EMPHI3(I,IE)
            ELSEIF (IL .EQ. 7) THEN                     ! EM4
              ENER = EMPHI4(I,IE)
            ELSEIF (IL .EQ. 8) THEN                     ! CCMG
              ENER = ECCMGPHI(I,IE)
            ELSEIF (IL .EQ. 9) THEN                     ! ICD
              ENER = EICDPHI(I,IE)
            ELSEIF (IL .EQ. 10) THEN                    ! ECMG
              ENER = EECMGPHI(I,IE)
            ELSEIF (IL .EQ. 11) THEN                    ! FH1
              ENER = EFHPHI1(I,IE)
            ELSEIF (IL .EQ. 12) THEN                    ! FH2
              ENER = EFHPHI2(I,IE)
            ELSEIF (IL .EQ. 13) THEN                    ! FH3
              ENER = EFHPHI3(I,IE)
            ELSEIF (IL .EQ. 14) THEN                    ! FH4( EC only )
              ENER = EFHPHI4(I,IE)
            ELSEIF(IL.GE.MNLYCH .AND. IL.LE.MXLYCH) THEN
              ENER = ECHPHI(I,IE)
            ENDIF
C-
            IF (ENER .LT. EMIN)       GO TO 200
C-
            LG = CEXIST(IE,IP,IL)
            IF (LG) THEN
C--- Determinig color for the cells
              CALL PCECOL(ENER,EMIN,COLRNGE,IC,CC,NUM)
              IF(RLEVEL .EQ. -2.) THEN
                CALL PXCOLR(CC)
              ELSE
                CALL PXCOLFILL(CC)
              ENDIF
              CALL CELVEC(IE,IP,IL,XS,YS,ZS,NS,IERR)
              IF(IERR .NE.0 .OR. NS.EQ.0)   GO TO 200
              CALL PL2VEC(YS,ZS,NS)
            ENDIF
  200     CONTINUE
  300   CONTINUE
  400 CONTINUE
  500 CALL JRCLOS
C-
C--- Find Primary Vertex
C-
      LVERT = GZVERT(1)
      IF (LVERT .GT. 0) THEN
        CALL UCOPY (Q(LVERT+3), XP(1), 3)
      ELSE
        CALL VZERO(XP,3)
      ENDIF
C-
C *** Drawing Legend (setting viewing parameters to X Y viwe)
C-
      CALL PCELAB(ICOLOR,LABELS)
      DO I=1,NUM
        CALL PXCOLITOC(ICOLOR(I),COLORS(I))
      ENDDO
      CALL LEGEND(COLORS,LABELS,NUM)
C-
      IF(IX.EQ.0) THEN
        IF ( ETHIST ) THEN
          WRITE(STR1,2004) EMAX
          WRITE(STR2,2006) ESUM
        ELSE
          WRITE(STR1,2000) EMAX
          WRITE(STR2,2002) ESUM
        ENDIF
      ELSE
        IF ( EMAX .GT. 99999.) EMAX = 99999.
        WRITE(STR1,2010) EMAX
        WRITE(STR2,2012) ESUM
      ENDIF
      WRITE(STR3,2022) XP(3)
      CALL PCTEXT(1,STR1)
      CALL PCTEXT(2,STR2)
      CALL PCTEXT(3,STR3)
C-
 2000 FORMAT(' Max E= ',F6.1,' GeV')
 2002 FORMAT(' CAEP E SUM=',F6.1,' GeV')
 2004 FORMAT(' Max ET= ',F6.1,' GeV')
 2006 FORMAT(' CAEH ET SUM=',F6.1,' GeV')
 2010 FORMAT(' Max E=',F7.1,' ADC counts')
 2012 FORMAT(' CAEP E SUM=',F8.0,' ADC counts')
 2022 FORMAT(' VTX in Z=',F6.1,' (cm)')
C-
  900 CALL EZRSET   ! Reset CAL RCP file
  999 RETURN
      END
