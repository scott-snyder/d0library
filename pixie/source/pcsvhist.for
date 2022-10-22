      SUBROUTINE PCSVHIST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : HISTOGRAM Calorimeter Energies (Side View)
C-                         Bins have divisions in ETA between RMIN
C-                         and RMAX for CC and each EC
C-   Inputs  :
C-   Outputs :
C-
C-   Created   8-JUN-1988   Michael Peters
C-   Updated  26-AUG-1988   Michael W. Peters  Change to 2D E array
C-   Updated  01-MAR-1990   Nobu. Oshima (Clean up local INC's files)
C-   Updated  23-JUL-1990   Lupe Howell  Implementing RCP parameters in PIXIE
C-   Updated  10-SEP-1990   Harrison B. Prosper
C-      Use PX_CALDIS_RCP
C-   Updated  24-APR-1991   Nobuaki Oshima
C-      Remove EVT# check and fix the bug to clear buffers.
C-   Updated  16-MAY-1991   N. Oshima ( Global PHI handling by PX_SYSTEM_RCP )
C-   Updated  18-AUG-1991   N. Oshima ( Selected PHI Display )
C-   Updated  11-OCT-1991   N. Oshima ( Put an ETA label )
C-   Modified 12-JAN-1992   Nobu Oshima - Use BYTE_ORDER.PARAMS
C-   Updated  19-FEB-1992   Lupe Howell  Removed selft nested FOR loos for SGI 
C-   Updated  20-JUL-1992   N. Oshima ( Add 'CALL PCPETA' for picking ETA )
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
C
      REAL EEM(NPHIL,-NETAL:NETAL) ! elemag energy.
      REAL EHD(NPHIL,-NETAL:NETAL) ! had. energy(fine+coarse)
      REAL EMPHI(2,-NETAL:NETAL),EHPHI(2,-NETAL:NETAL)
      REAL EMAX,SCL,TH1,TH2,EMSCL,EHSCL,CENTH,DELTH
      REAL ENER,ESUM,MET, FETA
      REAL DIST,X1,Y1,X2,Y2
      REAL CENRAD,DELRAD,CENZED,DELZED
      REAL X, Y, Z, CENPHI,WIDPHI
      REAL PCR1,PCR2,PCZ1,PCZ2, TTH
      REAL PHIBIT(64)
      INTEGER GZCAEP, LCAEP,LDCAEP,INDCES,NRP,NCH
      INTEGER JPHI,MNPHI,MXPHI,JETA,IT,I,ARGSOK, NUM
      INTEGER IL,IP,IE,IETA,IPHI,IDPHI
      INTEGER NETACC,JBIT,IX,IER
      INTEGER RUN,ID,RUNSAVE,IDSAVE
      LOGICAL LPHITYP,EZERROR,CONLY,PU_PICK_ACTIVE
      SAVE RUNSAVE,IDSAVE
      CHARACTER*30 STR1,STR2
      CHARACTER*3 COLORS(6)
      CHARACTER*15 LABELS(6)
      CHARACTER*20 REM
      CHARACTER*4 CETA
C
      BYTE BYTES(4)
      EQUIVALENCE (BYTES(1),INDCES)
C-
      DATA COLORS /6*'   '/
      DATA LABELS /6*'   '/
      DATA NETACC /12/
C----------------------------------------------------------------------
C-
C-
C--- Check do picking...
C-
      IF ( PU_PICK_ACTIVE() ) THEN
        CALL PCPETA
        CALL PX_PICK_QUIT
        GO TO 999
      ENDIF
C-
C--- GET CAEP BANK
      IF(GZCAEP().LE.0) THEN
        CALL PUMESS(' PCSVHIST-CAEP BANK DOES NOT EXIST')
        GO TO 999
      ENDIF
C--- CLEAR ALL BUFFERS
      CALL VZERO(PHIBIT,64)
      CALL VZERO(EEM,NPHIL*(2*NETAL+1))
      CALL VZERO(EHD,NPHIL*(2*NETAL+1))
      CALL VZERO(EMPHI,2*(2*NETAL+1))
      CALL VZERO(EHPHI,2*(2*NETAL+1))
C-
      LCAEP = GZCAEP()
      NRP   = IQ(LCAEP+2)
      NCH   = IQ(LCAEP+3)
      DO 10 I=1,NCH
        LDCAEP = LCAEP+(I-1)*NRP
        INDCES = IQ(LDCAEP+4)
        ENER   = Q(LDCAEP+5)
        IE=BYTES(BYTE4)
        IP=BYTES(BYTE3)
        IL=BYTES(BYTE2)
        IF(ENER.LE.0) GO TO 10
        IF(IL.GE.MNLYEM .AND. IL.LE.MXLYEM) THEN
          EEM(IP,IE) = EEM(IP,IE) + ENER
        ELSEIF(IL.GE.MNLYFH .AND. IL.LE.MXLYCH) THEN
          EHD(IP,IE) = EHD(IP,IE) + ENER
        ENDIF
   10 CONTINUE
C-
      IX=JBIT(INDCES,6)
C-
C--- SETUP PC_PARAMETERS
      CALL EVNTID(RUN,ID)
      IF(RUN.NE.RUNSAVE.OR.ID.NE.IDSAVE) THEN
        RUNSAVE = RUN
        IDSAVE  = ID
        TTH = TAN(2.*ATAN(EXP(-NETACC/10.)))
        CALL CELXYZ(26, 1, MXLYEM, X, Y, Z, ARGSOK)
        PCR1 = Z*TTH
        PCZ1 = Z
        CALL CALRAD( 1,MNLYCH,CENRAD,DELRAD,CENZED,DELZED,ARGSOK)
        PCR2 = CENRAD + DELRAD/2.
        PCZ2 = PCR2/TTH
      ENDIF
C
C ****  DETERMINE CURRENT PHI
C
      CALL EZPICK('PX_CALDIS_RCP')          ! Selecting Caldis bank
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PCSVHIST',
     &    'Bank PX_CALDIS_RCP NOT FOUND','W')
        GOTO 999
      ENDIF
      CALL PUGET_l('CAL ONLY',CONLY)
      CALL PUGET_i('CAL PHI',IPHI)
      CALL PUGET_i('CAL DPHI',IDPHI)
      CALL PUGET_i('CAL ETA',IETA)
C-
C--- Impose up and down region in the CAL SIDE view
C-
      IF(IPHI .GT. NPHIL/2) IPHI = IPHI - NPHIL/2
C-
      CALL EZRSET
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
        CALL EZRSET
      ENDIF
C-
  199 MNPHI=IPHI-IDPHI
      IF(MNPHI.LT.1) MNPHI=MNPHI+NPHIL
      MXPHI=IPHI+IDPHI
      IF (IDPHI .EQ. 15) MXPHI = MXPHI + 1 !    Nobu. (23-JAN-1991)
      IF(MXPHI.LT.MNPHI) MXPHI=MXPHI+NPHIL
C SUM ENERGY OVER CURRENT PHI LIMITS
C UPPER PHI SLICE ON PLOT WILL BE THE CURRENT PHI
      DO 50 JETA=-NETAL,NETAL
        IF(JETA.EQ.0) GO TO 50
        DO 40 IPHI=1,2
          DO 30 I=MNPHI,MXPHI
            IF(IPHI.EQ.1) THEN
              JPHI=MOD(I-1,NPHIL)+1
              IF(JETA.EQ.1) PHIBIT(JPHI) = 1.
            ELSE
              JPHI=MOD(NPHIL/2+I-1,NPHIL)+1
              IF(JETA.EQ.1) PHIBIT(JPHI) = 1.
            ENDIF
            EMPHI(IPHI,JETA)=EMPHI(IPHI,JETA)+EEM(JPHI,JETA)
            EHPHI(IPHI,JETA)=EHPHI(IPHI,JETA)+EHD(JPHI,JETA)
   30     CONTINUE
   40   CONTINUE
   50 CONTINUE
C-
C--- FIND MAXIMUM ENERGY IN A CELL
      EMAX=0.
      DO 100 JETA=-NETAL,NETAL
        IF(JETA.EQ.0) GO TO 100
        DO 99 IPHI=1,2
          ESUM=EMPHI(IPHI,JETA)+EHPHI(IPHI,JETA)
          IF(ESUM .GT. EMAX) THEN
            EMAX=ESUM
          ENDIF
   99   CONTINUE
  100 CONTINUE
      IF(EMAX.LE.0.) GO TO 1100
C CALCULATE SCALE SO MAX ENERGY FILLS A CELL
      SCL=MIN((PCR2-PCR1)/EMAX,(PCZ2-PCZ1)/EMAX)
      IF(IETA.NE.0) THEN
C CALCULATE LINE TO SHOW CURRENT ETA
        CALL CALTH(IETA,CENTH,DELTH,ARGSOK)
        IF(ARGSOK.NE.0) GO TO 500
        IF(ABS(IETA).LE.NETACC) THEN
C CURRENT ETA IS IN CC
          DIST=PCR2+5.
          X1=DIST/TAN(CENTH+DELTH/2.)
          Y1=DIST
          X2=DIST/TAN(CENTH-DELTH/2.)
          Y2=DIST
        ELSE
C CURRENT ETA IS IN EC
          DIST=PCZ2+5.
          X1=DIST
          Y1=DIST*TAN(CENTH+DELTH/2.)
          X2=DIST
          Y2=DIST*TAN(CENTH-DELTH/2.)
        ENDIF
      ENDIF
  500 CONTINUE
C-
C--- DRAW SELECTED PHI SEGMENTS
C-
      CALL PCPHIDIS(PHIBIT)
C-
      CALL PUOPEN
C DRAW CURRENT ETA VALUE
      IF(IETA.NE.0) THEN
        CALL JMOVE(X1,Y1)
        CALL JDRAW(X2,Y2)
      ENDIF
C DRAW CELLS WITH HISTOGRAM OF ENERGY IN CELL
      CALL PXCOLR('GRE')
      CALL JJUST(2,1)
      CALL JSIZE( 10., 15.)
      CALL JFONT(5)
      CALL J3MOVE( 0., PCR2, 0. )
      CALL J3DRAW( 0., PCR2+8., 0. )
      CALL JHSTRG('ETA=0.')
      CALL PXCOLR('FOR')
      NUM = 1
      DO 1000 JETA=-NETAL,NETAL
        IF(JETA.EQ.0) GO TO 1000
        CALL CALTH(JETA,CENTH,DELTH,ARGSOK)
        IF(ARGSOK.NE.0) GO TO 1000
        TH1=CENTH+DELTH/2.
        TH2=CENTH-DELTH/2.
        IF(ABS(JETA).LE.NETACC) THEN
C-
C--- DRAW CC CELLS IN UPPER AND LOWER PHI SLICES
          IF( MOD(JETA,5) .EQ. 0) THEN
            FETA = FLOAT(JETA)/10.
            WRITE(CETA,2010) FETA
            CALL PXCOLR('GRE')
            IF(JETA .LT. 0) THEN
              CALL JJUST(2,1)
            ELSE
              CALL JJUST(1,1)
            ENDIF
            CALL JSIZE( 10., 15.)
            CALL JFONT(5)
            CALL J3MOVE( (PCR2+5.)/TAN(TH1), PCR2+5., 0. )
            CALL JHSTRG(CETA)
            CALL PXCOLR('FOR')
          ENDIF
C-
          EMSCL=SCL*EMPHI(1,JETA)
          EHSCL=SCL*EHPHI(1,JETA)
          CALL PCSVCC(PCR1,PCR2,TH1,TH2,2,EMSCL,EHSCL,0.,COLORS,
     &                LABELS,NUM)
          EMSCL=SCL*EMPHI(2,JETA)
          EHSCL=SCL*EHPHI(2,JETA)
          CALL PCSVCC(PCR1,PCR2,-TH1,-TH2,2,EMSCL,EHSCL,0.,COLORS,
     &         LABELS,NUM)
        ELSE
C-
C--- DRAW EC CELLS IN UPPER AND LOWER PHI SLICES
          IF( MOD(JETA,5) .EQ. 0) THEN
            FETA = FLOAT(JETA)/10.
            WRITE(CETA,2010) FETA
            CALL PXCOLR('GRE')
            IF(JETA .LT. 0) THEN
              CALL J3MOVE( -(PCZ2+5.), -(PCZ2+5.)*TAN(TH1), 0. )
              CALL JJUST(3,2)
            ELSE
              CALL J3MOVE( PCZ2+8., (PCZ2+5.)*TAN(TH2), 0. )
              CALL JJUST(2,2)
            ENDIF
            CALL JSIZE( 10., 15.)
            CALL JFONT(5)
            CALL JHSTRG(CETA)
            CALL PXCOLR('FOR')
          ENDIF
C-
          EMSCL=SCL*EMPHI(1,JETA)
          EHSCL=SCL*EHPHI(1,JETA)
          CALL PCSVEC(PCZ1,PCZ2,TH1,TH2,2,EMSCL,EHSCL,0.,COLORS,
     &                LABELS, NUM)
          EMSCL=SCL*EMPHI(2,JETA)
          EHSCL=SCL*EHPHI(2,JETA)
          CALL PCSVEC(PCZ1,PCZ2,-TH1,-TH2,2,EMSCL,EHSCL,0.,COLORS,
     &                 LABELS,NUM)
        ENDIF
 1000 CONTINUE
      CALL PCSVME(MNPHI,MXPHI,PCR1,PCR2,PCZ1,PCZ2,SCL,MET)
      CALL JRCLOS
C--- PUT A BOX INTO LEGEND FOR MISS. ET
      COLORS(NUM) = 'MAG'
      LABELS(NUM) = ' MISS ET      '
      NUM = NUM + 1
      CALL LEGEND(COLORS,LABELS,(NUM-1) )
 1100 IF(IX.EQ.0) THEN
        WRITE(STR1,2000) EMAX
        WRITE(STR2,2005) MET
      ELSE
        WRITE(STR1,2001) EMAX
        WRITE(STR2,2006) MET
      ENDIF
 2000 FORMAT(' Max E = ',F6.1,' GeV')
 2001 FORMAT(' Max E =',F7.1,' ADC counts')
 2005 FORMAT(' Miss ET = ',F6.1,' GeV')
 2006 FORMAT(' Miss ET =',F7.1,' ADC Counts')
 2010 FORMAT(F4.1)
      CALL PCTEXT(1,STR1)
      CALL PCTEXT(2,STR2)
C-
  999 RETURN
      END
