      SUBROUTINE PCEVEN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Picture Calorimeter End View Energy
C-
C-   Inputs  :
C-   Outputs :
C-
C-   Created   8-JUN-1988   Michael Peters
C-   Updated  26-AUG-1988   Michael W. Peters  Change to 2D E array
C-   Updated  01-MAR-1990   N. Oshima (Clean up local INC's files)
C-   Updated  28-MAR-1990   N. Oshima (Switch to CAEH and handle IETA)
C-   Updated  10-SEP-1990   Harrison B. Prosper
C-      Use PX_CALDIS_RCP in EZPICK
C-   Updated  30-JAN-1991   Lupe Howell  Get all the elements from
C-      PX_CALDIS_RCP at the beginning of the rotine to insure the right RCP
C-      file is active.
C-   Updated  24-APR-1991   N. Oshima (Change CAEH bank handling.)
C-   Updated  16-MAY-1991   N. Oshima (Global PHI handling by PX_SYSTEM_RCP)
C-   Updated  03-JUL-1991   N. Oshima ( System Picking )
C-   Modified 18-AUG-1991   N. Oshima ( Gives colors for E&S )
C-   Modified 10-OCT-1991   N. Oshima ( Gives X-Y axes labels )
C-   Modified 20-JAN-1992   N. Oshima
C-      Change the ADC counts format and add CONLY parameter for
C-      Local PHI handling.
C-   Modified 20-MAR-1992   N. Oshima ( Add ICD and MG Energy )
C-   Modified 07-MAY-1992   Nobuaki Oshima
C-      Add Logical Params FIXEMAX and EMAX when FIXEMAX is .TRUE. for the
C-      combined END View.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
C----------------------------------------------------------------------
      REAL EEM(NPHIL,-NETAL:NETAL) ! elemag energy.
      REAL EHD(NPHIL,-NETAL:NETAL) ! had. energy(fine+coarse)
      REAL EICDMG(NPHIL,-NETAL:NETAL) ! energy in ICD and MG
      REAL EHPH(NPHIL),EMPH(NPHIL),ESUM,EMAX,X(4),Y(4),SCL
      REAL EIMPHI(NPHIL),EISCL
      REAL CENPHI,WIDPHI,DELPHI,PHI1,PHI2,EMSCL,EHSCL
      REAL PCMIN,PCMAX,ENER,MET,EMAXFX
      REAL RLEVEL
      REAL CENRAD,DELRAD,CENZED,DELZED
      REAL MXEMBN,MXHDBN,MXIMBN,MXTOTBN
      REAL MXEMHS,MXHDHS,MXIMHS,MXTOTHS
      INTEGER IPHI,IDPHI,IETAMN,IETAMX,IETACN,IDETA
      INTEGER IETA,ARGSOK, NUM,ILVL,IOPT
      INTEGER IL,IP,IE, I,JPHI
      INTEGER IX,JBIT, ICOLOR(6)
      INTEGER GZCAEH, LCAEH,LDCAEH,GZCAEP,LCAEP
      INTEGER NRP,NCH,NCHP
      INTEGER RUN,ID,RUNSAVE,IDSAVE, TYP,IER
      SAVE RUNSAVE,IDSAVE
      CHARACTER*30 STR1,STR2,STR3
      CHARACTER*3 COLORS(6)
      CHARACTER*15 LABELS(6)
      LOGICAL PU_PICK_ACTIVE,EZERROR
      LOGICAL CONLY,LPHITYP,ETHIST,FIXEMAX
C----------------------------------------------------------------------
      DATA COLORS /6*'   '/
      DATA LABELS /6*'   '/
C----------------------------------------------------------------------
C-
C--- Check do picking...
C-
      IF ( PU_PICK_ACTIVE() ) THEN
        CALL PCPPHI
        CALL PX_PICK_QUIT
        GO TO 999
      ENDIF
C-
C-
      CALL EZPICK('PX_CALDIS_RCP')          ! Selecting Caldis bank
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PCEVEN','Bank PX_CALDIS_RCP NOT FOUND','W')
        GOTO 999
      ENDIF
C-
      CALL PUGETV('CAL ONLY',CONLY)
      CALL PUGETV('CAL FIXEMAX',FIXEMAX)
      CALL PUGETV('CAL EMAX',EMAXFX)
      CALL PUGETV('CAL IETAMIN',IETAMN)
      CALL PUGETV('CAL IETAMAX',IETAMX)
      CALL PUGETV('CAL IETACEN',IETACN)
      CALL PUGETV('CAL IDETA',  IDETA)
      CALL PUGETV('CAL ETORE',  ETHIST)
      CALL PUGETV('CAL PHI',    IPHI)
      CALL PUGETV('CAL DPHI',   IDPHI)
      CALL PUGETV('PNUT LEVEL', ILVL)
C-
      IF (IETACN .NE. 0) THEN
        IETAMN = IETACN - IDETA
        IETAMX = IETACN + IDETA
        IF (IETAMN .EQ. 0) IETAMN = 1
        IF (IETAMX .EQ. 0) IETAMX = -1
      ENDIF
      IF (IETAMN .GT. IETAMX)   IETAMN = IETAMX
C-
C--- Impose up and down region in the CAL SIDE view
C-
      IF(IPHI .GT. NPHIL/2) IPHI = IPHI - NPHIL/2
C-
      CALL EZRSET
      IF ( .NOT. CONLY ) THEN
C-
C--- Get PHI from PX_SYSTEM_RCP if PHI Type was global.
        CALL EZPICK('PX_SYSTEM_RCP')          ! Selecting SYSTEM bank
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('PIXIE','PCEVEN','Bank PX_SYSTEM_RCP NOT FOUND',
     &      'W')
          GOTO 199
        ENDIF
        CALL PUGETV('PHI TYPE',LPHITYP)
        IF(LPHITYP) THEN                  ! GLOBAL Mode
          CALL PUGETV('PHI CENTER',CENPHI)
          CALL PUGETV('PHI WIDTH',WIDPHI)
          CALL PUPHI_DEGTOSEG(CENPHI,WIDPHI,IPHI,IDPHI)
        ENDIF
C-
        CALL EZRSET
  199   CONTINUE
      ENDIF
C-
C--- GET CAEH BANK
      IF(GZCAEH().LE.0) THEN
        CALL PUMESS(' PCEVEN-CAEH BANK DOES NOT EXIST')
        CALL VZERO(EEM,NPHIL*(2*NETAL+1))
        CALL VZERO(EHD,NPHIL*(2*NETAL+1))
        CALL VZERO(EICDMG,NPHIL*(2*NETAL+1))
        GO TO 20
      ENDIF
      LCAEH = GZCAEH()
      NRP   = IQ(LCAEH+2)
      NCH   = IQ(LCAEH+3)
      CALL VZERO(EEM,NPHIL*(2*NETAL+1))
      CALL VZERO(EHD,NPHIL*(2*NETAL+1))
      CALL VZERO(EICDMG,NPHIL*(2*NETAL+1))
      DO 10 I=1,NCH
        LDCAEH = LCAEH+3+(I-1)*NRP
        IF (ETHIST) THEN
          ENER = Q(LDCAEH+5)
        ELSE
          ENER = Q(LDCAEH+4)
        ENDIF
        IE     = IQ(LDCAEH+9)
        IP     = IQ(LDCAEH+10)
        IL     = IQ(LDCAEH+11)
        IF(ENER.LE.0) GO TO 10
        IF(IE.GE.-32 .AND. IE.LE.32) THEN
          IF(IL.GE.MNLYEM .AND. IL.LE.MXLYEM) THEN
            EEM(IP,IE) = EEM(IP,IE) + ENER
          ELSEIF(IL.GE.MNLYFH .AND. IL.LE.MXLYCH) THEN
            EHD(IP,IE) = EHD(IP,IE) + ENER
          ELSEIF(IL.GE.MNLYMG .AND. IL.LE.MXLYMG) THEN
            EICDMG(IP,IE) = EICDMG(IP,IE) + ENER
          ENDIF
        ELSE
          ENER = ENER/2.
          IF(IL.GE.MNLYEM .AND. IL.LE.MXLYEM) THEN
            EEM(IP,IE)   = EEM(IP,IE) + ENER
            EEM(IP+1,IE) = EEM(IP+1,IE) + ENER
          ELSEIF(IL.GE.MNLYFH .AND. IL.LE.MXLYCH) THEN
            EHD(IP,IE)   = EHD(IP,IE) + ENER
            EHD(IP+1,IE) = EHD(IP+1,IE) + ENER
          ENDIF
        ENDIF
   10 CONTINUE
C-
C--- Get initial Valus to draw the end view and DATA Type information
   20 CALL EVNTID(RUN,ID)
      IF(RUN.NE.RUNSAVE.OR.ID.NE.IDSAVE) THEN
        RUNSAVE = RUN
        IDSAVE  = ID
        CALL CALRAD(1,MNLYEM,CENRAD,DELRAD,CENZED,DELZED,ARGSOK)
        PCMIN = CENRAD - DELRAD/2.
        CALL CALRAD(1,MNLYCH,CENRAD,DELRAD,CENZED,DELZED,ARGSOK)
        PCMAX = CENRAD + DELRAD/2.
C-
        LCAEP = GZCAEP()
        NCHP  = IQ(LCAEP+3)
        IF(LCAEP.GT.0 .AND. NCHP.GT.0) THEN
          IX=JBIT(IQ(LCAEP+4),6)
        ENDIF
      ENDIF
C-
      EMAX=0.
      DO 50 JPHI=1,NPHIL
        EMPH(JPHI)=0.
        EHPH(JPHI)=0.
        EIMPHI(JPHI)=0.
        DO 30 IETA=IETAMN,IETAMX
          IF(IETA.EQ.0) GO TO 30
          EMPH(JPHI)=EMPH(JPHI)+EEM(JPHI,IETA)
          EHPH(JPHI)=EHPH(JPHI)+EHD(JPHI,IETA)
          EIMPHI(JPHI)=EIMPHI(JPHI)+EICDMG(JPHI,IETA)
   30   CONTINUE
        ESUM = EMPH(JPHI)+EHPH(JPHI) + EIMPHI(JPHI)
        IF(ESUM .GT. EMAX) THEN
          EMAX = ESUM
        ENDIF
   50 CONTINUE
C-
      IF ( FIXEMAX ) THEN
        IF ( ETHIST ) THEN
          IOPT = 2
        ELSE
          IOPT = 1
        ENDIF
        CALL PC_GET_MAXEORET(IOPT,MXEMBN,MXHDBN,MXIMBN,MXTOTBN,
     &                            MXEMHS,MXHDHS,MXIMHS,MXTOTHS)
        EMAX = MXTOTHS
      ENDIF
C-
C---  CALCULATE THE PHI LIMITS CURRENTLY IN EFFECT
C-
      CALL CALPHI(IPHI-IDPHI,1,CENPHI,DELPHI,ARGSOK)
      PHI1=(CENPHI-DELPHI/2)/RADIAN     ! JARC requires DEGREES
      CALL CALPHI(IPHI+IDPHI,1,CENPHI,DELPHI,ARGSOK)
      PHI2=(CENPHI+DELPHI/2)/RADIAN     ! JARC requires DEGREES
      IF (IDPHI .EQ. 15)   PHI2 = PHI2 + 4.
C- CALCULATE THE ENERGY SCALE SO EMAX FILLS THE CELL
      IF (EMAX .GT. 0.) THEN
        SCL=(PCMAX-PCMIN)/EMAX
      ELSE
        SCL=100.
      ENDIF
C-
      CALL PUOPEN
C- DRAW THE CURRENT PHI LIMITS IN UP & DOWN(Nobu. 29-APR-1991)
      CALL JARC(0.,0.,0.,PCMAX+5.,0,PHI1,PHI2)
      CALL JARC(0.,0.,0.,PCMAX+5.,0,PHI1+180.,PHI2+180.)
C-
C--- DRAW EACH CELL WITH HISTOGRAM OF ITS ENERGY
      NUM  = 1
      DO 100 JPHI=1,NPHIL
        CALL CALPHI(JPHI,1,CENPHI,DELPHI,ARGSOK)
        PHI1 = CENPHI-DELPHI/2.
        PHI2 = CENPHI+DELPHI/2.
        EMSCL= SCL*EMPH(JPHI)
        EISCL= SCL*EIMPHI(JPHI)
        EHSCL= SCL*EHPH(JPHI)
        CALL PCEVCL(PCMIN,PCMAX,PHI1,PHI2,3,EMSCL,EISCL,EHSCL,
     &             COLORS,LABELS,NUM)
  100 CONTINUE
      IF (EMAX .GT. 0.) CALL PCEVME(PCMIN,PCMAX,SCL,MET,ILVL)
C-
C--- LABEL FOR X- AND Y-AXES
      CALL PXCOLR('GRE')
      CALL JJUST(1,2)
      CALL JSIZE(PCMAX/18.,PCMAX/15.)
      CALL JFONT(5)
      CALL J3MOVE( PCMAX+5., 0., 0. )
      CALL J3DRAW( PCMAX+40., 0., 0. )
      CALL J3MOVE( PCMAX+40.-PCMAX/20., 0., 0. )
      CALL JHSTRG('> X[PHI=0]')
      CALL J3MOVE( 0., PCMAX+5., 0. )
      CALL J3DRAW( 0., PCMAX+40., 0. )
      CALL JBASE( 0., 1., 0.)
      CALL JPLANE(-1., 0., 0.)
      CALL J3MOVE( 0., PCMAX+40.-PCMAX/28., 0. )
      CALL JHSTRG('>')
      CALL JBASE( 1., 0., 0.)
      CALL JPLANE( 0., 1., 0.)
      CALL J3MOVE( PCMAX/15., PCMAX+36., 0. )
      CALL JHSTRG('Y')
C---
C-
      CALL JRCLOS
C--- PUT A BOX INTO LEGEND FOR MISS. ET
      CALL JIQDIL(RLEVEL)
      COLORS(NUM) = 'MAG'
      LABELS(NUM) = ' MISS ET      '
C.N.O.   NUM = NUM + 1
      CALL LEGEND(COLORS,LABELS,NUM)
  150 IF (IX.EQ.0) THEN
        IF (ETHIST) THEN
          WRITE(STR1,200) EMAX
        ELSE
          WRITE(STR1,201) EMAX
        ENDIF
        WRITE(STR2,211) ILVL,MET
      ELSE
        IF (EMAX .GT. 99999.) EMAX = 99999.
        IF (MET  .GT. 99999.) MET  = 99999.
        IF (ETHIST) THEN
          WRITE(STR1,202) EMAX
        ELSE
          WRITE(STR1,203) EMAX
        ENDIF
        WRITE(STR2,212) MET
      ENDIF
      WRITE(STR3,210) IETAMN,IETAMX
      CALL PCTEXT(1,STR1)
      CALL PCTEXT(2,STR2)
      CALL PCTEXT(3,STR3)
C-
      IF ( CONLY ) THEN
        IF (IETAMN.GE.-13 .AND. IETAMX.LE.13) THEN
          CALL PXTITL(' Central Calorimeter')
        ELSEIF (IETAMX .LE. -14) THEN
          CALL PXTITL(' North End Cap Calorimeter')
        ELSEIF (IETAMN .GE. 14) THEN
          CALL PXTITL(' South End Cap Calorimeter')
        ENDIF
      ENDIF
C-
  900 CONTINUE
C-
  999 RETURN
C---
  200 FORMAT(' Max ET = ',F6.1,' GeV')
  201 FORMAT(' Max E  = ',F6.1,' GeV')
  202 FORMAT(' Max ET =',F7.1,' ADC counts')
  203 FORMAT(' Max E  =',F7.1,' ADC counts')
  210 FORMAT(' ETA(MIN:',I3,'-MAX:',I3,')')
  211 FORMAT(' MISS ET(',I1,')= ',F6.1,' GeV')
  212 FORMAT(' MISS ET=',F7.1,' ADC counts')
C-
      END
