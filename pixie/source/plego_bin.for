      SUBROUTINE PLEGO_BIN(IMARK,TXMIN,TXMAX,TYMIN,TYMAX,ZDIV,ETCUT,
     X  NMARK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Put special bin on LEGO PLOT
C-
C-   Inputs  : IMARK - Kind of bin
C-             IMARK=1; MISS ET
C-             IMARK=2; elec
C-             IMARK=3; photon
C-             IMARK=4; tau
C-             TXMIN - Min of y lego plot
C-             TXMAX - Max of x lego plot
C-             TYMIN - Min of y lego plot
C-             TYMAX - Max of y lego plot
C-             ZDIV  - Scale factor
C-             ETCUT - minimum ET to plot value
C-   Output  : NMARK - number of marks of this type plotted
C-
C-   Modified  26-JUL-1993   Nobuaki Oshima
C-      Get an EM fraction of Electron/Photon from CACL instead of the
C-     energy of isolation cone of PELC/PPHO Bank.
C-   Created   10-JUN-1992   Sharon Hagopian
C-   based on PLMARK
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C ARGUMENTS:
      INTEGER IMARK  ! FLAG to tell which kind of BIN to put
      INTEGER NMARK ! number of marks of ITYPE plotted
      INTEGER IT,IER
      INTEGER NUM_TAU
      REAL    ETAU(4),ETTAU,THETAT,ETAT,PHIT,RMS_WIDTH
      REAL TXMIN,TXMAX ! Min and max of lego plot x axis
      REAL TYMIN,TYMAX ! Min and max of lego plot y axis
      REAL ZDIV !Z SCALE FACTOR
      REAL ETCUT
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
C------------------------------------------------------------------------
      INTEGER LPNUT,GZPNUT,IPASS
      INTEGER IPHI,IETA,IP
      INTEGER LPELC,GZPELC
      INTEGER LPPHO,GZPPHO
      INTEGER LCACL,IPTAU,J
      INTEGER NUM_ELC
      REAL ET,TE,EM,ETA,PHI,TET,EMT
      REAL HAD,EMFRAC,ELC_ETA(10),ELC_PHI(10)
      REAL TX,TY,Z1,Z2,Z3,DELZ
      REAL XSIZ,YSIZ,DRET,DRMIN
      CHARACTER*8 TITLE(4)
      SAVE NUM_ELC,ELC_ETA,ELC_PHI
      DATA TITLE/'Miss ET ','  ELEC  ','  PHO   ','  TAU   '/
C------------------------------------------------------------------------
      NMARK=0
      IF(IMARK.LE.0)GO TO 999
      IF(IMARK.EQ.1)THEN
C-
C--- Missing ET
C-
        DO 10 IPASS = 5,1,-1
          LPNUT = GZPNUT(IPASS)
          IF( LPNUT .GT. 0)   GO TO 20
   10   CONTINUE
        GO TO 100
C-
   20   IPHI=(Q(LPNUT+10)/TWOPI)*64+1
        ETA=Q(LPNUT+9)
        IETA=10.*ETA
        IF(ETA.LT.0)THEN
          IETA=IETA+NETAL+1
        ELSE
          IETA=IETA+NETAL+2
        ENDIF
        ET=Q(LPNUT+7)
        IF(ET.LT.ETCUT) GO TO 999
        NMARK=1
C        ETERR=Q(LPNUT+13)
        TX=IPHI
C Set missing Et at ETA=0.
        TY=37.
        IF(TX.LT.TXMIN.OR.TX.GT.TXMAX)GO TO 100
        Z1=ET/ZDIV
        CALL JLSTYL(1)
        CALL JPINTR(0) ! No fill
        CALL PXCOLR('MAG') ! Filling with magenta
C DRAW VBAR
        CALL PLDBAR(3,TX,TX+1.,TY+.5,TY+1.5,0.,Z1,0)
C label line
        XSIZ=1.0
        YSIZ=.67
        CALL PXCOLR('FOR')
        CALL JSIZE(XSIZ,YSIZ)
        DELZ=1.
        CALL J3MOVE(TX,TY,Z1+DELZ)
        CALL JJUST(2,2)
        CALL J1STRG(TITLE(1))
C Draw vertical line for edge of bin
        CALL J3MOVE(TX+.5,TY+1.,Z1+(DELZ/2.))
        CALL J3DRAW(TX+.5,TY+1.,Z1)
  100   CONTINUE
        GO TO 999
      ELSE IF(IMARK.EQ.2)THEN
C- FOR ELECTRONS
C--- Start PELC loop...
C-
        NUM_ELC = 0
        CALL VZERO(ELC_ETA,10)
        CALL VZERO(ELC_PHI,10)
        LPELC = GZPELC()
   50   IF (LPELC .LE. 0)     GO TO 999
        ET      =  Q(LPELC+7)
        IF(ET.LT.ETCUT)GO TO 56
        NMARK=NMARK+1
        LCACL = LQ(LPELC-2)
        IF (LCACL .GT. 0) THEN
          TE     = Q(LCACL+7)
          HAD    = Q(LCACL+19)
          EM     = TE - HAD
          EMFRAC = EM/TE
          TET    = ET
          EMT    = ET*EMFRAC
        ELSE
          GO TO 56
        ENDIF
        ETA=Q(LPELC+9)
        PHI=Q(LPELC+10)
        IF (NMARK.GT.0 .AND. NMARK.LE.10) THEN
          NUM_ELC = NMARK
          ELC_ETA(NMARK) = ETA
          ELC_PHI(NMARK) = PHI
        ENDIF
C      WRITE(72,803)NELE,ETA,PHI,ET
  803   FORMAT(/' NELE=',I4,' ETA=',F12.1,' PHI='F12.1,' ET='F12.1)
        IP=(PHI/TWOPI)*64. +1.
        IETA=10.*(ETA+3.7)+1.
C      WRITE(72,802)IP,IETA
        IF(IP.GT.0.AND.IP.LE.NPHIL  .AND.
     +    IETA.GT.0.AND.IETA.LE.2*NETAL)GO TO 55
        WRITE(6,*)' ERROR - IP=',IP,' IETA=',IETA
        GO TO 56
   55   CONTINUE
        TX=IP
        TY=IETA
        IF(TX.LT.TXMIN.OR.TX.GT.TXMAX)GO TO 999
        Z1=EMT/ZDIV
        Z2=TET/ZDIV
C        CALL JLSTYL(1)
C        IFILL=1
C        CALL JPINTR(IFILL) !  fill
C        CALL PXCOLR('FOR') ! Filling with FORGROUND
        CALL PXCOLFILL('RED')
C DRAW VBAR
        CALL PLDBAR(3,TX,TX+1.,TY+.5,TY+1.5,0.,Z1,0)
C        IFILL2=2
C        CALL JPINTR(IFILL2) !  fill
C        CALL PXCOLR('FOR') ! Filling with FORGROUND
        CALL PXCOLFILL('CYA')
C DRAW VBAR
        CALL PLDBAR(3,TX,TX+1.,TY+.5,TY+1.5,Z1,Z2,0)
C label line
        XSIZ=1.0
        YSIZ=.67
        CALL JSIZE(XSIZ,YSIZ)
        DELZ=1.
        CALL J3MOVE(TX,TY,Z2+DELZ)
        CALL JJUST(2,2)
        CALL J1STRG(TITLE(2))
C Draw vertical line to point to bin
        CALL J3MOVE(TX+.5,TY+1.,Z2+(DELZ/2.))
        CALL J3DRAW(TX+.5,TY+1.,Z2)
C--- GO TO THE NEXT PELC BANK
C-
   56   LPELC = LQ(LPELC)
        GO TO 50
      ELSE IF(IMARK.EQ.3)THEN
C PHOTONS  !!!!!!!!!!!!
        LPPHO = GZPPHO()
        IF (LPPHO .LE. 0) GO TO 999
C-
C--- Start PPHO loop...
C-
  650   IF (LPPHO .LE. 0)     GO TO 999
        ET      =  Q(LPPHO+7)
        IF(ET.LT.ETCUT)GO TO 656
        NMARK=NMARK+1
        LCACL = LQ(LPPHO-2)
        IF (LCACL .GT. 0) THEN
          TE     = Q(LCACL+7)
          HAD    = Q(LCACL+19)
          EM     = TE - HAD
          EMFRAC = EM/TE
          TET    = ET
          EMT    = ET*EMFRAC
        ELSE
          GO TO 656
        ENDIF
        ETA=Q(LPPHO+9)
        PHI=Q(LPPHO+10)
C      WRITE(72,803)NPHO,ETA,PHI,ET
        IP=(PHI/TWOPI)*64. +1.
        IETA=10.*(ETA+3.7)+1.
C      WRITE(72,802)IP,IETA
        IF(IP.GT.0.AND.IP.LE.NPHIL  .AND.
     +    IETA.GT.0.AND.IETA.LE.2*NETAL)GO TO 655
C      WRITE(6,*)' ERROR - IP=',IP,' IETA=',IETA
        GO TO 656
  655   CONTINUE
        TX=IP
        TY=IETA
        IF(TX.LT.TXMIN.OR.TX.GT.TXMAX)GO TO 999
        Z1=EMT/ZDIV
        Z2=TET/ZDIV
C        CALL JLSTYL(1)
C        CALL JPINTR(IFILL) !  fill
C        CALL PXCOLR('FOR') ! Filling
        CALL PXCOLFILL('RED')
C DRAW VBAR
        CALL PLDBAR(3,TX,TX+1.,TY+.5,TY+1.5,0.,Z1,0)
C        CALL JPINTR(IFILL2) !  fill
C        CALL PXCOLR('FOR') ! Filling
        CALL PXCOLFILL('CYA')
C DRAW VBAR
        CALL PLDBAR(3,TX,TX+1.,TY+.5,TY+1.5,Z1,Z2,0)
C label line
        XSIZ=1.0
        YSIZ=.67
        CALL JSIZE(XSIZ,YSIZ)
        DELZ=1.
        CALL J3MOVE(TX,TY,Z2+DELZ)
        CALL JJUST(2,2)
        CALL J1STRG(TITLE(3))
C Draw vertical line for edge of bin
        CALL J3MOVE(TX+.5,TY+1.,Z2+(DELZ/2.))
        CALL J3DRAW(TX+.5,TY+1.,Z2)
C--- GO TO THE NEXT PELC BANK
C-
  656   LPPHO = LQ(LPPHO)
        GO TO 650
      ELSE IF(IMARK.EQ.4)THEN
C TAUS  !!!!!!!!!!!!
C====== Process PTAU ================================================
C-
C-
        CALL PUGETV('PHYDIS DRAW PTAU',IPTAU)
        IF (IPTAU .EQ. 0) GO TO 999
C-
        CALL GTPTAU_TOTAL(NUM_TAU,IER)
        IF (NUM_TAU.EQ.0.OR. IER .NE. 0) GO TO 999
C-
C--- Start PTAU loop...
C-
        DO 756 IT = 1,NUM_TAU
          CALL GTPTAU(IT,ETAU,ETTAU,THETAT,ETAT,PHIT,RMS_WIDTH,IER)
C-
          NMARK=NMARK+1
C-
C--- Check overlap Tau and Electron
C-
          DRMIN = 10.
          DO J=1,NUM_ELC
            DRET = SQRT((ELC_ETA(J)-ETAT)**2 + (ELC_PHI(J)-PHIT)**2)
            IF (DRET .LT. DRMIN)   DRMIN = DRET
          ENDDO
          IF (DRMIN .LT. .3) GO TO 756
C-
          IP=(PHIT/TWOPI)*64. +1.
          IETA=10.*(ETAT+3.7)+1.
C      WRITE(16,802)IP,IETA
          IF(IP.GT.0.AND.IP.LE.NPHIL  .AND.
     +      IETA.GT.0.AND.IETA.LE.2*NETAL)GO TO 755
C      WRITE(6,*)' ERROR - IP=',IP,' IETA=',IETA
          GO TO 756
  755     CONTINUE
          TX=IP
          TY=IETA
          IF(TX.LT.TXMIN.OR.TX.GT.TXMAX)GO TO 999
          Z1=ETTAU/ZDIV
          Z2=Z1 +DELZ
C DRAW VBAR
C        CALL PLDBAR(3,TX,TX+1.,TY+.5,TY+1.5,0.,Z1,0)
C        CALL PXCOLFILL('BLU')
C DRAW VBAR
C        CALL PLDBAR(3,TX,TX+1.,TY+.5,TY+1.5,Z1,Z2,0)
C label line
          XSIZ=1.0
          YSIZ=.67
          CALL JSIZE(XSIZ,YSIZ)
          DELZ=1.
          CALL J3MOVE(TX,TY,Z2+DELZ)
          CALL JJUST(2,2)
          CALL J1STRG(TITLE(4))
C Draw vertical line for edge of bin
          Z3=Z2+(DELZ/2.)
          CALL J3MOVE(TX+.5,TY+1.,Z3)
          CALL J3DRAW(TX+.5,TY+1.,0.)
C DRAW TOP OF T
          CALL J3MOVE(TX+.5,TY+1.,Z3)
          CALL J3DRAW(TX+1.,TY+1.,Z3)
          CALL J3MOVE(TX+.5,TY+1.,Z3)
          CALL J3DRAW(TX-1.,TY+1.,Z3)
  756   CONTINUE
      ENDIF
C-
  999 RETURN
      END
