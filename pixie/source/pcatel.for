      SUBROUTINE PCATEL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make ET LEGO plot for D0 calorimeter
C-                         from CATE bank (Eta,phi corrected for vertex)
C-                         EM energy in BLUE, hadronic energy in RED
C-
C-   Inputs  : none
C-   Outputs :
C-   Controls:
C-
C-   Modified 30-DEC-1991   Nobu Oshima
C-      Add an E or ET switch to be selected by the parameter 'CAL ETORE'
C-      in PX_CALDIS_RCP.
C-   Modified 18-AUG-1991   Nobu Oshima ( Change ETA & PHI Label )
C-   Updated  13-MAY-1991   Nobu Oshima ( Fix a bug. )
C-   Updated   3-DEC-1990   Harrison B. Prosper
C-      Make sure correct bank is selected.
C-   Modified 19-JAN-1990   Nobu. (changed ETA cut logic after ETOETA)
C-   Modified 12-OCT-1989   Nobu. (Activate JTYPE for the special user)
C-   Created  21-MAR-1989   S. Hagopian
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZHITS.LINK'
      INCLUDE 'D0$LINKS:IZCAHT.LINK'
      INCLUDE 'D0$LINKS:IZCATE.LINK'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
      REAL    E(4),PHI,THETA,ETA
      REAL    ARRAY1(NPHIL,2*NETAL),ARRAY2(NPHIL,2*NETAL)
      INTEGER I,NCH,NREP,IER
      INTEGER IL,IP,IE,LEN,INX
      INTEGER IETA,JTYPE
      CHARACTER*3 COL1,COL2
      INTEGER IEND
      INTEGER GZCATE,LCATE,IFL
C----------------------------------------------------------------------
      INTEGER IMARK
      INTEGER NXMIN,NYMIN,N
      INTEGER NXG,NYG
      INTEGER NX,NY
      REAL    XMIN,XMAX,YMIN,YMAX,ZMAX
      REAL    ZSCAL
      REAL    EMIN   ! MINIMUM ENERGY/CELL  TO APPEAR IN LEGO PLOT
      CHARACTER*24 XLAB,YLAB,ZLAB
      CHARACTER*32 PLTITL
      CHARACTER*8  CEMIN
      CHARACTER*16 IMESS
      CHARACTER*36 MESS1
      LOGICAL EZERROR,ETORE,CALPLOT
C =======================================================================
      DATA COL1,COL2/'RED','CYA'/
      DATA IMARK/1/
C ========================================================================
C-
C--- CHECK CATE BANK IS AVAILABLE OR NOT...
      IF(GZCATE().LE.0) THEN
        CALL PUMESS('%PCATEL - No CATE Bank!!! Use CATD Bank...')
        CALL PCATDL
        GO TO 999            ! EXIT from routine
      ENDIF
C--- Get minimum energy for cell to be plotted
C--- Select correct RCP bank
      CALL EZPICK('PX_CALDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PCATEL',
     &    'Unable to pick RCP bank PX_CALDIS_RCP','W')
        GOTO 999
      ENDIF
C-
      CALL PUGETV('CALEGO EMIN',EMIN)
      CALL PUGETV('CAL ETORE',ETORE)
      CALL PUGETV('JET TYPE',JTYPE)
C-
      CALL EZRSET
C-
C--- Fill ARRAY1 and ARRAY2 here...
C-
      LCATE = GZCATE()
      CALL VZERO(ARRAY1,NPHIL*2*NETAL)
      CALL VZERO(ARRAY2,NPHIL*2*NETAL)
   20 IF(LCATE.LE.0)GO TO 150
      NCH=IQ(LCATE-1)
      IF(NCH.LE.1)GO TO 150
      NREP=IQ(LCATE+2)
      DO 100 I = 4,NCH,NREP
        INX = LCATE + I
        E(4)=Q(INX+3)
        IF(E(4).LE.0.) GO TO 100
        E(1)=Q(INX)
        E(2)=Q(INX+1)
        E(3)=Q(INX+2)
        IF ( ETORE ) THEN
          CALL ETOETA(E,PHI,THETA,ETA)
          IF(ETA.GT.3.7)THEN
            ETA=3.7
          ELSE IF(ETA.LT.-3.7)THEN
            ETA=-3.7
          ENDIF
          IETA=10.*ETA
          IP=(PHI/TWOPI)*64 +1
        ELSE
          IETA = IQ(INX+8)
          IP   = IQ(INX+9)
        ENDIF
        IF(IETA .LT. 0)THEN
          IETA=IETA+NETAL+1
        ELSE
          IETA=IETA+NETAL
        ENDIF
C-
C--- EM ET OR E
        IF(IQ(INX+10).EQ.1)THEN
          IF ( ETORE ) THEN
            ARRAY1(IP,IETA)=ARRAY1(IP,IETA)+Q(INX+4)   ! 4:ET
          ELSE
            ARRAY1(IP,IETA)=ARRAY1(IP,IETA)+Q(INX+3)   ! 3:E
          ENDIF
        ELSE
C--- TOTAL ET OR E
          IF ( ETORE ) THEN
            ARRAY2(IP,IETA)=ARRAY2(IP,IETA)+Q(INX+4)   ! 4:ET
          ELSE
            ARRAY2(IP,IETA)=ARRAY2(IP,IETA)+Q(INX+3)   ! 3:E
          ENDIF
        ENDIF
  100 CONTINUE
C--- GO TO THE NEXT CATE BANK
   21 LCATE=LQ(LCATE)
      GO TO 20
  150 CONTINUE
C--- SEAPARATE HAD ENERGY
      IEND=2*NETAL
      DO 200 IE=1,IEND
        DO 200 IP=1,NPHIL
  200 ARRAY2(IP,IE)=ARRAY2(IP,IE)-ARRAY1(IP,IE)
C-
C---
      NY=NETAL*2
      NX=NPHIL
      XMIN=1.
      XMAX=NPHIL
      ZMAX=-1.
      NXMIN=1
      NYMIN=1
      NXG=1
      NYG=1
      N=NX
      ZSCAL=.2
      IF ( ETORE ) THEN
        YMAX= NETAL/10.
        YMIN=-NETAL/10.
        XLAB='PHI'
        YLAB='ETA'
        ZLAB='ET'
        CALPLOT = .FALSE.                 ! Cal Plot ET
        PLTITL='CATE ETA-PHI ET'
      ELSE
        YMAX= NETAL
        YMIN=-NETAL
        XLAB='IPHI'
        YLAB='IETA'
        ZLAB='E'
        CALPLOT = .TRUE.                  ! Cal Plot E
        PLTITL='CATE ETA-PHI E'
      ENDIF
C-
C--- PLOT EM ONLY
      IF ( JTYPE.EQ.1) THEN
        CALL P1LEGO(NX,XMIN,XMAX,NY,YMIN,YMAX,EMIN,ZMAX,PLTITL,
     &     XLAB,YLAB,ZLAB,COL1,ARRAY1,
     &     NXMIN,NYMIN,NXG,NYG,N,ZSCAL,IMARK)
C--- PLOT TOTAL( EM+HAD )
      ELSE
        CALL P2LEGO(NX,XMIN,XMAX,NY,YMIN,YMAX,EMIN,ZMAX,PLTITL,
     &     XLAB,YLAB,ZLAB,COL1,COL2,ARRAY1,ARRAY2,
     &     NXMIN,NYMIN,NXG,NYG,N,ZSCAL,IMARK,CALPLOT)
      ENDIF
C-
      IF (JTYPE.EQ.1) THEN
        IF ( ETORE ) THEN
          WRITE(MESS1,221) EMIN
        ELSE
          WRITE(MESS1,222) EMIN
        ENDIF
      ELSE
        IF ( ETORE ) THEN
          WRITE(MESS1,223) EMIN
        ELSE
          WRITE(MESS1,224) EMIN
        ENDIF
      ENDIF
  221 FORMAT('CALEGO ETMIN',F6.2,' GeV-EM Towers')
  222 FORMAT('CALEGO EMIN',F6.2,' GeV-EM Towers')
  223 FORMAT('CALEGO ETMIN',F6.2,' GeV-TOTAL Towers')
  224 FORMAT('CALEGO EMIN',F6.2,' GeV-TOTAL Towers')
      CALL PCTEXT(1,MESS1)
C-
  999 RETURN
      END
