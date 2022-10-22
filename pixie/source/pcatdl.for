      SUBROUTINE PCATDL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make ET LEGO plot from CATD bank in DST which
C-                         has packed ieta, iphi and Et of CATE bank.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Modified 10-MAR-1994   Nobuaki Oshima
C-        Fixed overwrite problem on ARRAY1 and ARRAY2
C-   Created  10-DEC-1991   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
C
      INTEGER LCATD,IPNTEM,IPNTHD,IPNTMU,NEMTWR,NHDTWR,NMUTWR
      INTEGER KCATD,ISEC,IETA,IPHI
      INTEGER GZCATD, IE,IP,IENER,IMARK
      INTEGER NXMIN,NYMIN,N,NXG,NYG,NX,NY
      INTEGER I,J,NTOWER,IER
      REAL    XMIN,XMAX,YMIN,YMAX,ZMAX
      REAL    ZSCAL, ENER,ECUT,ETMIN,ETMNEM,ETMNHD,EMNMUO
      REAL    ETA,PHI,DELETA,E(4), ET, THETA
      REAL    ARRAY1(NPHIL,2*NETAL),ARRAY2(NPHIL,2*NETAL)
      CHARACTER*3 COL1,COL2
      CHARACTER*24 XLAB,YLAB,ZLAB
      CHARACTER*32 PLTITL
      CHARACTER*27 MESS1
      LOGICAL EZERROR,EORET,CALPLOT
C
      DATA COL1,COL2/'RED','CYA'/
      DATA IMARK/1/
C----------------------------------------------------------------------
C-
C--- Select correct RCP bank and get min. E/Et for towers to be plotted
      CALL EZPICK('PX_CALDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PCATDL',
     &    'Unable to pick RCP bank PX_CALDIS_RCP','W')
        GOTO 999
      ELSE
        CALL PUGETV('CALEGO EMIN',ECUT)
        CALL PUGET_l('CAL ETORE',EORET)
        CALL EZRSET
      ENDIF
C-
C--- fill ARRAY1(EM) and ARRAY2(HAD) by CATD bank contents
C-
      CALL VZERO(ARRAY1,NPHIL*2*NETAL)
      CALL VZERO(ARRAY2,NPHIL*2*NETAL)
C-
C--- CHECK CATE BANK IS AVAILABLE OR NOT...
      CALL GTCATD (LCATD,IPNTEM,IPNTHD,IPNTMU,
     &           NEMTWR,NHDTWR,NMUTWR,ETMNEM,ETMNHD,EMNMUO)
      IF (LCATD .LE. 0) THEN
        CALL PUMESS(' PCATDL-CATD BANK DOES NOT EXIST')
        GO TO 999            ! EXIT from routine
      ENDIF
C-
      DO J=1,2
        IF(J .EQ. 1) THEN
          NTOWER = NEMTWR
        ELSE
          NTOWER = NHDTWR
        ENDIF
        ISEC = J
        DO I=1,NTOWER
          IF(J .EQ. 1) THEN
            KCATD = LCATD + IPNTEM + I
          ELSE
            KCATD = LCATD + IPNTHD + I
          ENDIF
          CALL UPCATD(KCATD,ISEC,IETA,IPHI,ETA,PHI,DELETA,E)
          IF ( EORET ) THEN
            ET = SQRT(E(1)*E(1) + E(2)*E(2))
            IF (ETA .GT. 3.7) THEN
              ETA=3.7
            ELSEIF(ETA .LT. -3.7) THEN
              ETA=-3.7
            ENDIF
            IETA = 10.*ETA
            IP = (PHI/TWOPI)*64 + 1
          ELSE
            ET = E(4)
            IP = IPHI
          ENDIF
C-
          IF(IETA .LT. 0)THEN
            IE = IETA + NETAL + 1
          ELSE
            IE = IETA + NETAL
          ENDIF
          IF (ISEC .EQ. 1) THEN
            ARRAY1(IP,IE) = ARRAY1(IP,IE) + ET
          ELSE
            ARRAY2(IP,IE) = ARRAY2(IP,IE) + ET
          ENDIF
        ENDDO
      ENDDO
C-
C--- difine LEGO
C-
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
      IF ( EORET ) THEN
        YMAX= NETAL/10.
        YMIN=-NETAL/10.
        XLAB='PHI'
        YLAB='ETA'
        ZLAB='ET'
        CALPLOT = .FALSE.                ! Cal Plot ET
        PLTITL='CATD ETA-PHI ET'
      ELSE
        YMAX= NETAL
        YMIN=-NETAL
        XLAB='IPHI'
        YLAB='IETA'
        ZLAB='E'
        CALPLOT = .TRUE.                 ! Cal Plot E
        PLTITL='CATD ETA-PHI E'
      ENDIF
C-
C--- do PLOT
      CALL P2LEGO(NX,XMIN,XMAX,NY,YMIN,YMAX,ECUT,ZMAX,PLTITL,
     X     XLAB,YLAB,ZLAB,COL1,COL2,ARRAY1,ARRAY2,
     X     NXMIN,NYMIN,NXG,NYG,N,ZSCAL,IMARK,CALPLOT)
C-
      IF (EORET) THEN
        WRITE(MESS1,222) ECUT
  222   FORMAT('CATD LEGO ETMIN =',F6.2,' GeV')
      ELSE
        WRITE(MESS1,224) ECUT
  224   FORMAT('CATD LEGO EMIN  =',F6.2,' GeV')
      ENDIF
      CALL PCTEXT(1,MESS1)
C-
C----------------------------------------------------------------------
  999 RETURN
      END
