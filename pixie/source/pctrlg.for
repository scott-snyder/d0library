      SUBROUTINE PCTRLG
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make a Calorimeter LEGO plot for Trigger.
C-                         ( EM energy in red, hadronic energy in blue.)
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Modified 15-MAR-1993   Nobuaki Oshima - Added Miss_Et mark.
C-   Created   3-APR-1991   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
C----------------------------------------------------------------------
      INTEGER NX,NY,IMARK
      REAL XMIN,XMAX,YMIN,YMAX,ZMAX
      CHARACTER*3 COL1, COL2, CVAL
      CHARACTER*24 XLAB,YLAB,ZLAB
      CHARACTER*32 PLTITL
      INTEGER NXMIN,NYMIN,N
      INTEGER NXG,NYG
      INTEGER IHIND,L1IPHI,L1IETA,IETA
      INTEGER IER
      REAL ETTOT,ETELM,ETHAD
      REAL ZSCAL
      REAL ARRAY1(NPHIL1,2*NETAL1)
      REAL ARRAY2(NPHIL1,2*NETAL1)
      REAL ENER,EMIN
      CHARACTER*24 MESS
      LOGICAL CALPLOT,EZERROR
C----------------------------------------------------------------------
C  Data Statements:
C
      DATA IMARK/1/
C----------------------------------------------------------------------
C-
      CALL EZPICK('PX_CALDIS_RCP')          ! Selecting Caldis bank
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PCTRLG','Bank PX_CALDIS_RCP NOT FOUND','W')
        GOTO 999
      ENDIF
C-
      CALL VZERO(ARRAY1,NPHIL1*2*NETAL1)
      CALL VZERO(ARRAY2,NPHIL1*2*NETAL1)
C--- L1IPHI & L1IETA are same order in EM(0-1279) & HAD(1280-2559)
      DO IHIND=0,1279
        CALL CHOTTT(IHIND,L1IPHI,L1IETA)
        IF (L1IETA.GE.-20 .AND. L1IETA.LE.20) THEN
          IF (L1IETA .LT. 0) THEN
            IETA = L1IETA + 21
          ELSE
            IETA = L1IETA + 20
          ENDIF
          CALL PCTRGR(L1IPHI,L1IETA,1,ETELM)
          CALL PCTRGR(L1IPHI,L1IETA,0,ETTOT)
          IF (ETELM .GT. 0.)
     &        ARRAY1(L1IPHI,IETA) = ARRAY1(L1IPHI,IETA) + ETELM
          IF (ETTOT .GT. ETELM) THEN
            ETHAD = ETTOT - ETELM
            ARRAY2(L1IPHI,IETA) = ARRAY2(L1IPHI,IETA) + ETHAD
          ENDIF
        ENDIF
      ENDDO
C-
C--- GET MINIMUM ENERGY FOR CELL TO BE PLOTTED
      CALL PUGETV('CATRIG LEGO EMIN',EMIN)
C---
      NY=NETAL1*2
      NX=NPHIL1
      YMIN=-NETAL1
      YMAX=NETAL1
      XMIN=1.
      XMAX=NPHIL1
      ZMAX=-1.
      PLTITL='ET of TRGR in ETA-PHI'
      XLAB='PHI'
      YLAB='ETA'
      ZLAB='ET'
      COL1= 'RED'   ! Red
      COL2= 'CYA'   ! Cyan
      NXMIN=1
      NYMIN=1
      NXG=1
      NYG=1
      N=NX
      ZSCAL=.2
      CALPLOT = .FALSE.                 ! Cal Plot ET
C-
      CALL P2LEGO(NX,XMIN,XMAX,NY,YMIN,YMAX,EMIN,ZMAX,PLTITL,
     X     XLAB,YLAB,ZLAB,COL1,COL2,ARRAY1,ARRAY2,
     X     NXMIN,NYMIN,NXG,NYG,N,ZSCAL,IMARK,CALPLOT)
C-
      WRITE(MESS,200) EMIN
  200 FORMAT(' CALEGO EMIN =',F4.0,' GeV')
      CALL PCTEXT(1,MESS)
C-
C--- Reset RCP bank
C-
      CALL EZRSET
  999 RETURN
      END
