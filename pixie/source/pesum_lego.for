      SUBROUTINE PESUM_LEGO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make LEGO plot from ESUM banks
C-
C-
C-   Created   8-JUL-1992 Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
C----------------------------------------------------------------------
      INTEGER NOBJS(8),NMARK(3)
      INTEGER NX,NY
      INTEGER NXMIN,NYMIN,N
      INTEGER NXG,NYG
      INTEGER IOK,I,J,K1,K2,IFIL,IBLK,ITOT,NB
      INTEGER ICOLOR(8)
      REAL    XMIN,XMAX,YMIN,YMAX,ZMAX
      REAL    ZSCAL,VERTZ(3)
      REAL    ETMIN           ! MINIMUM ET TO APPEAR IN LEGO PLOT
      REAL    ARRAY(NPHIL,2*NETAL)
      INTEGER IARRAY(NPHIL,2*NETAL)
      CHARACTER*24 XLAB,YLAB,ZLAB
      CHARACTER*32 PLTITL
      CHARACTER*24 MESS,VXMESS(3)
      CHARACTER*10 TOBJS(8)
      CHARACTER*12 LABEL1(8),LABEL2(8)
      CHARACTER*3  COBJS(8)
      CHARACTER*3  COLOR1(8),COLOR2(8)
      CHARACTER*4  PATHNAM
C-
      DATA TOBJS /' Jets',' Muon',' Electron',' Photon',
     &            ' Tau',' Miss Et',' Esum',' '/
      DATA COBJS /'CYA','GRE','RED','RED',
     &            'CYA','MAG','MAG','FOR'/
C-
C----------------------------------------------------------------------
C-
      CALL PUGETV('PHYDIS ETMIN',ETMIN)
C-
C--- Get Information of ESUM Bank
C-
      CALL VZERO_i(NOBJS,8)
      CALL VZERO(VERTZ,3)
      CALL PU_GET_ESUM(ARRAY,IARRAY,NOBJS,VERTZ,PATHNAM,IOK)
C-
      IF (IOK .EQ. 0) THEN
        NY=NETAL*2
        NX=NPHIL
        YMIN=-NETAL
        YMAX=NETAL
        XMIN=0.
        XMAX=TWOPI
        YMIN=YMIN/10.
        YMAX=YMAX/10.
        ZMAX=-1.
        PLTITL='ET/PT ESUM('//PATHNAM//') ETA-PHI'
        XLAB='PHI'
        YLAB='ETA'
        ZLAB='ET'
        NXMIN=1
        NYMIN=1
        NXG=1
        NYG=1
        N=NX
        ZSCAL=.2
C-
C--- Draw LEGO
C-
        CALL P5LEGO(NX,XMIN,XMAX,NY,YMIN,YMAX,ETMIN,ZMAX,PLTITL,
     &              XLAB,YLAB,ZLAB,ARRAY,IARRAY,NXMIN,NYMIN,
     &              NXG,NYG,N,ZSCAL,NMARK)
C-
C--- Drawing the legend
C-
        DO J=1,8
          LABEL1(J) = ' '
          LABEL2(J) = ' '
          COLOR1(J) = ' '
          COLOR2(J) = ' '
        ENDDO
        IFIL = 0
        IBLK = 0
        DO NB = 1,8
          IF (NOBJS(NB) .GT. 0) THEN
            IF(NB.LE.3 .OR. NB.GE.7) THEN
              IFIL = IFIL + 1
              WRITE(LABEL1(IFIL),300) NOBJS(NB),TOBJS(NB)
              COLOR1(IFIL) = COBJS(NB)
            ELSE
              IBLK = IBLK + 1
              WRITE(LABEL2(IBLK),300) NOBJS(NB),TOBJS(NB)
              COLOR2(IBLK) = COBJS(NB)
            ENDIF
          ENDIF
        ENDDO
        ITOT = IFIL + IBLK
        DO J=1,IBLK
          K1 = IBLK -J+1
          K2 = ITOT -J+1
          LABEL2(K2) = LABEL2(K1)
          COLOR2(K2) = COLOR2(K1)
        ENDDO
        DO I=1,IFIL
          LABEL2(I)      = ' '
          COLOR2(I)      = 'FOR'
        ENDDO
        DO I=1,ITOT
          CALL PXCOLCTOI(COLOR2(I),ICOLOR(I))
        ENDDO
        CALL LEGEND3D(ICOLOR,LABEL2,ITOT)
        CALL LEGEND  (COLOR1,LABEL1,IFIL)
C-
        WRITE(MESS,200) ETMIN
        CALL PCTEXT(1,MESS)
C--- VTXs
        DO I=1,3
          IF (VERTZ(I) .NE. 0.) THEN
            WRITE(VXMESS(I),400) I,VERTZ(I)
            J = I + 1
            CALL PCTEXT(J,VXMESS(I))
          ENDIF
        ENDDO
      ENDIF
C-
  200 FORMAT(' PHYDIS ETMIN=',F6.2)
  300 FORMAT(I2,A10)
  400 FORMAT(' Z of VX(',I1,')=',F8.2)
C---
  999 RETURN
      END
C =================================================== end  pcdst_lego
