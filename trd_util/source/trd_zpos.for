
      SUBROUTINE TRD_ZPOS(LTRDT,LAYER,phitr_r,ztrd)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Compute Z from hit anode and cathode
C-
C-   Inputs  : Layer = TRD layer number (1 to 3)
C-   Outputs : PHITR_R : <phi> of hit anode cells
C-             ZTRD    : Z position
C-   Controls:
C-
C-   Created  27-FEB-1991   A. Zylberstejn
C-   Updated   5-JUN-1993   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.inc'
      REAL WIDCAT(3),DPHICA(3),DISTCA(3),DPHIDZ(3), OFSCAT(3)
      INTEGER NSTRIP(3)
C      INCLUDE 'D0$INC:geomtr.INC'
      INCLUDE 'D0$INC:WORKSP.INC'
      INCLUDE 'D0$INC:zebcom.INC'
      INTEGER I,J,IAIN,IAOUT,ICIN,ICOUT,IW,JC,LZTRK,LZFIT,ZEN
      INTEGER LAYER,LOUT,IFOIS,LTRDT,LTPRL,DANG(3)
      INTEGER NANODE,NCATHOD,NTRY,TRUNIT,INTW(100)
      INTEGER NUM_ANOD(4),NUM_CATH(4),tracki
      INTEGER ICELL,IWIR,NHITA,NHITC
      REAL SE,DPHIST_R,PHICAT_R,PHITR(2),PHITR_R,Z_R,ZMIN,ZMAX,ztrd
      REAL V(2),VMIN,VMAX,RW(100),TRDZCAL,ECAT,SCAT,ZI,DELANG(3)
      REAL RADEXT(3)
      REAL ENRG,VNTPRL, WCAT(3), DRCAT(3), DZCAT(3),CALPHA,SALPHA,THETAC
      REAL PI,TWOPI
      INTEGER IGTRAC,IER
      INCLUDE 'D0$INC:TRD_NWORD.INC'
      LOGICAL FIRST,DOPRINT,TRD_DO_PRINT
      DATA FIRST/.TRUE./
      DATA WCAT /0.6, 0.8, 0.79939/    !Cath. strip width in each module
      DATA DRCAT /3*0.005/             !Half-thickness of cathode strips
      DATA DZCAT /3*84.8/              !Dist. in Z from cathode read-out
      DATA RADEXT/26.7,37.25,47.80/
      DATA tracki/0/
C----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST=.FALSE.
        LOUT=TRUNIT()
        IFOIS=0
        PI=ACOS(-1.)
        TWOPI=2.*PI
        DO J=1,3
          DELANG(J)=TWOPI/FLOAT(NWIRE_PER_LAYER(J))
C  NUMBER OF CATHODE STRIPSNUMBER OF CATHODE STRIPS
          NSTRIP(J) = 256
C  WIDTH OF STRIPS
          WIDCAT(J) = WCAT(J)
C  DELTA PHI IN RADIANS BETWEEN 2 STRIPS
          DPHICA(J) = TWOPI/256.
C  DISTANCE BETWEEN 2 STRIPS (ALONG PHI)
          DISTCA(J) = TWOPI*RADEXT(J)/256.
          CALPHA=256.*WIDCAT(J)/(TWOPI*RADEXT(J))
          SALPHA=SQRT(1.-CALPHA**2)
          IF(J.EQ.2)SALPHA=-SALPHA
C  ANGLE OF CATHODE STRIPS W.R.T. BEAM
          THETAC = ASIN(SALPHA)
C  DPHI/DZ FOR CATHODE STRIPS
C        DPHIDZ(J) = TAN(THETAC)/RADEXT(J)
          DPHIDZ(J)=SALPHA/(RADEXT(J)*CALPHA)
          OFSCAT(J)=DZCAT(J)*DPHIDZ(J)
        END DO
        doprint=.true.
        lout=73
      END IF
      IFOIS=IFOIS+1
c      DOPRINT=TRD_DO_PRINT()
      IF(tracki.NE.iq(ltrdt-5))then
c        WRITE(73,*)'--------------------------------------------'
        tracki=iq(ltrdt-5)
      END IF
c      WRITE(73,*)'           enter TRD_ZPOS, track',iq(ltrdt-5),
c     &  ' layer', layer
      LTPRL=LQ(LTRDT-LAYER)
      ZTRD=-1000.
      IF(LTPRL.LE.0)GO TO 999
      CALL VZERO(REAL_WORD,NWORD)
      CALL VZERO(INTEGER_WORD,NWORD)
C unpack wire energies
      CALL UNPACK_TPRL(LTPRL,VNTPRL,REAL_WORD,INTEGER_WORD,IER)
      IF(IER.NE.0)GO TO 100
C check if there are any anode hits in this layer
      NHITA=INTEGER_WORD(4)
      IF(NHITA.LE.0)GO TO 100 ! Check if anodes are there
      NHITC=INTEGER_WORD(5)
      IF(NHITC.LE.0)GO TO 100 ! Check if cathodes are there
c      WRITE(73,*)' in ztrd, nhita',nhita,' nhitc',nhitc,' layer',
c     &  layer
C  determine phi from hit anodes
      SE=0.
      PHITR_R=0.
      DO  IWIR=1,NHITA
        ICELL=INTEGER_WORD(50+IWIR)
        ENRG=REAL_WORD(50+IWIR)
        SE=SE+ENRG
c        WRITE(LOUT,*)' cell',ICELL, 'phi',(FLOAT(ICELL)-.5)*
c     &    DELANG(LAYER)
        PHITR_R=PHITR_R+(FLOAT(ICELL)-.5)*DELANG(LAYER)*ENRG
      END DO
      IF(SE.LE.0.)GO TO 100
      PHITR_R=PHITR_R/SE
   40 CONTINUE
      ZMIN=1000.
      ZMAX=-1000.
      SCAT=0.
      SE=0.
      ZEN=0.
      DO  IWIR=1,NHITC
        ICELL=INTEGER_WORD(50+NHITA+IWIR)
        ENRG=REAL_WORD(50+NHITA+IWIR)
        DPHIST_R=FLOAT(ICELL-1)*DPHICA(LAYER)
        PHICAT_R=DPHIST_R+OFSCAT(LAYER)
        NTRY=0
   84   Z_R     =-(PHICAT_R-PHITR_R)/
     &        DPHIDZ(LAYER)
        IF(ABS(Z_R).GT.85.)THEN
          PHICAT_R=PHICAT_R+TWOPI
          NTRY=NTRY+1
          IF(NTRY.LT.2)GO TO 84
        END IF
c        IF(DOPRINT)THEN
c          WRITE(LOUT,*)' ntry',NTRY,' ztrd',Z_R
c          WRITE(LOUT,*)' in ztrd,phicat',PHICAT_R,
c     &      ' dphist_r',DPHIST_R,' z',Z_R, 'phitr_r',PHITR_R,
c     &      ' cell cath',ICELL,' ofscat',OFSCAT(LAYER),' dphidz',
c     &      DPHIDZ(LAYER)
c        END IF
        IF(ABS(Z_R).LT.100.)THEN
          SE=SE+ENRG
          ZEN=ZEN+Z_R*ENRG
        END IF
      END DO
  120 CONTINUE
  100 CONTINUE
      ZTRD=-1000.
      IF(SE.NE.0.) ZTRD=ZEN/SE
  999 RETURN
      END
