      SUBROUTINE ISA_PROFIL(NCELL,IETAC,IPHIC,LAYERC,IEH,THETA,P,NCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : use bock parametrization to distribute
C-                         calorimeter energy longitudinally via
C-                         look-up tables
C-
C-   Inputs  : nch:                     # channels with energy dep.
C-             ieh:                     em/hadronic switch
C-             theta:                   polar angle of track
C-             ncell,ietac,iphic,layerc:#cells hit, hit physics coords
C-   Outputs : NCH = updated number of channels filled
C-   Controls:
C-
C-   Created  28-SEP-1990   Samuel Aronson
C-   Updated  11-OCT-1990   Serban D. Protopopescu   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCAEP.INC'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INTEGER NCLMAX
      PARAMETER (NCLMAX=20)
      INTEGER IETAC(*),IPHIC(*),LAYERC(*),PAKADR
      INTEGER NCELL,IETA,IPHI,LAYER,NR
      INTEGER NALOC,NCH,LCAEP,GZCAEP,LDCAEP,I,J
      INTEGER IFLAG,IEBIN,IEH,ITR,IDR,ITA,IDA
      REAL    P(4)
      REAL    RADL(NCLMAX),ABSL(NCLMAX),DEPTH,RAD_LEN,ABS_LEN
      REAL    THETA,EBIN,TRADL,DRADL,QT,QEBOCK(15,50),QD
      REAL    DINT,TABSL,DABSL,DDINT,QHBOCK(15,200),F(NCLMAX)
C&IF VAXVMS
      BYTE BYTES(4)
      EQUIVALENCE (PAKADR,BYTES)
C&ENDIF
      REAL AE,BE,QE,XE,DXE,DQE
      REAL AH,BH,CH,W,E,R,QH,XH,DXH,DQH
      REAL AE1,AE2,BE1,BE2
      REAL AH1,AH2,BH1,BH2,CH1,CH2,W1,W2
      REAL EI,ALGEE,RANF
      LOGICAL FIRST,CELL_LOOP,FIRST_CELL
      SAVE QHBOCK,QEBOCK,FIRST
      DATA FIRST/.TRUE./
      DATA AE1,AE2,BE1,BE2/1.65,0.47,0.50,0.0/
      DATA AH1,AH2,BH1,BH2,CH1,CH2,W1,W2/0.62,0.32,0.22,0.0,0.91,
     &     -0.02,0.46,0.0/ 
      DATA R,DXE,DXH/28.,1.0,0.05/
C----------------------------------------------------------------------
C
C          INITIALIZATION
C        fill Bock tables first time called
C
      IF(FIRST) THEN
        DO I=1,15
          EI=0.2*(I-1)                    ! logarithmic energy bins
          E=10**EI
          XE=0.
          XH=0.
          QE=0.
          QH=0.
C-   compute energy-dependent Bock parameters
          ALGEE=ALOG(E)
          AE=AE1+AE2*ALGEE
          BE=BE1+BE2*ALGEE
          AH=AH1+AH2*ALGEE
          BH=BH1+BH2*ALGEE
          CH=CH1+CH2*ALGEE
          W=W1+W2*ALGEE
          DO J=1,50                       ! fill integral tables
            XE=XE+DXE
            DQE=(XE**AE)*EXP(-BE*XE)*DXE
            QE=QE+DQE
            QEBOCK(I,J)=QE
          ENDDO
          DO J=1,200
            XH=XH+DXH
            DQH=DXH*(W*(R*XH)**(AH-1.)*EXP(-BH*R*XH)
     &              +(1.-W)*XH**(AH-1.)*EXP(-CH*XH))
            QH=QH+DQH
            QHBOCK(I,J)=QH
          ENDDO
        ENDDO
        DO I=1,15                         ! normalize tables
          DO J=1,50
            QEBOCK(I,J)=QEBOCK(I,J)/QEBOCK(I,50)
          ENDDO
          DO J=1,200
            QHBOCK(I,J)=QHBOCK(I,J)/QHBOCK(I,200)
          ENDDO
        ENDDO
        FIRST=.FALSE.
      ENDIF
C
C             DEPOSIT ENERGY IN LAYERS
C
C               initialize RADL and ABSL
      DO I=1,NCLMAX
        RADL(I)=0.
        ABSL(I)=0.
        F(I)=0.
      ENDDO
      DO I=1,NCELL
        LAYER=LAYERC(I)
        IPHI=IPHIC(I)
        IETA=IETAC(I)
        CALL CAL_DEPTH(IETA,LAYER,DEPTH,RAD_LEN,ABS_LEN,IFLAG)
        GOTO (10,20,30,999), IFLAG
   10   RADL(I)=DEPTH/SIN(THETA)/RAD_LEN        ! central cal. cell
        ABSL(I)=DEPTH/SIN(THETA)/ABS_LEN
        GO TO 30
   20   RADL(I)=DEPTH/ABS(COS(THETA))/RAD_LEN        ! endcap cal. cell
        ABSL(I)=DEPTH/ABS(COS(THETA))/ABS_LEN
   30   CONTINUE
      ENDDO
C
C      find energy fractions from lookup tables qebock and qhbock
      EBIN=5.*ALOG10(P(4))            ! pick table energy bin
      IEBIN=EBIN
      CELL_LOOP=NCELL.GT.0
      IF((EBIN-IEBIN).GT.0.5) IEBIN=IEBIN+1
      IF(IEBIN.LT.1) IEBIN=1
      IF(IEBIN.GT.15) IEBIN=15
C
      IF(IEH.EQ.0) THEN               ! EM shower
        TRADL=0.
        DRADL=0.
        I=0
        FIRST_CELL=.TRUE.
        DO WHILE (CELL_LOOP)
          I=I+1
          DRADL=TRADL
          TRADL=TRADL+RADL(I)
          ITR=TRADL/DXE+1             ! table depth bins
          IDR=DRADL/DXE+1
          IF(ITR.LE.50) THEN
            QT=QEBOCK(IEBIN,ITR)
          ELSE
            QT=1.
          ENDIF
          IF(IDR.LE.50) THEN
            QD=QEBOCK(IEBIN,IDR)
            IF(FIRST_CELL) THEN
              QD=0
              FIRST_CELL=.FALSE.
            ENDIF
          ELSE
            QD=1.
          ENDIF
          F(I)=QT-QD
          IF(F(I).LT..0001.OR.I.EQ.NCELL) CELL_LOOP=.FALSE.
        ENDDO
C
      ELSE                            ! hadronic shower
        DINT=-ALOG(RANF())            ! pick interaction point
        TABSL=0.
        DABSL=0.
        I=0
        FIRST_CELL=.TRUE.
        DO WHILE (CELL_LOOP)
          I=I+1
          DDINT=ABSL(I)
          DINT=DINT-ABSL(I)
          IF(DINT.LT.0.) THEN ! check cell is after interaction point
            IF(DINT.GT.-ABSL(I)) DDINT=-DINT
            DABSL=TABSL
            TABSL=TABSL+DDINT
            ITA=TABSL/DXH+1                 ! table depth bins
            IDA=DABSL/DXH+1
            IF(IDA.LE.200) THEN
              QD=QHBOCK(IEBIN,IDA)
              IF(FIRST_CELL) QD=0
            ELSE
              QD=1.
            ENDIF
            IF(ITA.LE.200) THEN
              QT=QHBOCK(IEBIN,ITA)
              IF(FIRST_CELL) FIRST_CELL=.FALSE.
            ELSE
              QT=1.
            ENDIF
            F(I)=QT-QD
            IF(F(I).LT..0001.AND.ITA.NE.1) CELL_LOOP=.FALSE.
          ENDIF
          IF(I.EQ.NCELL) CELL_LOOP=.FALSE.
        ENDDO
      ENDIF
C
C             find pointer and fill CAEP bank
      LCAEP=GZCAEP()
      NR=IQ(LCAEP+2)
      DO I=1,NCELL
        IF(F(I).GT..0001) THEN    ! skip cells with little energy
          LAYER=LAYERC(I)
          IPHI=IPHIC(I)
          IETA=IETAC(I)
          IF(PTCAEP(IETA,IPHI,LAYER).EQ.0) THEN
            NCH=NCH+1
            PTCAEP(IETA,IPHI,LAYER)=NCH
            LDCAEP=LCAEP+(NCH-1)*NR
C           pack addresses
C&IF VAXVMS
            BYTES(4)=IETA
            BYTES(3)=IPHI
            BYTES(2)=LAYER
            BYTES(1)=0             ! not smeared
C&ENDIF
            IQ(LDCAEP+4)=PAKADR
            Q(LDCAEP+5)=P(4)*F(I)         ! F(I) = fraction of total in layer
          ELSE
            LDCAEP=LCAEP+(PTCAEP(IETA,IPHI,LAYER)-1)*NR
            Q(LDCAEP+5)=Q(LDCAEP+5)+P(4)*F(I)
          ENDIF
        ENDIF
      ENDDO
C
  999 RETURN
      END
