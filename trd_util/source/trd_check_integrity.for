      LOGICAL FUNCTION TRD_CHECK_INTEGRITY(LZTRK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-    trd_check_integrity=.TRUE.  If at least one TPRL cell corresponds to 
C-     the cell hit  by the track or to the nearest cell
C     (If the the number of TPRL cells is >0)
C-   Created  12-Dec-1994  Y. Ducros
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZZTRK.LINK/LIST'
      INCLUDE 'D0$INC:isacan.INC'
C
      REAL DCEL(3),VERSION
      INTEGER NA(3),NC(3),ICEL(3),IA,NUMRUN
C
C      LOGICAL TRD_TEST
      LOGICAL RUN1A
      REAL UX,UY,UZ
      REAL X2,Y2,Z2,XP2,YP2,ZP2,XC(3),YC(3),ZC(3),RC(3)
      REAL DICEL,PHIXEL
      REAL XEL,YEL,ZEL
      INTEGER LTRDT,LTPRL,ILAY,IER
      INTEGER LZTRK,LZFIT
      INTEGER ANUM
      REAL Z(3)
      REAL RMAX
      REAL DICMIN,DISPRO
      REAL ZCHSEL
      REAL REAL_WORD(300)
      REAL CELTOT(3)
      REAL PI,DPI
      INTEGER INTEGER_WORD(300),NUMIN,NUMCAN
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA CELTOT/256.,256.,512./
C----------------------------------------------------------------------
C
C       book histograms in directory DST
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        PI=4*ATAN2(1.,1.)
        DPI=2.*PI
        ZCHSEL=90.
      ENDIF
C
C **** Parametres de la trace 
C
      TRD_CHECK_INTEGRITY=.TRUE.
      CELTOT(3)=512
      IF(RUN1A())CELTOT(3)=256.
      IF(LZTRK.LE.0) THEN
C        TRD_CHECK_INTEGRITY=.FALSE.
        GO TO 999
      END IF
      LZFIT=LQ(LZTRK-1)
C *****  Si la trace Electron n'a pas de bank ZFIT on sort  ******
      IF(LZFIT.LE.0) THEN
        TRD_CHECK_INTEGRITY=.FALSE.
        GO TO 999
      END IF
      CALL VZERO(ICEL,3)
      CALL VFILL(DCEL,3,-1000.)
      XEL=Q(LZFIT+11)
      YEL=Q(LZFIT+12)
      ZEL=Q(LZFIT+15)
      UX=Q(LZFIT+20)
      UY=Q(LZFIT+22)
      UZ=Q(LZFIT+24)
      X1=XEL
      Y1=YEL
      Z1=ZEL
      UX1=UX
      UY1=UY
      UZ1=UZ
      XV1=0.
      YV1=0.
      ZV1=0.
      UI1=0.
      VI1=0.
      WI1=1.
C  *****  INTERSECTION OF THE TRACK WITH THE LAYER 2 
      RMAX=47.8
      CALL TRD_CYLIND(RMAX,X2,Y2,Z2,XP2,YP2,ZP2)
      XC(3)=X2
      YC(3)=Y2
      ZC(3)=Z2
      RC(3)=SQRT(X2**2+Y2**2)
C
C***  HIT CELL AND POSITION OF THE TRACK IN THE CELL : ICEL() et DCEL()
C
      IF(ABS(Z2).LE.ZCHSEL) THEN
        PHIXEL=ATAN2(-Y2,-X2)+PI
        ICEL(3)=INT(PHIXEL*CELTOT(3)/DPI)+1
        DCEL(3)=PHIXEL*CELTOT(3)/DPI-FLOAT(ICEL(3))+0.5
      END IF
C  **** LAYER 1 *******
      RMAX=37.25
      CALL TRD_CYLIND(RMAX,X2,Y2,Z2,XP2,YP2,ZP2)
      IF(ABS(Z2).LE.ZCHSEL) THEN
        PHIXEL=ATAN2(-Y2,-X2)+PI
        ICEL(2)=INT(PHIXEL*CELTOT(2)/DPI)+1
        DCEL(2)=PHIXEL*CELTOT(2)/DPI-FLOAT(ICEL(2))+0.5
      END IF
C  **** LAYER  0 ******
      RMAX=26.7
      CALL TRD_CYLIND(RMAX,X2,Y2,Z2,XP2,YP2,ZP2)
      IF(ABS(Z2).LE.ZCHSEL) THEN
        PHIXEL=ATAN2(-Y2,-X2)+PI
        ICEL(1)=INT(PHIXEL*CELTOT(1)/DPI)+1
        DCEL(1)=PHIXEL*CELTOT(1)/DPI-FLOAT(ICEL(1))+0.5
      END IF
C
C  ***** INFORMATIONS FROM THE TRD BANKS *********
C
      LTRDT=LQ(LZTRK-9)
C
      IF(LTRDT.LE.0) GO TO 999
      DO 24 ILAY=1,3
C
        NA(ILAY)=0
        LTPRL=LQ(LTRDT-ILAY)
        IF(LTPRL.LE.0) GO TO 24
        CALL UNPACK_TPRL(LTPRL,VERSION,REAL_WORD,INTEGER_WORD,IER)
C
        NA(ILAY)=INTEGER_WORD(4)
C        IF(NA(ILAY).GT.4) GO TO 999
        IF(NA(ILAY).GT.0) THEN
          NUMCAN=0
          NUMIN=0
C
          DO 21 IA=1,NA(ILAY)
            ANUM=INTEGER_WORD(50+IA)
            DICEL=FLOAT(ANUM-ICEL(ILAY))
            IF(ABS(DICEL).GT.CELTOT(ILAY)/2.)
     #        DICEL=DICEL-SIGN(CELTOT(ILAY),DICEL)
            IF(DICEL.EQ.0.) THEN
              NUMCAN=IA
            END IF
            DISPRO=ABS(DICEL-DCEL(ILAY))
            IF(ABS(DICEL).EQ.1.) THEN
              NUMIN=IA
            END IF
   21     CONTINUE
C  ****  NO TPRL CELL CORRESPONDING TO THE HIT CELL FOR LAYER ILAY
          IF(NUMCAN.EQ.0.AND.NUMIN.EQ.0) THEN
            TRD_CHECK_INTEGRITY=.FALSE.
            GO TO 999
          END IF
C
        END IF
   24 CONTINUE
C**********************************************************
  999 CONTINUE
      RETURN
      END
       

