      SUBROUTINE PRCATE(PRUNIT,LCATEI,NCATEI,CFL,IFL)
C-----------------------------------------------------------------------
C-
C-  Print out for CATE (calorimeter energy in hardware towers) bank
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LCATEI= bank address
C-  NCATEI= bank number
C-  CFL   = flag to control printout
C-        = 'HED' header only
C-  IFL   = not used
C-   Created   8-MAR-1989   Andrzej Zieminski
C-   Updated   7-MAY-1989   Rajendran Raja
C-----------------------------------------------------------------------

      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:DCPARA.DEF/LIST'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS/LIST'
      INTEGER PRUNIT,LCATE,NCATE,IFL,GZCATE,LCATEI,NCATEI
      INTEGER I,POINT,LDATA,ND,NR,ETA,PHI,LYR,NEM,IV
      INTEGER NCH,K,VRSION,JBIT
      INTEGER LYRS(18),NLYR,KK,ILYR
      REAL ENERGY
      CHARACTER CFL*(*)
C--------------------------------------------------------------------
C
C--   check if bank does exist
C
      LCATE=LCATEI
      NCATE=NCATEI
      IF(LCATE.EQ.0)THEN
        LCATE=GZCATE()
        IF(LCATE.LE.0) GOTO 99
      ENDIF
C
C--   Print header
C
      WRITE(PRUNIT,102)
      IF(CFL.EQ.'HED') GOTO 98
C
      IV=IQ(LCATE+1)
      NR=IQ(LCATE+2)
      NCH=IQ(LCATE+3)
      NEM=NCH/10000
      NCH=MOD(NCH,10000)
      WRITE(PRUNIT,103) IV,NR,NEM,NCH
      IF(NCH.LE.0) GOTO 98
      WRITE(PRUNIT,100)
C
C        Print contents of the bank
C
      DO 10 I=1,NCH
        IF(MOD(I,50).EQ.1) WRITE(PRUNIT,104)
        LDATA=LCATE+NR*(I-1)
C
        NLYR = IQ(LDATA+11)             ! number of layers
        ILYR =  0
        DO 20 KK = 1,NLYRL
          IF ( JBIT(IQ(LDATA+17),KK).NE.0 ) THEN
            ILYR = ILYR+1
            LYRS(ILYR) = KK
          ENDIF
   20   CONTINUE
        IF(ILYR.NE.NLYR)THEN
          CALL ERRMSG('CALORIMETER','PRCATE',
     &      'MIS-MATCH IN LAYER NUMBER ','W')
        ENDIF
C
        WRITE(PRUNIT,105) (IQ(LDATA+K),K=11,14),
     &    (Q(LDATA+K),K=4,10),(LYRS(KK),KK=1,NLYR)
   10 CONTINUE
C
   98 RETURN
   99 WRITE(PRUNIT,100)
      WRITE(PRUNIT,101) LCATE,NCATE
      RETURN
C
C
  100 FORMAT(//1X,57('*')/)
  101 FORMAT(/,' Wrong Address for a CATE bank: LCATE =',I8
     +,' NCATE =',I8/)
  102 FORMAT(/,
     +' ========================================================='/
     +'      CATE: Calorimeter towers energies bank                  '/
     +' ========================================================='/)
  103 FORMAT('  Bank version= ',I5,
     &  /,'  Number of words per tower= ',I5,
     &  /,'  Number of EM  towers=',I5,
     &  /,'  Number of ALL towers=',I5)
  104 FORMAT(/,'  NLy Eta Phi EmH',5X,'Ex',8X,'Ey',8X,'Ez',9X,'E',8X,
     &  'Et',4X,'sig2(Ex)',2X,'sig2(Ey)',2X,'Layers contributing ')
  105 FORMAT(4I4,7F10.3,8I3,/,10I3)
C
C
      END
