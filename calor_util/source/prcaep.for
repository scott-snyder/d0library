      SUBROUTINE PRCAEP(PRUNIT,LCAEPI,NCAEPI,CFL,IFL)
C***********************************************************************
C-
C-  Print out for CAEP (calorimeter energy hits) bank
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LCAEPI= bank address
C-  NCAEPI= bank number
C-  CFL   = flag to control printout
C-        = 'HED' header only
C-  IFL   = flag to control printout
C-        = 1 full printout
C-        .NE. 1 print only number of channels
C-                                                Serban D. Protopopescu
C-                                                          Dec. 21,1988
C-
C-  Updated: 9-May-1989   Stan Krzywdzinski
C-                        Corrected printout of channels/line.
C-                        Added printout of the current PATH
C-                        to CAEP bank (GEAN or RECO).
C-  Updated  17-Mar-1992  Herbert Greenlee
C-    Fix byte order
C-   Updated   6-SEP-1994  J. Drinkard                Add SUNOS switch
C-
C***********************************************************************
 
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER PRUNIT,LCAEP,NCAEP,IFL,GZCAEP,LCAEPI,NCAEPI
      INTEGER I,J,K,KK,LDATA,NCH,NR,ETA,VRSION
      INTEGER    NCHLIN
      PARAMETER (NCHLIN=3)
      REAL ENERGY(NCHLIN)
      CHARACTER CFL*(*)
      INTEGER INDCES(NCHLIN)
      CHARACTER*4 PATH
      BYTE BYT(4,NCHLIN)
      EQUIVALENCE (BYT(1,1),INDCES)
C
C--------------------------------------------------------------------
C
C--   check if bank does exist
C
      LCAEP=LCAEPI
      NCAEP=NCAEPI
      IF(LCAEP.EQ.0)THEN
        LCAEP=GZCAEP()
        IF(LCAEP.LE.0) GOTO 999
      ENDIF
C
C     Get the actual PATH
C
      CALL PATHGT(PATH)
C
C--   Print header
C
    5 WRITE(PRUNIT,102) PATH
      VRSION=IQ(LCAEP+1)
      NR=IQ(LCAEP+2)
      NCH=IQ(LCAEP+3)
      WRITE(PRUNIT,103) VRSION,NCH
      WRITE(PRUNIT,100)
C
      IF(CFL.EQ.'HED') GOTO 98
C
C        Print contents of the bank
C
      IF(IFL.EQ.1) THEN
        K=0
        DO 10 I=1,NCH
          IF(MOD(I,50).EQ.1) WRITE(PRUNIT,104)
          LDATA=LCAEP+NR*(I-1)+3
          K=K+1
          INDCES(K)=IQ(LDATA+1)
          ENERGY(K)=Q(LDATA+2)
          IF(MOD(I,NCHLIN).EQ.0 .OR. I.EQ.NCH) THEN
            WRITE(PRUNIT,105) (BYT(BYTE4,KK),BYT(BYTE3,KK),
     &        BYT(BYTE2,KK),BYT(BYTE1,KK),ENERGY(KK),KK=1,K)
C&IF VAXVMS,ULTRIX,SIUNIX,SUNOS,ALFOSF
  105       FORMAT(<NCHLIN>(2X,3I4,2X,Z2,F8.3))
C&ENDIF
C&IF IBMAIX,LINUX
C&  105       FORMAT(3(2X,3I4,2X,Z2,F8.3))
C&ENDIF
            K=0
          ENDIF
   10   CONTINUE
      ENDIF
C
   98 CONTINUE
      LCAEP = LQ(LCAEP)
      IF ( LCAEP.GT.0 ) GOTO 5
      RETURN
  999 WRITE(PRUNIT,100)
      WRITE(PRUNIT,101) LCAEP,NCAEP
      RETURN
C
C
C
  100 FORMAT(//1X,57('*')/)
  101 FORMAT(/,' Wrong Address for a CAEP bank: LCAEP =',I8
     +,' NCAEP =',I8/)
  102 FORMAT(/,
     +' ========================================================='/
     +'      CAEP: Calorimeter energy hits bank     PATH: ',A     /
     +' ========================================================='/)
  103 FORMAT('  The version number of the current bank (CAEP) is ',I4,
     &  /,'  Number of channels=',I5)
C&IF VAXVMS,ULTRIX,SIUNIX,SUNOS,ALFOSF
  104 FORMAT(/,1X,<NCHLIN>('   ETA PHI LYR BITS ENERGY'))
C&ENDIF
C&IF IBMAIX,LINUX
C&  104 FORMAT(/,1X,3('   ETA PHI LYR BITS ENERGY'))
C&ENDIF
C
      END
