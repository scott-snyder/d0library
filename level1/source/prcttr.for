      SUBROUTINE PRCTTR(PRUNIT,LCTTRI,NCTTRI,CFL,IFL)
C***********************************************************************
C-
C-  Print out for CTTR (calorimeter energy hits) bank
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LCTTRI= bank address
C-  NCTTRI= bank number
C-  CFL   = flag to control printout
C-        = 'HED' header only
C-  IFL   = flag to control printout
C-        = 1 full printout
C-        .NE. 1 print only number of channels
C-                                                Serban D. Protopopescu
C-                                                          Aug. 19,1990
C-
C-
C***********************************************************************
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER PRUNIT,LCTTR,NCTTR,IFL,GZCTTR,LCTTRI,NCTTRI
      INTEGER K,I,LDATA,NEM,NHAD,NR,VRSION,SMEAR_STAT,NCH
      CHARACTER*48 SMEAR_INFO
      CHARACTER CFL*(*)
      CHARACTER*4 PATH
C
C--------------------------------------------------------------------
C
C--   check if bank does exist
C
      LCTTR=LCTTRI
      NCTTR=NCTTRI
      IF(LCTTR.EQ.0)THEN
        LCTTR=GZCTTR()
        IF(LCTTR.LE.0) GOTO 999
      ENDIF
C
C     Get the actual PATH
C
      CALL PATHGT(PATH)
C
C--   Print header
C
    5 WRITE(PRUNIT,102) PATH
      VRSION=IQ(LCTTR+1)
      NR=IQ(LCTTR+2)
      NEM=IQ(LCTTR+3)/1000
      NHAD=MOD(IQ(LCTTR+3),1000)
      WRITE(PRUNIT,103) VRSION,NEM,NHAD
      WRITE(PRUNIT,100)
      SMEAR_STAT=IQ(LCTTR+4)
      IF(SMEAR_STAT.EQ.0)
     &  SMEAR_INFO=' No noise added or smearing done'
      IF(SMEAR_STAT.EQ.1)
     &  SMEAR_INFO='  Noise added but no smearing'
      IF(SMEAR_STAT.EQ.2)
     &  SMEAR_INFO=' Smearing added but no noise'
      IF(SMEAR_STAT.EQ.3)
     &  SMEAR_INFO=' Noise added and resolution smearing done'
      WRITE(PRUNIT,104) SMEAR_INFO
C
      IF(CFL.EQ.'HED') GOTO 98
C
C        Print contents of the bank
C
      IF(IFL.EQ.1) THEN
        NCH=NEM+NHAD
        DO 10 I=1,NCH
          IF(MOD(I,50).EQ.1) WRITE(PRUNIT,110)
          LDATA=LCTTR+NR*(I-1)
          WRITE(PRUNIT,111) (Q(LDATA+K),K=5,8),(IQ(LDATA+K),
     &      K=9,11)
   10   CONTINUE
      ENDIF
C
   98 CONTINUE
      LCTTR = LQ(LCTTR)
      IF ( LCTTR.GT.0 ) GOTO 5
      RETURN
  999 WRITE(PRUNIT,100)
      WRITE(PRUNIT,101) LCTTR,NCTTR
      RETURN
C
C
C
  100 FORMAT(//X,57('*')/)
  101 FORMAT(/,' Wrong Address for a CTTR bank: LCTTR =',I8
     +,' NCTTR =',I8/)
  102 FORMAT(/,
     +' ========================================================='/
     +'      CTTR: Calorimeter energy hits bank     PATH: ',A     /
     +' ========================================================='/)
  103 FORMAT('  The version number of the current bank (CTTR) is ',I4,
     &  /,'  Number of e.m. channels=',I5,
     &  ',  Number of summed channels=',I5)
  104 FORMAT(A)
  110 FORMAT(/,' Energy',7X,'Et',5X,'eta',5X,'phi',5X,'eta index',
     &  2X,'phi index  em/sum(1/2)')
  111 FORMAT(1X,4F8.2,3(6X,I4))
C
C
      END
