      SUBROUTINE PRCAHT(PRUNIT,LCAHTI,NCAHTI,CFL,IFL)
C***********************************************************************
C-
C-  Print out for CAHT (calorimeter hits) bank
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LCAHTI= bank address 
C-  NCAHTI= bank number
C-  CFL   = flag to control printout
C-        = 'HED' header only
C-  IFL   = flag to control printout (not used)
C-                                                 Serban Protopopescu 
C-                                                        Jan. 13, 1987 
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER PRUNIT,LCAHT,NCAHT,IFL,LCAHTI,NCAHTI,GZCAHT
      INTEGER K,NFULL,NEMPTY
      CHARACTER CFL*(*)
      INTEGER NBANKS
      PARAMETER (NBANKS=3)
      CHARACTER*4 BLIST(NBANKS),EMPTY(NBANKS),FULL(NBANKS)
      DATA BLIST/'CAEP','CAEH','CATE'/ 
C--------------------------------------------------------------------
C
C--   check if bank does exist
C
      LCAHT=LCAHTI
      IF(LCAHT.EQ.0)THEN
        LCAHT=GZCAHT()
        IF(LCAHT.LE.0) GOTO 99
      ENDIF
C
C--   Print header
C
      WRITE(PRUNIT,102) 
C
      IF(CFL.EQ.'HED') GOTO 98
C
C     Give list of empty banks and banks with data
C
      NEMPTY=0
      NFULL=0
      DO 1 K=1,NBANKS
        IF(LQ(LCAHT-K).EQ.0) THEN
          NEMPTY=NEMPTY+1
          EMPTY(NEMPTY)=BLIST(K)
        ELSE
          NFULL=NFULL+1
          FULL(NFULL)=BLIST(K)
        ENDIF
  1   CONTINUE  
C      
      IF(NEMPTY.GT.0) WRITE(PRUNIT,103) (EMPTY(K),K=1,NEMPTY)
      IF(NFULL.GT.0)  WRITE(PRUNIT,104) (FULL(K),K=1,NFULL)
C
      WRITE(PRUNIT,105) IQ(LCAHT+1)
C
C
  98  RETURN
  99  WRITE(PRUNIT,100)
      WRITE(PRUNIT,101) LCAHT,NCAHT
      RETURN
C
C
 100  FORMAT(//1X,57('*')/)
 101  FORMAT(/,' Wrong Address for a CAHT bank: LCAHT =',I8
     +,' NCAHT =',I8/)
 102  FORMAT(/,
     +' ========================================================='/
     +'      CAHT: Calorimeter hits bank                         '/
     +' ========================================================='/)
 103  FORMAT('  Bank ',A4,' is EMPTY')
 104  FORMAT('  Bank ',A4,' has DATA')
 105  FORMAT('  The version number of the current bank (CAHT) is ',I4)
C
      END
