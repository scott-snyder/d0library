      SUBROUTINE PRHITS(PRUNIT,LHITSI,NHITSI,CFL,IFL)
C***********************************************************************
C-
C-  Print out for HITS (event reconstruction) bank
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LHITSI= bank address 
C-  NHITSI= bank number
C-  CFL   = flag to control printout
C-        = 'HED' header only
C-  IFL   = flag to control printout (not used)
C-                                                 Marcel Demarteau
C-                                                 Serban Protopopescu 
C-                                                        Sept. 24 1987 
C-  Modified 19-APR-1991 Tom Trippe  -  Fix Naming of CD Banks
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER PRUNIT,LHITS,NHITS,IFL,LHITSI,NHITSI,GZHITS
      INTEGER LPREV
      INTEGER I,K,NFULL,NEMPTY
      CHARACTER CFL*(*)
      CHARACTER*4 BLIST(9),FULL(9),EMPTY(9)

      DATA BLIST/'LVHT','MUHT','VTXH','CDCH','FDCH','TRDH',
     +           'CAHT','BLNK','BLNK'/ 
C--------------------------------------------------------------------
C
C--   check if bank does exist
C
      LPREV=0
      LHITS=LHITSI
      NHITS=NHITSI
C
      I=NHITS
      IF(NHITS.EQ.0) I=1
  10  IF(LHITS.EQ.0)THEN
         LHITS=GZHITS(I)
         IF(LHITS.EQ.LPREV) GOTO 98
         IF(LHITS.LE.0) GOTO 99
      ENDIF
C
C--   Print header
C
      WRITE(PRUNIT,102) 

      IF(CFL.EQ.'HED') GOTO 98
C
C     Give list of empty banks and banks with data
C
      NEMPTY=0
      NFULL=0
      DO 1 K=1,9
         IF(BLIST(K).NE.'BLNK') THEN
            IF(LQ(LHITS-K).EQ.0) THEN
               NEMPTY=NEMPTY+1
               EMPTY(NEMPTY)=BLIST(K)
            ELSE
               NFULL=NFULL+1
               FULL(NFULL)=BLIST(K)
            ENDIF
         ENDIF
  1   CONTINUE  
      
      IF(NEMPTY.GT.0) WRITE(PRUNIT,103) (EMPTY(K),K=1,NEMPTY)
      IF(NFULL.GT.0)  WRITE(PRUNIT,104) (FULL(K),K=1,NFULL)
C
      IF(NHITS.EQ.0) THEN
         IF(I.EQ.1) THEN
            I=2
            LPREV=LHITS
            LHITS=0
            GOTO 10
         ENDIF
      ENDIF
C
C
  98  RETURN
  99  WRITE(PRUNIT,100)
      WRITE(PRUNIT,101) LHITS,NHITS
      RETURN
C
 100  FORMAT(//2X,50('*')/)
 101  FORMAT(/,' Wrong Address for a HITS bank: LHITS =',I8
     +,' NHITS =',I8/)
 102  FORMAT(/,
     +' ========================================================='/
     +'      HITS: General hits bank                             '/
     +' ========================================================='/)
 103  FORMAT('  Bank ',A4,' is EMPTY')
 104  FORMAT('  Bank ',A4,' has DATA')
C
      END
