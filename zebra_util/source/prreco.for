      SUBROUTINE PRRECO(PRUNIT,LRECOI,NRECOI,CFL,IFL)
C***********************************************************************
C-
C-  Print out for RECO (event reconstruction) bank
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LRECOI= bank address 
C-  NRECOI= bank number
C-  CFL   = flag to control printout
C-        = 'HED' header only
C-  IFL   = flag to control printout (not used)
C-                                                 Marcel Demarteau
C-                                                 Serban Protopopescu 
C-                                                        Sept. 24 1987 
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZRECO.LINK'
      INTEGER PRUNIT,LRECO,NRECO,IFL,LRECOI,NRECOI
      INTEGER K,NFULL,NEMPTY
      CHARACTER CFL*(*)
      CHARACTER*4 BLIST(3),FULL(3),EMPTY(3)

      DATA BLIST/'HITS','PROC','BLNK'/ 
C--------------------------------------------------------------------
C
C--   check if bank does exist
C
      LRECO=LRECOI
      NRECO=NRECOI
      IF(LRECO.EQ.0)THEN
        LRECO=LQ(LHEAD-IZRECO)
        IF(LRECO.EQ.0) GOTO 99
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
      DO 1 K=1,3
         IF(BLIST(K).NE.'BLNK') THEN
            IF(LQ(LRECO-K).EQ.0) THEN
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

      WRITE(PRUNIT,105) IQ(LRECO+1)
C
C
  98  RETURN
  99  WRITE(PRUNIT,100)
      WRITE(PRUNIT,101) LRECO,NRECO
      RETURN
C
 100  FORMAT(//2X,50('*')/)
 101  FORMAT(/,' Wrong Address for a RECO bank: LRECO =',I8
     +,' NRECO =',I8/)
 102  FORMAT(/,
     +' ========================================================='/
     +'      RECO: Event reconstruction bank                     '/
     +' ========================================================='/)
 103  FORMAT('  Bank ',A4,' is EMPTY')
 104  FORMAT('  Bank ',A4,' has DATA')
 105  FORMAT('  The version number of the current bank (RECO) is ',I4)
C
      END
