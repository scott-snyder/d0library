      SUBROUTINE PRCAEH(PRUNIT,LCAEHI,NCAEHI,CFL,IFL)
C-----------------------------------------------------------------------
C-
C-  Print out for CAEH (calorimeter energy hits) bank
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LCAEHI= bank address 
C-  NCAEHI= bank number
C-  CFL   = flag to control printout
C-        = 'HED' header only
C-  IFL   = not used
C-
C-   Created  Dec. 21,1988  Serban Protopopescu
C-   Updated  20-APR-1992   Chip Stewart   - no abort
C-   Updated  20-MAY-1993   Stan Krzywdzinski
C-      Add printout of error matrix
C-   Updated  24-JUN-1993   Stan Krzywdzinski
C-      Fixed format mismatch
C-----------------------------------------------------------------------

      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:DCPARA.DEF/LIST'
      INTEGER PRUNIT,LCAEH,NCAEH,IFL,GZCAEH,LCAEHI,NCAEHI
      INTEGER I,POINT,LDATA,ND,NR,ETA,PHI,LYR
      INTEGER NCH,K,VRSION
      REAL ENERGY
      CHARACTER CFL*(*)
      LOGICAL GETMAT
C--------------------------------------------------------------------
C
C--   check if bank does exist
C
      LCAEH=LCAEHI
      NCAEH=NCAEHI
      IF(LCAEH.EQ.0)THEN
        LCAEH=GZCAEH()
        IF(LCAEH.LE.0) GOTO 99
      ENDIF
C
C--   Print header
C
      WRITE(PRUNIT,102) 
      IF(CFL.EQ.'HED') GOTO 98
C
      VRSION=IQ(LCAEH+1)
      NR=IQ(LCAEH+2)
      GETMAT=(VRSION.GE.3) .AND. (NR.GE.17) 
      NCH=IQ(LCAEH+3)
      WRITE(PRUNIT,103) VRSION,NCH
      WRITE(PRUNIT,100)
C
C        Print contents of the bank
C
      DO 10 I=1,NCH
        IF (GETMAT) THEN
          IF(MOD(I,50).EQ.1) WRITE(PRUNIT,204)
          LDATA=LCAEH+NR*(I-1)
          WRITE(PRUNIT,205) (IQ(LDATA+K),K=12,14),(Q(LDATA+K),K=4,8),
     &                        Q(LDATA+11),Q(LDATA+16)
          IF(MOD(I,50).EQ.1) WRITE(PRUNIT,206)
          WRITE(PRUNIT,207) (Q(LDATA+K),K=9,10),(Q(LDATA+K),K=17,20)
        ELSE
          IF(MOD(I,50).EQ.1) WRITE(PRUNIT,104)
          LDATA=LCAEH+NR*(I-1)
          WRITE(PRUNIT,105) (IQ(LDATA+K),K=12,14),(Q(LDATA+K),K=4,11)
        ENDIF
  10  CONTINUE         
C
  98  RETURN
  99  WRITE(PRUNIT,100)
      WRITE(PRUNIT,101) LCAEH,NCAEH
      RETURN
C
C
 100  FORMAT(//1X,57('*')/)
 101  FORMAT(/,' Wrong Address for a CAEH bank: LCAEH =',I8
     +,' NCAEH =',I8/)
 102  FORMAT(/,
     +' ========================================================='/
     +'      CAEH: Calorimeter energy hits bank                  '/
     +' ========================================================='/)
 103  FORMAT('  The version number of the current bank (CAEH) is ',I4,
     &  /,'  Number of channels=',I5)
 104  FORMAT(/,'  Eta Phi Lyr',5X,'Ex',8X,'Ey',8X,'Ez',9X,'E',8X,'Et',
     &  4X,'sig2(Ex)',2X,'sig2(Ey)',4X,'weight')
 105  FORMAT(3I4,8F10.3)
C
 204  FORMAT(/,'  Eta Phi Lyr',5X,'Ex',8X,'Ey',8X,'Ez',9X,'E',8X,'Et',
     & 4X,'weight',2X,'sig2(Et)')
 205  FORMAT(3I4,7F10.3)
 206  FORMAT(12X,6X,'SIG2EX',6X,'SIG2EY',6X,'SIG2EZ',
     1           6X,'DEXDEY',6X,'DEXDEZ',6X,'DEYDEZ')
 207  FORMAT(12X,6E12.3)
C
C
      END
