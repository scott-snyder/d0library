      SUBROUTINE PRPNUT(PRUNIT,LPNUTI,NPNUT,CFL,IFL)
C------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-  Print out for PNUT (parton) bank
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LPNUTI= bank address
C-  NPNUT = bank number
C-  CFL   = flag to control printout
C-          'ALL' for all banks, 'LINEAR' for one linear structure
C-          'ONE' for one bank only
C-          LPNUTI must be provided for 'LINEAR',
C-          LPNUTI or NPNUT may be provided for 'ONE',
C-          LPNUTI and NPNUT ignored for 'ALL'          
C-  IFL   = 0  print everything
C-          1  print only 4-momenta and Et
C-
C-   Created  18-JAN-1989   Serban D. Protopopescu
C-   Updated  10-JAN-1990   Harrison B. Prosper  
C-      Add printout of ET sum 
C-   Updated  18-MAY-1993   Stan Krzywdzinski
C-      Add printout of error matrix
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZPNUT.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER PRUNIT,LPNUTI,NPNUT,IFL
      CHARACTER CFL*(*)
      INTEGER GZPNUT,GZPARH,LPNUT,LPARH
      INTEGER NPASS,NVRSN,K1,K2,K3,K4,K
      LOGICAL GETMAT
C----------------------------------------------------------------------
C
      LPNUT=LPNUTI
      IF(CFL.EQ.'ONE') THEN
        IF(LPNUT.EQ.0) THEN
          IF(NPNUT.EQ.0) GOTO 98      ! error exit
          LPNUT=GZPNUT(NPNUT)
        ENDIF
      ENDIF
C
      IF(CFL.EQ.'ALL') THEN
        LPARH=GZPARH()
        IF(LPARH.EQ.0) RETURN
        LPNUT=LQ(LPARH-IZPNUT)
      ENDIF
C
      NVRSN=IQ(LPNUT+1)
      GETMAT=NVRSN.GE.3
C
C        print title
C
      IF (GETMAT) THEN
        IF(IFL.EQ.0) WRITE (PRUNIT,200)    
        IF(IFL.NE.0) WRITE (PRUNIT,101)
      ELSE
        IF(IFL.EQ.0) WRITE (PRUNIT,100)    
        IF(IFL.NE.0) WRITE (PRUNIT,101)
      ENDIF
C
    1 IF(LPNUT.GT.0) THEN
C
C   Print contents of bank
C
        NPASS=IQ(LPNUT-5)
        K1=LPNUT+3
        IF (GETMAT) THEN
          K2=LPNUT+10
          IF(IFL.NE.0) K2=LPNUT+7
          K3=LPNUT+13
          K4=LPNUT+14
          IF(IFL.NE.0) K4=K3
          WRITE(PRUNIT,202) NVRSN,NPASS,(Q(K),K=K1,K2),(Q(K),K=K3,K4)
          IF(IFL.EQ.0) THEN
            K1=LPNUT+11
            K2=LPNUT+12
            K3=LPNUT+15
            K4=LPNUT+18
            WRITE(PRUNIT,201)
            WRITE(PRUNIT,203) (Q(K),K=K1,K2),(Q(K),K=K3,K4)
          ENDIF
        ELSE
          K2=LPNUT+13
          IF(IFL.NE.0) K2=LPNUT+7
          K3=LPNUT+14
          WRITE(PRUNIT,102) NVRSN,NPASS,(Q(K),K=K1,K2),Q(K3)
        ENDIF
C
        IF(CFL.NE.'ONE') THEN
          LPNUT=LQ(LPNUT)               ! pointer to next bank
          GOTO 1
        ENDIF
C
        IF(CFL.EQ.'ALL') THEN
          LPARH=LQ(LPARH)               ! check for additional headers
          IF(LPARH.GT.0) THEN
            LPNUT=LQ(LPARH-IZPNUT)
            GOTO 1
          ENDIF
        ENDIF
C
      ENDIF
C
      RETURN
   98 PRINT 111,LPNUT,NPNUT
      RETURN
   99 PRINT 112,LPNUT
      RETURN
  100 FORMAT(///,' NEUTRINO BANKS (PNUT)',/,
     1 ' VERSION  PASS',6X,'EX',8X,'EY',8X,'EZ',8X,'E ',
     2 8X,'Et',7X,'PHI',5X,'THETA',7X,'ETA',
     2 6X,'SIG2EX',6X,'SIG2EY',5X,'SIGET',5X,'Et-Sum')
  101 FORMAT(///,' NEUTRINO BANKS (PNUT)',/,
     1 ' VERSION  PASS',6X,'EX',8X,'EY',8X,'EZ',8X,'E ',
     2 8X,'Et',5X,'Et-Sum')
  102 FORMAT(I4,4X,I4,2X,5F10.3,3F10.4,2E12.3,2F10.3)
C
  200 FORMAT(///,' NEUTRINO BANKS (PNUT)',/,
     1 ' VERSION  PASS',6X,'EX',8X,'EY',8X,'EZ',8X,'E ',
     2 8X,'Et',7X,'PHI',5X,'THETA',7X,'ETA',
     3 5X,'SIGET',5X,'Et-Sum')
  201 FORMAT(14X,6X,'SIG2EX',6X,'SIG2EY',6X,'SIG2EZ',
     1           6X,'DEXDEY',6X,'DEXDEZ',6X,'DEYDEZ')
  202 FORMAT(I4,4X,I4,2X,5F10.3,3F10.4,2F10.3)
  203 FORMAT(14X,6E12.3)
  111 FORMAT('0',//,'  FOR A SINGLE BANK PRINTOUT OF PNUT YOU MUST',
     1 ' DEFINE POINTER OR BANK NUMBER',/,' THEY ARE NOW SET TO',2I10)
  112 FORMAT('0',//,' FOR PRINTOUT OF LINEAR ARRAY OF PNUT',
     1 ' YOU MUST DEFINE POINTER',/,' IT IS NOW SET TO',I10)
  999 RETURN
      END
