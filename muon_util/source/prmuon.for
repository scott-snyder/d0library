      SUBROUTINE PRMUON(PRUNIT,LMUONI,NMUON,CFL,IFL)
C------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-  Print out for MUON (muon) bank
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LMUONI= bank address
C-  NMUON = bank number
C-  CFL   = flag to control printout
C-          'ALL' for all banks, 'LINEAR' for one linear structure
C-          'ONE' for one bank only
C-          LMUONI must be provided for 'LINEAR',
C-          LMUONI or NMUON may be provided for 'ONE',
C-          LMUONI and NMUON ignored for 'ALL'          
C-  IFL   = 0  print everything
C-          1  print partially (not yet implemented) 
C-
C-   Created  25-JUN-1990   Susumu Igarashi
C-   Modified  9-JUL-1990   Susumu Igarashi
C-   Modified 07-MAR-1991   S. Abachi
C    MARCH 12,1991   D. HEDIN
C-   Modified 08-APR-1991   S. Abachi
C-   Modified 10-JAN-1992   S. Abachi
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZMUON.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C     -- variables in input arguments...
      INTEGER PRUNIT,LMUONI,NMUON,IFL
      CHARACTER CFL*(*)
C     -- local variables...
      INTEGER LMUON,LMTRH
      INTEGER K1,K2,K3,K4,I
      INTEGER NS,L,NMUOT,NZTRK,NVRSN,NTRAK,NPID,K
      REAL DPP
C
      INTEGER GZMUON,GZMTRH
      EXTERNAL GZMUON
      EXTERNAL GZMTRH
C----------------------------------------------------------------------
C
      LMUON=LMUONI
      IF(CFL.EQ.'ONE') THEN
        IF(LMUON.EQ.0) THEN
          IF(NMUON.EQ.0) GOTO 98      ! error exit
          LMUON=GZMUON(NMUON)
        ENDIF
      ENDIF
C
      IF(CFL.EQ.'ALL') THEN
        LMTRH=GZMTRH()
        IF(LMTRH.NE.0) THEN
           LMUON=LQ(LMTRH-IZMUON)
        ELSE
           LMUON=0
        ENDIF
      ENDIF
C
C        print title
C
      IF(LMUON.EQ.0) THEN 
         WRITE (PRUNIT,200)
         GO TO 999
      ELSE
         WRITE(PRUNIT,201)
      ENDIF
C
    1 IF(LMUON.GT.0) THEN
C
C   Print contents of bank
C
C       -- MUOT bank number...
        L=LQ(LMUON-NS-1)
        IF(L.NE.0) THEN
           NMUOT=IQ(L-5)
        ELSE
           NMUOT=0
        ENDIF
C+++++++++++++++++++++++++++
      WRITE(PRUNIT,101)
      WRITE(PRUNIT,102) (IQ(LMUON+I),I=1,10)
C
      WRITE(PRUNIT,103)
      WRITE(PRUNIT,104) (Q(LMUON+I),I=11,18)
C
      WRITE(PRUNIT,105)
      WRITE(PRUNIT,106) (Q(LMUON+I),I=19,25)
C
      WRITE(PRUNIT,107)
      WRITE(PRUNIT,108) (Q(LMUON+I),I=26,31)
C
      WRITE(PRUNIT,109)
      WRITE(PRUNIT,110) (Q(LMUON+I),I=32,36)
C
      WRITE(PRUNIT,111)
      WRITE(PRUNIT,112) (Q(LMUON+I),I=37,46)
C
      WRITE(PRUNIT,113)
      WRITE(PRUNIT,114) (Q(LMUON+I),I=47,55)
C
  101 FORMAT(/
     & '   NVRSN  MUID  F_DEDX   F_MOM NCDTRK  F_QLTY   F_KINK  F_VECT 
     & QUAD  FIT_METHOD ')
  102 FORMAT(2I6,8I8)
C
  103 FORMAT(/'     PX        PY       PZ        P        PT     THETA  
     &    ETA     PHI') 
  104 FORMAT(1X,8F9.3)
C
  105 FORMAT(/'  RADL_CA   RADL_MU   MULTSC_CA  MULTSC_MU CONE_SIZE
     & ELOSS_CAex  ELOSS_MU')
  106 FORMAT(F10.2,F10.2,2F11.3,3F9.1)
C
  107 FORMAT(/'       SIG**2PX      SIG**2_PY     SIG**2_PZ
     &      SIG**2P      SIG**2PT       CHISQ')
  108 FORMAT(1X,6F14.3)
C
  109 FORMAT(/'      ISOL1     ISOL2     ISOL3     ISOL4     ISOL5')
  110 FORMAT(1X,5F10.3)
C
  111 FORMAT(/
     &'   X_VRT    Y_VRT     Z_VRT ANGLE_MUCD   D_PHI  D_THETA
     $  ECONE0.2  ECONE0.4  ECONE0.6  TOF')
  112 FORMAT(1X,10F9.3)
C
  113 FORMAT(/
     &'    X_MID    Y_MID    Z_MID      X_DIRCM    Y_DIRCM   Z_DIRCM
     $  IMPCT_PAR  IMPACT_PAR2  B_MEAN')
  114 FORMAT(1X,4F9.3,5F11.3)
C
C+++++++++++++++++++++++++++
C
        IF(CFL.NE.'ONE') THEN
          LMUON=LQ(LMUON)               ! pointer to next bank
          GOTO 1
        ENDIF
C
        IF(CFL.EQ.'ALL') THEN
          LMTRH=LQ(LMTRH)               ! check for additional headers
          IF(LMTRH.GT.0) THEN
            LMUON=LQ(LMTRH-IZMUON)
            GOTO 1
          ENDIF
        ENDIF
C
      ENDIF
C
      RETURN
   98 WRITE(PRUNIT,211) LMUON,NMUON
      RETURN
   99 WRITE(PRUNIT,212) LMUON
      RETURN
  200 FORMAT(10X/' PRMUON:  NO MUON BANK.') 
  201 FORMAT(10X/20X,'================= MUON BANK ==================')
C
  211 FORMAT('0',//,'  FOR A SINGLE BANK PRINTOUT OF MUON YOU MUST',
     1 ' DEFINE POINTER OR BANK NUMBER',/,' THEY ARE NOW SET TO',2I10)
  212 FORMAT('0',//,' FOR PRINTOUT OF LINEAR ARRAY OF MUON',
     1 ' YOU MUST DEFINE POINTER',/,' IT IS NOW SET TO',I10)
  999 RETURN
      END
