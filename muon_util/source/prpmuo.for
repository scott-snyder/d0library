      SUBROUTINE PRPMUO(PRUNIT,LPMUOI,NPMUO,CFL,IFL)
C------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-  Print out for PMUO (muon) bank
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LPMUOI= bank address
C-  NPMUO = bank number
C-  CFL   = flag to control printout
C-          'ALL' for all banks, 'LINEAR' for one linear structure
C-          'ONE' for one bank only
C-          LPMUOI must be provided for 'LINEAR',
C-          LPMUOI or NPMUO may be provided for 'ONE',
C-          LPMUOI and NPMUO ignored for 'ALL'
C-  IFL   = 0  print everything
C-          1  print partially (not yet implemented)
C-
C-   Created  11-JUN-1990   Shuichi Kunori
C-   Modified 09-JUL-1990   Susumu Igarashi
C-   Modified 07-MAR-1991   S. Abachi
C    D. HEDIN 3-12-91
C-   Modified 08-APR-1991   S. Abachi
C-   Updated  13-JUN-1991   Daria Zieminska
C-   Updated  14-JAN-1992   S. ABACHI
C    Hedin 12/94 fix format for scint words
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZPMUO.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C     -- variables in input arguments...
      INTEGER PRUNIT,LPMUOI,NPMUO,IFL
      CHARACTER CFL*(*)
C     -- local variables...
      INTEGER LPMUO,LPARH
      INTEGER I
      INTEGER NS,L,NMUOT,NMUON,NVERT
C
      INTEGER GZPMUO,GZPARH
      EXTERNAL GZPMUO
      EXTERNAL GZPARH
C----------------------------------------------------------------------
C
      LPMUO=LPMUOI
      IF(CFL.EQ.'ONE') THEN
        IF(LPMUO.EQ.0) THEN
          IF(NPMUO.EQ.0) GOTO 98      ! error exit
          LPMUO=GZPMUO(NPMUO)
        ENDIF
      ENDIF
C
      IF(CFL.EQ.'ALL') THEN
        LPARH=GZPARH()
        IF(LPARH.NE.0) THEN
          LPMUO=LQ(LPARH-IZPMUO)
        ELSE
          LPMUO=0
        ENDIF
      ENDIF
C
C        print title
C
      IF(LPMUO.EQ.0) THEN
        WRITE (PRUNIT,200)
        GO TO 999
      ELSE
        WRITE(PRUNIT,201)
      ENDIF
C
    1 IF(LPMUO.GT.0) THEN
C
C   Print contents of bank
C
C       -- VERT bank number...
        NS=IQ(LPMUO-2)
        L=LQ(LPMUO-NS-3)
        IF(L.NE.0) THEN
          NVERT=IQ(L-5)
        ELSE
          NVERT=0
        ENDIF
C
C       -- MUON bank number...
        L=LQ(LPMUO-NS-2)
        IF(L.NE.0) THEN
          NMUON=IQ(L-5)
        ELSE
          NMUON=0
        ENDIF
C
C       -- MUOT bank number...
        L=LQ(LPMUO-NS-1)
        IF(L.NE.0) THEN
          NMUOT=IQ(L-5)
        ELSE
          NMUOT=0
        ENDIF
C
        WRITE(PRUNIT,89)
        WRITE(PRUNIT,100) IQ(LPMUO-5),NMUOT,NMUON,NVERT
C
        WRITE(PRUNIT,101)
        WRITE(PRUNIT,102) (IQ(LPMUO+I), I=1,9)
C
        WRITE(PRUNIT,103)
        WRITE(PRUNIT,104) (Q(LPMUO+I), I=10,17)
C
        WRITE(PRUNIT,105)
        WRITE(PRUNIT,106) (Q(LPMUO+I), I=18,27)
C
        WRITE(PRUNIT,107)
        WRITE(PRUNIT,108) (Q(LPMUO+I), I=28,32)
C
        WRITE(PRUNIT,109)
        WRITE(PRUNIT,110) (Q(LPMUO+I), I=33,39)
C
        WRITE(PRUNIT,111)
        WRITE(PRUNIT,112) (Q(LPMUO+I), I=40,43), IQ(LPMUO+44)
C
        WRITE(PRUNIT,113)
        WRITE(PRUNIT,114) (IQ(LPMUO+I),I=45,51),Q(LPMUO+52),
     A Q(LPMUO+53)
  113   FORMAT(/
     &    '     USERFLAG    HITS(TRK)  HITS(FIT)',18X,
     & 'TRIGGER',16X,'SCINT TOF meas+exp')
  114   FORMAT(1X,3I10,2X,4I12,3X,2F6.0)

        WRITE(PRUNIT,115)
        WRITE(PRUNIT,116) IQ(LPMUO+54),IQ(LPMUO+55),
     A (Q(LPMUO+I),I=56,59)
  115   FORMAT(/' VTX No. a-b       MUON-IMPACT       CENT-IMPACT')
  116   FORMAT(1X,2I4,6X,4F9.3)
C
        WRITE(PRUNIT,117)
        WRITE(PRUNIT,118) (Q(LPMUO+I),I=60,65)
        WRITE(PRUNIT,119) (Q(LPMUO+I),I=66,71)
        WRITE(PRUNIT,120) (Q(LPMUO+I),I=72,77)

  117   FORMAT(/6X,'X',8X,'Y',8X,'Z',8X,'DIR-X   DIR-Y   DIR-Z')
  118   FORMAT(1X,3F9.2,2X,3F8.3,5X,'(CENT)')
  119   FORMAT(1X,3F9.2,2X,3F8.3,5X,'(CAL )')
  120   FORMAT(1X,3F9.2,2X,3F8.3,5X,'(MUON)')

        WRITE(PRUNIT,121)
        WRITE(PRUNIT,122) (Q(LPMUO+I),I=78,82)
        WRITE(PRUNIT,123) (Q(LPMUO+I),I=83,85)
        WRITE(PRUNIT,124) (Q(LPMUO+I),I=86,89)

  121   FORMAT(/6X,'ENERGY IN CALORIMETER')
  122   FORMAT(1X,5F9.2,10X,'(EM ALONG MUON)')
  123   FORMAT(1X,3F9.2,10X,'(ETOTAL HIT CELLS & HIT CELLS + 1) SPARE')
  124   FORMAT(1X,4F9.2,10X,'(TOTAL OPPOSIT TO MUON)')

        IF(CFL.NE.'ONE') THEN
          LPMUO=LQ(LPMUO)               ! pointer to next bank
          IF (LPMUO.GT.0) GOTO 1
        ENDIF
C
        IF(CFL.EQ.'ALL') THEN
          LPARH=LQ(LPARH)               ! check for additional headers
          IF(LPARH.GT.0) THEN
            LPMUO=LQ(LPARH-IZPMUO)
            IF (LPMUO.GT.0) GOTO 1
          ENDIF
        ENDIF
C
      ENDIF
C
      RETURN
   98 WRITE(PRUNIT,211) LPMUO,NPMUO
      RETURN
   99 WRITE(PRUNIT,212) LPMUO
      RETURN
  211 FORMAT('0',//,'  FOR A SINGLE BANK PRINTOUT OF PMUO YOU MUST',
     1  ' DEFINE POINTER OR BANK NUMBER',/,' THEY ARE NOW SET TO',2I10)
  212 FORMAT('0',//,' FOR PRINTOUT OF LINEAR ARRAY OF PMUO',
     1  ' YOU MUST DEFINE POINTER',/,' IT IS NOW SET TO',I10)
C
  200 FORMAT(10X/' PRMUON:  NO PMUO BANK.')
  201 FORMAT(10X/20X,'================= PMUO BANK ==================')
C
   89 FORMAT(//'   NPMUO NMUOT NMUON NVERT')
  100 FORMAT(1X,4I6)
C
  101 FORMAT(/
     &' VSN  MUID  F_DEDX  METHD  FLG_VTX  NCDTRK  QUAD F_MTHD  QULTY')
  102 FORMAT(2X,I2,3X,I4,2X,I3,3X,I4,6X,I2,I7,I8,I6,I7)
C
  103 FORMAT(2X,
     a '     PX      PY     PZ      P       PT     THETA    ETA  ',
     &    '   PHI')
  104 FORMAT(2X,8F8.3)
C
  105 FORMAT(/
     &    '     SIGPX2  SIGPY2  SIGPZ2  SIGP2  SIGPT2  CHISQ ',
     &    '     TOF     X_VTX     Y_VTX     Z_VTX')
  106 FORMAT(1X,6F8.2,F10.2,1X,F10.4,1X,F10.4,1X,F10.4)
C
  107 FORMAT(/10X,
     &    '    ISOL1    ISOL2    ISOL3    ISOL4    ISOL5')
  108 FORMAT(10X,5F9.3)
C
  109 FORMAT(2X,
     &    '        ELOSS(exptd)  ECONE0.2     ECONE0.4     ECONE0.6',
     & '   ANGLE_MUCD       D_PHI      D_THETA')
  110 FORMAT(2X,7F14.3)
C
  111 FORMAT(2X,
     &    '      CONE-SIZE    IMPACT1    IMPACT2   ELOSS_IRON    ',
     &    '   F_KINK ')
  112 FORMAT(2X,4F12.3,I10)
C
C
  999 RETURN
      END
