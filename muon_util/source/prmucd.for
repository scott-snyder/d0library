      SUBROUTINE PRMUCD(PRUNIT,LMUCDI,NMUCD,CFL,IFL)
C------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-  Print out for MUCD (muon) bank
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LMUCDI= bank address
C-  NMUCD = bank number
C-  CFL   = flag to control printout
C-          'ALL' for all banks, 'LINEAR' for one linear structure
C-          'ONE' for one bank only
C-          LMUCDI must be provided for 'LINEAR',
C-          LMUCDI or NMUCD may be provided for 'ONE',
C-          LMUCDI and NMUCD ignored for 'ALL'          
C-  IFL   = 0  print everything
C-          1  print partially (not yet implemented) 
C-
C-   Created  26-JUN-1990   Susumu Igarashi
C-   Modified 08-Jul-1990   S. Kunori
C-                           fixed LMUON=0 problem and removed a code
C-                           for next MTRH.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZMUCD.LINK'
      INCLUDE 'D0$LINKS:IZMUON.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C     -- variables in input arguments...
      INTEGER PRUNIT,LMUCDI,NMUCD,IFL
      CHARACTER CFL*(*)
C     -- local variables...
      INTEGER LMUCD,LMTRH,LMUON,LZTRK
C
      INTEGER GZMUCD,GZMUON,GZMTRH
      EXTERNAL GZMUCD
      EXTERNAL GZMUON
      EXTERNAL GZMTRH
C----------------------------------------------------------------------
C
      LMUCD=LMUCDI
      IF(CFL.EQ.'ONE') THEN
        IF(LMUCD.EQ.0) THEN
          IF(NMUCD.EQ.0) GOTO 98      ! error exit
          LMUON=GZMUON(NMUCD)
          LMUCD=GZMUCD(NMUCD)
        ENDIF
      ENDIF
C
      IF(CFL.EQ.'ALL') THEN
        LMUON=GZMUON(0)
        LMUCD=GZMUCD(0)
      ENDIF
C
C        print title
C
      IF(LMUCD.EQ.0) THEN 
         WRITE (PRUNIT,100)
         GO TO 999
      ELSE
         WRITE(PRUNIT,109)
         WRITE(PRUNIT,101)
      ENDIF
C
    1 IF(LMUCD.GT.0) THEN
C
C   Print contents of bank
C
        LZTRK=LQ(LMUCD-1)
        WRITE(PRUNIT,102)IQ(LMUCD-5),IQ(LMUON-5),IQ(LZTRK-5)
C
        IF(CFL.NE.'ONE') THEN
          LMUCD=LQ(LMUCD)               ! pointer to next bank
          IF(LMUCD.EQ.0) GOTO 2
          GOTO 1
        ENDIF
C
    2   IF(CFL.EQ.'ALL') THEN
          LMUON=LQ(LMUON)               ! check for other tracks
          IF(LMUON.GT.0) THEN
            LMUCD=LQ(LMUON-IZMUCD)
            GOTO 1
          ENDIF
        ENDIF
C
      ENDIF
C
      RETURN
   98 WRITE(PRUNIT,111) LMUCD,NMUCD
      RETURN
   99 WRITE(PRUNIT,112) LMUCD
      RETURN
  100 FORMAT(10X/' PRMUCD:  NO MUCD BANK.') 
  109 FORMAT(10X/20X,'================= MUCD BANK ==================')
  101 FORMAT(10X/' BANK_NUM  MUON_NUM  ZTRK_NUM')
  102 FORMAT(1X,I4,6X,I4,6X,I4)
  111 FORMAT('0',//,'  FOR A SINGLE BANK PRINTOUT OF MUCD YOU MUST',
     1 ' DEFINE POINTER OR BANK NUMBER',/,' THEY ARE NOW SET TO',2I10)
  112 FORMAT('0',//,' FOR PRINTOUT OF LINEAR ARRAY OF MUCD',
     1 ' YOU MUST DEFINE POINTER',/,' IT IS NOW SET TO',I10)
  999 RETURN
      END
