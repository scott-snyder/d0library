      SUBROUTINE PRMTRJ(PRUNIT,LMTRJI,NMTRJ,CFL,IFL)
C------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-  Print out for MTRJ (muon) bank
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LMTRJI= bank address
C-  NMTRJ = bank number
C-  CFL   = flag to control printout
C-          'ALL' for all banks, 'LINEAR' for one linear structure
C-          'ONE' for one bank only
C-          LMTRJI must be provided for 'LINEAR',
C-          LMTRJI or NMTRJ may be provided for 'ONE',
C-          LMTRJI and NMTRJ ignored for 'ALL'          
C-  IFL   = 0  print everything
C-          1  print partially (not yet implemented) 
C-
C-   Created  26-JUN-1990   Susumu Igarashi
C-   Modified 08-Jul-1990   S.Kunori
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZMTRJ.LINK'
      INCLUDE 'D0$LINKS:IZMUON.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C     -- variables in input arguments...
      INTEGER PRUNIT,LMTRJI,NMTRJ,IFL
      CHARACTER CFL*(*)
C     -- local variables...
      INTEGER LMTRJ,LMTRH,LMUON
      INTEGER K1,K2
      INTEGER K,BANKN,NWDS,NPOINTS,I
C
      INTEGER GZMTRJ,GZMUON,GZMTRH
      EXTERNAL GZMTRJ
      EXTERNAL GZMUON
      EXTERNAL GZMTRH
C----------------------------------------------------------------------
C
      LMTRJ=LMTRJI
      IF(CFL.EQ.'ONE') THEN
        IF(LMTRJ.EQ.0) THEN
          IF(NMTRJ.EQ.0) GOTO 98      ! error exit
          LMUON=GZMUON(NMTRJ)
          LMTRJ=GZMTRJ(NMTRJ)
        ENDIF
      ENDIF
C
      IF(CFL.EQ.'ALL') THEN
        LMUON=GZMUON(0)
        LMTRJ=GZMTRJ(0) 
      ENDIF
C
C        print title
C
      IF(LMTRJ.EQ.0) THEN 
         WRITE (PRUNIT,100)
         GO TO 999
      ELSE
         WRITE(PRUNIT,109)
      ENDIF
C
    1 IF(LMTRJ.GT.0) THEN
C
C   Print contents of bank
C
        BANKN=IQ(LMTRJ-5)
        NWDS=IQ(LMTRJ+2)
        NPOINTS=IQ(LMTRJ+3)
        K1=LMTRJ+6
        K2=LMTRJ+15
        WRITE(PRUNIT,101)
        WRITE(PRUNIT,102)BANKN,NPOINTS,(Q(K),K=K1,K2)

        IF(NPOINTS.EQ.1) GOTO 10
        DO 11 I=2,NPOINTS
          K1=LMTRJ+NWDS*(I-1)+6
          K2=LMTRJ+NWDS*(I-1)+15
          WRITE(PRUNIT,103)(Q(K),K=K1,K2)
   11   CONTINUE
   10   CONTINUE
C
        IF(CFL.NE.'ONE') THEN
          LMTRJ=LQ(LMTRJ)               ! pointer to next bank
          IF(LMTRJ.EQ.0) GOTO 2
          GOTO 1
        ENDIF
C
    2   IF(CFL.EQ.'ALL') THEN
          LMUON=LQ(LMUON)               ! check for other tracks
          IF(LMUON.GT.0) THEN
            LMTRJ=LQ(LMUON-IZMTRJ)
            GOTO 1
          ENDIF
        ENDIF
C
      ENDIF
C
      RETURN
   98 WRITE(PRUNIT,111) LMTRJ,NMTRJ
      RETURN
   99 WRITE(PRUNIT,112) LMTRJ
      RETURN
  100 FORMAT(10X/' PRMTRJ:  NO MTRJ BANK.') 
  109 FORMAT(10X/20X,'================= MTRJ BANK ==================')
  101 FORMAT(10X/'  NUM   POINTS  POINT_ID    X',8X,'Y',8X,'Z',
     1 7X,'PX/P     PY/P     PZ/P      P            S')
  102 FORMAT(1X,I3,4X,I4,2X,F8.0,2X,7F9.3,2X,2F10.1)
  103 FORMAT(14X,F8.0,2X,7F9.3,2X,2F10.1)
  111 FORMAT('0',//,'  FOR A SINGLE BANK PRINTOUT OF MTRJ YOU MUST',
     1 ' DEFINE POINTER OR BANK NUMBER',/,' THEY ARE NOW SET TO',2I10)
  112 FORMAT('0',//,' FOR PRINTOUT OF LINEAR ARRAY OF MTRJ',
     1 ' YOU MUST DEFINE POINTER',/,' IT IS NOW SET TO',I10)
  999 RETURN
      END
