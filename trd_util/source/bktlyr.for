      SUBROUTINE BKTLYR(LTLYR,ILAYT)
C======================================================================
C-
C-   Purpose and Methods :  Books the TRD banks down to LAYER for
C-                           TRDH (under HITS)
C-
C-   OUTPUT:      LMTLYR         [<-----LTRDH]
C-
C-
C-   Created   6-NOV-1987   A. ZYLBERSTEJN
C-   Updated  11-MAY-1992   Alain PLUQUET  Add MZFORM
C-   Updated   4-JUN-1992   A. Zylberstejn Add IF FIRST  
C-   Updated   5-JUN-1992   ALAIN PLUQUET  Correct calls to MZFORM   
C======================================================================
      IMPLICIT NONE
      INCLUDE 'D0$INC:FADCCN.INC/LIST'
      INCLUDE 'D0$INC:TCNTRL.INC'
C      INCLUDE 'D0$INC:TRDLNK.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER ILAYT,LL,LTLYR,LTRDH,GZTRDH,NDAT,NWORD,NBANK,IOTLYR
      INTEGER IFOIS,IXIO
      INTEGER NZBANK
      LOGICAL FIRST
      CHARACTER*4 LNKNAM
      DATA IFOIS/0/
      DATA FIRST/.TRUE./
C======================================================================
      IF(ILAYT.LE.0 .OR. ILAYT.GT.6)THEN
        CALL INTMSG(' Wrong layer number for BKTLYR')
        CALL EXIT
      END IF
C  book TRDH if needed
      LTRDH=GZTRDH()
      IF(LTRDH.LE.0) THEN
        CALL BKTRDH (LTRDH)!book the ZEBRA structure down to TRDH
      END IF
      IF(LTRDH.LE.0)GO TO 999
C ****  Book MTLYR(ILAYT)
      LTLYR = LQ (LTRDH-ILAYT )
C ****  Book GEAN or RECO if needed
      IF(COSMIC1)THEN
        IF(FIRST)THEN
          CALL MZFORM ('TLYR','-F',IOTLYR)
          FIRST=.FALSE.
        END IF
        CALL MZBOOK (IXMAIN,LTLYR,LTRDH,-ILAYT,
     &    'TLYR',0,0,50,IOTLYR,0)
      ELSE
        NWORD=IQ(LTRDH+3)
        NDAT=1+NFADC/NWORD+2
        IF(FIRST)THEN
          CALL MZFORM ('TLYR','-I',IOTLYR)
          FIRST=.FALSE.
        END IF
        CALL MZBOOK (IXMAIN,LTLYR,LTRDH,-ILAYT,
     &    'TLYR',0,0,NDAT,IOTLYR,0)
      END IF
C      IF(LTLYR.NE.0)GO TO 999
  999 CONTINUE
      RETURN
      END
