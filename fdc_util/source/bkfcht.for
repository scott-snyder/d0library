C VAX/DEC CMS REPLACEMENT HISTORY, Element BKFCHT.FOR
C *1     9-NOV-1993 17:59:50 AVERY "fdc changes for v12 reco"
C VAX/DEC CMS REPLACEMENT HISTORY, Element BKFCHT.FOR
      SUBROUTINE BKFCHT(LFCHT,NHITS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : book bank FCHT, which is the down link bank
C-                         from FDCH. FHIT bank contains compressed
C-                         information for all reconstructed FDC hits
C-
C-   Inputs  : none
C-   Outputs : LFCHT: address of FCHT bank
C-
C-   Created  19-AUG-1991   Robert E. Avery
C-                      based on BKDHIT by Qizhong Li-Demarteau
C-   Updated   4-NOV-1991   Robert E. Avery  use MZBOOK call.
C-   Updated  19-OCT-1993   Robert E. Avery  
C-                      New version (1), contains separate delay line hits.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFCHT.LINK'
      INTEGER NHITS
      INTEGER LFCHT, LFDCH, GZFDCH
      INTEGER ND,IXFCHT
      INTEGER HEADER_LENGTH ,TRAILER_LENGTH 
C
C----------------------------------------------------------------------
C
      LFDCH = GZFDCH()
      IF (LFDCH .LE. 0) CALL BKFDCH(LFDCH)
C
C ****  Book FCHT bank
C
      HEADER_LENGTH = 4
      TRAILER_LENGTH = 2
      ND = NHITS*2 + HEADER_LENGTH + TRAILER_LENGTH 
      CALL MZBOOK(IXMAIN,LFCHT,LFDCH,-IZFCHT,'FCHT',0,0,ND,1,0)
C
      IQ(LFCHT + 1) = HEADER_LENGTH    
C        
  999 RETURN
      END
