      SUBROUTINE BKL2EM(NHTEM,LL2EM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank L2EM
C-            multiple calls result in creation of a linear chain
C-
C-   Input   : NHTEM Number of Hot EMs
C-   Outputs : Link of Booked L2EM Bank
C-   Controls: None
C-
C-   Created   3-NOV-1991 , Yi   Xia
C-   Modified  6-AUG-1992   S Fahey added AETA,APHI to bank
C-   Modified  2-SEP-1992   S Fahey added CUTBITS bit mask
C-   Modified  7-DEC-1992   James T. McKinley add vertex corrected eta
C-   Modified 29-MAR-1993   James T. McKinley bump version number, definition
C-                          of element 35 changed to include E leakage fix
C-   Updated  13-NOV-1993   James T. Linnemann   change version (cutbits defn)
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LL2EM,NHTEM,IOL2EM,NR
      INTEGER GZFRES,LFRES,NALOC
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZL2EM.LINK/LIST'
      PARAMETER (NR = 34)
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LL2EM = 0
      IF(FIRST)THEN
C
        CALL MZFORM('L2EM','3I/5I 18F 3I 6F 1I 1F',IOL2EM)      ! Describe Bank format
        FIRST=.FALSE.
      ENDIF
C
C--   FIND LINK TO SUPPORTING PARENT BANK
C
      LFRES = GZFRES()
      IF ( LFRES.EQ.0 ) THEN
        CALL BKFRES(LFRES)
      ENDIF
C
      NALOC = NHTEM*NR + 3      
      CALL MZBOOK
     &  (IXMAIN,LL2EM,LFRES,-IZL2EM,'L2EM',1,1,NALOC,IOL2EM,0)
      IQ(LL2EM+1) = 6       ! version number
      IQ(LL2EM+2) = NR      ! repeat number
      IQ(LL2EM+3) = NHTEM   ! Total hot EM number from L1 
C
  999 RETURN
      END
