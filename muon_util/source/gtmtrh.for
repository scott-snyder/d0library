C DEC/CMS REPLACEMENT HISTORY, Element GTMTRH.FOR
C *2    13-JUN-1988 16:16:56 TAMI "CHECK FOR LMTRH=0"
C *1     7-FEB-1988 17:34:14 TAMI "GET INFO FROM MTRH BANK"
C DEC/CMS REPLACEMENT HISTORY, Element GTMTRH.FOR
C====================================================================
      SUBROUTINE GTMTRH(NTRAKS)
C====================================================================
C
C  Description:  Gets all the information from the MTRH Zebra bank.
C  ============
C
C  Argument Declarations:
C  ========================
C  NTRAKS - INTEGER - Output - Number of found muon tracks
C  
C  Author:
C  =======
C  Tami Kramer
C
C  Revision History:
C  =================
C  Original Creation - June 29,1987
C
C=======================================================================
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
C  Argument Declarations:
C  ======================
C
      INTEGER NTRAKS
C 
C  Local Declarations:
C  ===================
C
      INTEGER GZMTRH,LMTRH
C
C  Executable Code:
C  ================
C
      LMTRH = GZMTRH(0)
      IF (LMTRH .NE. 0) THEN
         NTRAKS = IQ(LMTRH+1)
      ELSE
         NTRAKS=0
      ENDIF
      RETURN
      END
