C DEC/CMS REPLACEMENT HISTORY, Element GTMHTT.FOR
C *3     3-OCT-1988 11:59:06 TAMI "Add argument IDELT"
C *2    27-AUG-1988 16:22:25 TAMI "ADD IPAD TO ARGUMENT LIST"
C *1     7-FEB-1988 17:33:51 TAMI "GET INFO FROM MHTT BANK"
C DEC/CMS REPLACEMENT HISTORY, Element GTMHTT.FOR
C====================================================================
      SUBROUTINE GTMHTT(ITRAK,IHIT,IWADD,IHMUOH,ITSIGN,IDELT,IPAD)
C====================================================================
C
C  Description:  Gets all the information from the MHTT Zebra bank for
C  ============  the hit # IHIT on track ITRAK
C
C  Argument Declarations:
C  ========================
C  ITRAK - INTEGER - Input - Number of the track for which one would like info.
C  IHIT - INTEGER - Input - Number of the hit for which one would like info.
C  IWADD - INTEGER - Output - Wire Address
C  IHMUOH  - INTEGER - Output - Pointer into MUOH bank
C  ITSIGN - INTEGER - Output - Time number and sign solution(-2,-1,0,1,2)
C  IDELT - INTEGER - Output - Delta time number and sign solution
C  IPAD   - INTEGER  Output - Pad solutions 
C  
C  Author:
C  =======
C  Tami Kramer
C
C  Revision History:
C  =================
C  Original Creation - June 29,1987
C  dh 5/88    add ipad
C=======================================================================
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
C  Argument Declarations:
C  ======================
C
      INTEGER IHIT,ITRAK
      INTEGER IWADD,ITSIGN,IHMUOH,IDELT,IPAD
C 
C  Local Declarations:
C  ===================
C
      INTEGER GZMHTT,LMHTT
C
C  Executable Code:
C  ================
C
      LMHTT = GZMHTT(ITRAK)
      IF (LMHTT .NE. 0) THEN
         IWADD = IQ(LMHTT+1+5*(IHIT-1))
         IHMUOH = IQ(LMHTT+2+5*(IHIT-1))
         ITSIGN = IQ(LMHTT+3+5*(IHIT-1))
         IDELT = IQ(LMHTT+4+5*(IHIT-1))
         IPAD = IQ(LMHTT+5+5*(IHIT-1))
      ELSE
         PRINT*,'ERROR IN ROUTINE GTMHTT - LMHTT=0'
      ENDIF
      RETURN
      END
