C *1    26-NOV-1986 01:25:58 TRIPPE "ZEBRA output"
      SUBROUTINE PUVWDA(ILAYR,ISCTR)
C-----------------------------------------------------------------------
C- SUBROUTINE PUVWDA(ILAYR,ISCTR) eliminates the unused portion of
C- bank VWDA after it has been filled.
C- 
C- T. Trippe, 2 Nov. 1986
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:VTXLNK.INC/LIST'
      INTEGER NHMXV,NWDSDA
      INTEGER NPTWDA,MXVWDA,NWVWDA
      PARAMETER (NHMXV=50)
      INTEGER ILAYR,ISCTR,NHTSDA
C
      IF(LVWDA(ISCTR,ILAYR).NE.0) THEN
         MXVWDA=IQ(LVWDA(ISCTR,ILAYR)-1)
         NHTSDA=IQ(LVWDA(ISCTR,ILAYR)+1)
         NPTWDA=IQ(LVWDA(ISCTR,ILAYR)+2) 
         NWDSDA=IQ(LVWDA(ISCTR,ILAYR)+3)
         NWVWDA=3+NPTWDA*2+NWDSDA*NHTSDA
         CALL MZPUSH(IXCOM, LVWDA(ISCTR,ILAYR),0,NWVWDA-MXVWDA,'R')
      ENDIF
C
      RETURN
      END
