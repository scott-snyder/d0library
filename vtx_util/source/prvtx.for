      SUBROUTINE PRVTX(PRUNIT,IFL)
C------------------------------------------------------------------
C-
C-  Calling routine for printing out Vertex Chamber hit banks
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  IFL   = 0      no printout
C-  IFL   = 1      no. of hits per layer
C-  IFL   = 2      no. of hits per bank in structure
C-  IFL   = 3      full printout
C-
C-       D.Z. JAN.,1987 
C-   Updated  18-OCT-1988   Ghita Rahal-Callot  : Corrected the call to PRVSEC
C-   if no hits 
C-   Updated  20-JUN-1989   Peter Grudberg - modify to use GEANT pr. flags
C-   Updated   3-OCT-1989   Peter Grudberg - use VTXLNK, special prints  
C-   Updated   4-NOV-1992   Peter M. Grudberg   Remove strips
C--------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:VTXLNK.INC/LIST'
      INTEGER PRUNIT,NHITS,IFL
      INTEGER KPVLAY,NVLAY,KPVSEC,NVSEC,KPVWDA,NVWDA
      INTEGER LAY,SEC,NUMSEC(0:2)
      CHARACTER*3 CFL
      DATA NUMSEC / 16, 32, 32 /
C
      IF ( IFL .LE. 0 ) GO TO 999
C          
C   Print out contents of bank VTXH
C
      IF (LVTXH.LE.0) GO TO 999    ! VTXH not booked
      CFL = 'ONE'
      CALL PRVTXH(PRUNIT,0,0,CFL,IFL)
C
C ****  If IFL = 1, use special printouts of hit banks for 
C ****  IFL = 1, CFL = 'ALL'
C
      IF ( IFL .EQ. 1 ) THEN
        CFL = 'ALL'
        CALL PRVLAY(PRUNIT,0,0,CFL,IFL)
        CALL PRVSEC(PRUNIT,0,0,CFL,IFL)
        CALL PRVWDA(PRUNIT,0,0,CFL,IFL)
        GO TO 999
      ENDIF
C
C ****  If IFL > 1, print banks individually
C
      CFL = 'ONE'
C
C ****  sense wires
C
      DO 100 LAY = 0, 2
        KPVLAY = LVLAY(LAY)
        CALL PRVLAY(PRUNIT,KPVLAY,NVLAY,CFL,IFL)
        DO 101 SEC = 0, NUMSEC(LAY) - 1
          KPVSEC = LVSEC(SEC,LAY)
          CALL PRVSEC(PRUNIT,KPVSEC,NVSEC,CFL,IFL)
          KPVWDA = LVWDA(SEC,LAY)
          CALL PRVWDA(PRUNIT,KPVWDA,NVWDA,CFL,IFL)
  101   CONTINUE
  100 CONTINUE
  999 CONTINUE
      RETURN
      END                     
