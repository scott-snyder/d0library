      SUBROUTINE DCODER(ADDR,LAYER,SCTR,WIRE,UBIT,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : decode or encode address into detector 
C-                         components for the CDC
C-   
C-   Inputs  : IFL = 1: decode ADDR (ADDR is input)
C-                 = 2: encode ADDR (ADDR is output)
C- For IFL =1:
C-   Inputs  : ADDR = coded address of TRD Channel
C-   Outputs : LAYER = 0-3  (bits 9:10)
C-             SCTR  = 0-31 (bits 4:8)
C-             WIRE = 0-15 (bits 0:3)
C-             Bits 11 and 12 are spare.
C-             Bits 13-14 are used for subdetector id (=1 for CDC)
C-             UBIT = 1 for inactive channel (bit 15)
C- Input/Output reversed for IFL = 2
C-
C-   Created : 15-JUN-1989   Srini Rajagopalan
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IFL,ADDR,SPARE
      INTEGER LAYER,SCTR,WIRE,UBIT,DETT
      DATA SPARE /0/
C----------------------------------------------------------------------
C
      IF (IFL.EQ.2) GO TO 10
        UBIT = IBITS(ADDR,15,1)         ! Unused Channel Bit
        DETT = IBITS(ADDR,13,2)         ! Detector Type , CDC = 1
        LAYER = IBITS(ADDR,9,2)         ! Layer Number
        SCTR = IBITS(ADDR,4,5)          ! Sector Number
        WIRE = IBITS(ADDR,0,4)          ! Wire Number
C
        GO TO 999
   10   CONTINUE
C
        DETT = 1                        ! CDC
        CALL MVBITS(UBIT,0,1,ADDR,15)
        CALL MVBITS(DETT,0,2,ADDR,13)   
        CALL MVBITS(SPARE,0,2,ADDR,11)
        CALL MVBITS(LAYER,0,2,ADDR,9)
        CALL MVBITS(SCTR,0,5,ADDR,4)
        CALL MVBITS(WIRE,0,4,ADDR,0)
C
  999 RETURN
      END
