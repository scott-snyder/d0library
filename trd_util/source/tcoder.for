      SUBROUTINE TCODER(ADDR,LAYER,WIRE,UBIT,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : decode or encode address into detector
C-                         components for the TRD
C-
C-   Inputs  : IFL = 1: decode ADDR (ADDR is input)
C-                 = 2: encode ADDR (ADDR is output)
C- For IFL =1:
C-   Inputs  : ADDR = coded address of TRD Channel
C-   Outputs :
C-            Version for 256 bins:
C-            --------------------
C-             LAYER = 0-5  (bits 8:10)
C-             WIRE = 0-255 (bits 0-7)
C-             Bits 11 and 12 are spare.
C-            Version for 512 bins:
C-            --------------------
C-             LAYER = 0-5  (bits 8:11)
C-             WIRE = 0-511 (bits 0-8)
C-             Bits  12 is spare.
C-            Common to both versions
C-            -----------------------
C-             Bits 13-14 are used for subdetector id (=3 for TRD)
C-             UBIT = 1 for inactive channel (bit 15)
C- Input/Output reversed for IFL = 2
C-
C-   Created : 15-JUN-1989   Srini Rajagopalan
C-   Updated : 3-Apr-1992    Herbert Greenlee
C-   Updated  5-JUL-1993 A.ZYLBERSTEJN: allow 512 wires for layer 3
C-   Updated 4-OCT-1993 JFG Corrected a few bugs.
C-   Updated  12-OCT-1993 Jean-Francois Glicenstein: add a call to
C-                        TRD_NWIRE_PER_LAYER to avoid NWIRE_PER_LAYER
C-                        to be undefined.
C-   Updated  25-JAN-1994   A. Zylberstejn  Swap parity of cells in layer3
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.INC'
      INTEGER IFL,ADDR,SPARE,WI
      INTEGER N3,NBIT
      INTEGER LAYER,WIRE,UBIT,DETT
      LOGICAL FIRST,RUN1A
      DATA SPARE /0/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL TRD_NWIRE_PER_LAYER
        NBIT = 8
        IF (.NOT.RUN1A()) NBIT = 9
        N3 = NBIT+3
        DETT = 3                        ! TRD
        FIRST = .FALSE.
      ENDIF
      IF (IFL.EQ.1) THEN
        UBIT = IBITS(ADDR,15,1)         ! Unused Channel Bit
        DETT = IBITS(ADDR,13,2)         ! Detector Type , TRD = 3
        LAYER = IBITS(ADDR,NBIT,3)      ! Layer Number
        WIRE = IBITS(ADDR,0,NBIT)          ! Wire Number
        IF(.NOT.RUN1A() .AND. LAYER.EQ.2)WIRE=WIRE+2*MOD(WIRE+1,2)-1
        GO TO 999
      ELSE
        CALL MVBITS(UBIT,0,1,ADDR,15)
        CALL MVBITS(DETT,0,2,ADDR,13)
        CALL MVBITS(SPARE,0,NBIT - 8,ADDR,N3)
        CALL MVBITS(LAYER,0,3,ADDR,NBIT)
        WI=WIRE
        IF(.NOT.RUN1A() .AND. LAYER.EQ.2)WI=WIRE+2*MOD(WIRE+1,2)-1
        CALL MVBITS(WI,0,NBIT,ADDR,0)
      END IF
C
  999 RETURN
      END
