      SUBROUTINE VCODER(ADDR,TYPE,LAYER,SCTR,WIRE,STRIP,END,UBIT,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : decode or encode address into detector 
C-                         components for the VTX
C-   
C-   Inputs  : IFL = 1: decode ADDR (ADDR is input)
C-                 = 2: encode ADDR (ADDR is output)
C- For IFL =1:
C-   Inputs  : ADDR = coded address of VTX channel
C-   Outputs : TYPE = 0 for wire channels, = 1 for z-strips (bit 12)
C-             LAYER = 0-2 for wires, 0-5 for z-strips (bits 9-11;TYPE=0 or 1)
C-             SCTR = 0-31(15 for wire layer 0) (bits 4-8; TYPE=0)
C-             WIRE = 0-7 (bits 1-3; TYPE=0)
C-             STRIP = 0-191 (bits 1-8; TYPE=1)(# of strips depends on z-layer)
C-             END = 0-1 (bit 0; TYPE=0 or 1)
C-             UBIT = 1 for inactive channel (bit 15; TYPE=0 or 1)
C-             Bits 13-14 are used for subdetector id (=0 for VTX)
C- Input/Output reversed for IFL = 2
C-
C-   Created   2-DEC-1988   Srini Rajagopalan
C-   Updated   5-MAR-1989   SR, Added IFL = 2 feature for reverse decoding
C-                          Redfined routine name to CTD_
C-   Updated  18-APR-1989   Accomodated change in Logical bit pattern - SR
C-                          Most significant bit is 0 always - sign bit
C-   Updated  2-MAY-1989    Removed sign bit, bit pattern changed as above
C-   Modified 10-MAY-1989  Peter Grudberg: create VCODER from FCODER
C-   Modified 16-JUL-1990   Tom Trippe: removed C&IF VAXVMS.  OK with CERNLIB 
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IFL,ADDR,IBITS
      INTEGER TYPE,LAYER,SCTR,WIRE,STRIP,END,UBIT,DETT
C----------------------------------------------------------------------
C
      IF (IFL.EQ.2) GO TO 10
        UBIT = IBITS(ADDR,15,1)
        DETT = IBITS(ADDR,13,2)
        TYPE = IBITS(ADDR,12,1)
        LAYER = IBITS(ADDR,9,3)
C
        IF ( TYPE .EQ. 0 ) THEN         ! Wire channel
          SCTR   = IBITS(ADDR,4,5)
          WIRE   = IBITS(ADDR,1,3)
        ELSE                            ! Strip channel
          STRIP = IBITS(ADDR,1,8)
        ENDIF
        END = IBITS(ADDR,0,1)           ! Same for wires and strips
        GO TO 999
   10   CONTINUE
C
        DETT = 0                        ! VTX
        CALL MVBITS(UBIT,0,1,ADDR,15)
        CALL MVBITS(DETT,0,2,ADDR,13)   ! Not really necessary
        CALL MVBITS(TYPE,0,1,ADDR,12)
        CALL MVBITS(LAYER,0,3,ADDR,9)
        IF ( TYPE .EQ. 0 ) THEN         ! Wire channel
          CALL MVBITS(SCTR,0,5,ADDR,4)
          CALL MVBITS(WIRE,0,3,ADDR,1)
        ELSE                            ! Strip channel
          CALL MVBITS(STRIP,0,8,ADDR,1)
        ENDIF
        CALL MVBITS(END,0,1,ADDR,0)
C
  999 RETURN
      END
