      SUBROUTINE ZDEC_PULSER(PATTERN,AMPLTDA,AMPLTDB,POLARITY,ODD_EVEN,
     &  HALF,PREAMP,SHCARD,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Decode Pulser Pattern number.
C-
C-   Inputs  : PATTERN  = Pulser pattern value as downloaded to pulser
C-   Outputs : AMPLTB   = Amplitude of step pulse B (0-255)
C-             AMPLTA   = Amplitude of step pulse A (0-255)
C-             POLARITY = Polarity of step pulse (0=Neg, 1=Pos)
C-             ODD_EVEN = Whether none/odd/even/all channels have been selected
C-             HALF     = Upper(0) or lower(1) half of shaper crate
C-             PREAMP   = Whether pulsed through shaper or preamps
C-             SHCARD   = Shaper card number = 0 through 15 or 24 (all slots)
C-   Controls: IFL = 1/2, For IFL = 2, inputs/outputs reversed.
C-
C-   Created  18-OCT-1990   Srini Rajagopalan
C-
C-   Note : The pulser pattern is described a follows :
C-   
C-     Bit Numbers              Description
C-
C-      24 - 31                 Amplitude of step pulse - B
C-      16 - 23                 Amplitude of step pulse - A
C-        15                    Step pulse polarity (0=Negative)
C-      13 - 14                 Crate Select (0 = OFF)
C-      09 - 12                 Monitor slot select
C-      04 - 08                 Shaper slot number selection (24 = all)
C-        03                    Pulse through preamps = 0, through shaper = 1
C-        02                    Half number (0 = Upper)
C-      00 - 01                 Quadrant number (0/1/2/3 = none/odd/even/all)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER PATTERN,IFL
      INTEGER HALF,SLOT,ODD_EVEN,PREAMP
      INTEGER POLARITY,AMPLTDA,AMPLTDB,SHCARD
C----------------------------------------------------------------------
C
      IF (IFL.EQ.1) THEN
        AMPLTDB = IBITS(PATTERN,24,8)
        AMPLTDA = IBITS(PATTERN,16,8)
        POLARITY = IBITS(PATTERN,15,1)
        SLOT = IBITS(PATTERN,4,5)
        PREAMP = IBITS(PATTERN,3,1)
        HALF = IBITS(PATTERN,2,1)
        ODD_EVEN = IBITS(PATTERN,0,2)
        IF (SLOT.NE.24) THEN
          SHCARD = SLOT - 2
        ELSE
          SHCARD = SLOT
        ENDIF
      ELSE
        IF (SHCARD.EQ.24) THEN
          SLOT = SHCARD
        ELSE
          SLOT = SHCARD + 2
        ENDIF
        CALL MVBITS(AMPLTDB,0,8,PATTERN,24)
        CALL MVBITS(AMPLTDA,0,8,PATTERN,16)
        CALL MVBITS(POLARITY,0,1,PATTERN,15)
        CALL MVBITS(SLOT,0,5,PATTERN,4)
        CALL MVBITS(PREAMP,0,1,PATTERN,3)
        CALL MVBITS(HALF,0,1,PATTERN,2)
        CALL MVBITS(ODD_EVEN,0,2,PATTERN,0)
      ENDIF
C        
  999 RETURN
      END
