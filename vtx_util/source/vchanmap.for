      SUBROUTINE VCHANMAP(LABEL, CRATE, CARD, CHANNEL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Translate logical LABEL to electronic address
C-                         (crate, card, channel)
C-
C-   Inputs  : LABEL = packed VTX logical channel number
C-   Outputs : Electronic channel address:
C-             CRATE(0-9)
C-             CARD(0-15)
C-             CHANNEL(0-15)
C-   Controls: 
C-
C-   Created  12-NOV-1990   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C I/O:
      INTEGER LABEL
      INTEGER CRATE, CARD, CHANNEL
C LOCAL:
      INTEGER TYP, LAY, SEC, WIR, STR, END, UB
      INTEGER GROUP, OFFSET, CHN, INOUT
C DATA:
      INTEGER WIRCRT(0:15,0:1,0:2), WIRCRD(0:15,0:2)
      INTEGER PADCRT(0:11,1:5), PADCRD(0:11,1:5)
      INTEGER OFFS(1:5), NSTRP(1:5), PADCHAN(0:15,0:1)
C
C ****  Map data: (note: -1 for unused entries)
C GROUP:            0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
      DATA WIRCRT 
     &  /           1, 1, 1, 1, 1, 1, 1, 1,-1,-1,-1,-1,-1,-1,-1,-1,! L0 E0
     &              0, 0, 0, 0, 0, 0, 0, 0,-1,-1,-1,-1,-1,-1,-1,-1,! L0 E1
     &              3, 3, 3, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 3, 3, 3,! L1 E0
     &              2, 2, 2, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4, 2, 2, 2,! L1 E1
     &              9, 9, 9, 9, 9, 7, 7, 7, 7, 7, 7, 7, 7, 9, 9, 9,! L2 E0
     &              8, 8, 8, 8, 8, 6, 6, 6, 6, 6, 6, 6, 6, 8, 8, 8 ! L2 E1
     &  /
      DATA WIRCRD 
     &  /           9,10,11,15,14,13,12, 8,-1,-1,-1,-1,-1,-1,-1,-1,! L0
     &              7, 9,11,13,15,15,13,11, 9, 7, 5, 3, 1, 1, 3, 5,! L1
     &              6, 8,10,12,14,14,12,10, 9, 7, 6, 5, 3, 3, 4, 5 ! L2
     &  /
C
      DATA PADCRT
     &  /           4, 4, 2, 2, 2, 2, 2, 4, 4, 4,-1,-1,! Pad L2 E1
     &              3, 3, 5, 5, 5, 5, 5, 3, 3, 3,-1,-1,! Pad L2 E0
     &              3, 3, 3, 5, 5, 5, 2, 2, 2, 4, 4, 4,! Pad L3
     &              9, 9, 7, 7, 7, 8, 8, 8, 6, 6, 6, 9,! Pad L4
     &              8, 9, 9, 6, 6, 7, 7, 8,-1,-1,-1,-1 ! Pad L5
     &  /
      DATA PADCRD
     &  /           4, 0, 0, 4, 8,10,14,12,10, 6,-1,-1,! Pad L2 E1
     &             10,14,12,10, 6, 4, 0, 0, 4, 8,-1,-1,! Pad L2 E0
     &              2, 6,12,14, 8, 2, 2, 6,12,14, 8, 2,! Pad L3
     &             11,13,15,13,11, 7,11,13,15,13,11, 7,! Pad L4
     &             15, 9,15, 8, 4, 8, 4, 9,-1,-1,-1,-1 ! Pad L5
     &  /
      DATA OFFS / 8, 12, 7, 11, 0 / ! Starting strip of group 0
      DATA NSTRP / 160, 160, 192, 192, 128 /    ! # of strips / lay
C Channel mapping for pads:
C       CHN =  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
      DATA PADCHAN
     &  /       8, 9,10,11,12,13,14,15, 7, 6, 5, 4, 3, 2, 1, 0,! INOUT = 0
     &         15,14,13,12,11,10, 9, 8, 0, 1, 2, 3, 4, 5, 6, 7 ! INOUT = 1
     &  /
C----------------------------------------------------------------------
C
      CALL VCODER(LABEL,TYP,LAY,SEC,WIR,STR,END,UB,1)   ! decode LABEL
C
      IF ( TYP .EQ. 0 ) THEN           ! Wire channel
        IF ( LAY .EQ. 1 ) SEC = MOD(SEC+31,32)
        GROUP = SEC / 2
        CRATE = WIRCRT(GROUP,END,LAY)
        CARD  = WIRCRD(GROUP,LAY)       ! Same card numbers for the two
                                        ! ends
C
C ****  Channel number: one fadc card contains channels from two adjacent
C ****  sectors.  For the north end (end 0), the channels from the higher
C ****  numbered sector (odd numbered) appear in channels 0-7, and vice 
C ****  versa for the south end.
C
        OFFSET = 0
        IF ( END .EQ. MOD(SEC,2) ) OFFSET = 8
        CHANNEL = WIR + OFFSET
C
      ELSEIF ( TYP .EQ. 1 ) THEN       ! Pad channel
C
C ****  Pad layer 2 is split at z=0, so each strip is read out at both ends.
C ****  The other pad layers are made up of strips that cover the whole range
C ****  in z, and END just tells which end is read out, but doesn't carry
C ****  information necessary to determine the electronic channel.  Also, pad
C ****  layers 0 and 1 are not implemented in hardware.  In order to avoid
C ****  having to carry around END, I will treat pad layer 2 end 1 as pad layer
C ****  1.  (First, record whether the pad layer is inner or outer for later
C ****  use in determining channel number.)
C
        INOUT = MOD(LAY,2)
        IF ( LAY .EQ. 2 ) LAY = LAY - END
C
C ****  Group number: be sure to handle wraparound! (16 channels per fadc card)
C
        GROUP = MOD(STR-OFFS(LAY)+NSTRP(LAY),NSTRP(LAY)) / 16
        CRATE = PADCRT(GROUP,LAY)
        CARD  = PADCRD(GROUP,LAY)
        CHN = STR - GROUP*16 - OFFS(LAY)
        IF ( CHN .LT. 0 ) CHN = CHN + NSTRP(LAY)
C
C ****  The fadc channel number depends on both END and INOUT.  For end 1, the
C ****  mapping for CHN 0-7 is the same as CHN 8-15 for the opposite surface (
C ****  End 1 outer ch 0-7 corresponds to end 0 inner ch 8-15, etc)
C
        IF ( END .EQ. 1 ) THEN
          CHN = MOD(CHN+8,16)
          INOUT = 1 - INOUT
        ENDIF
        CHANNEL = PADCHAN(CHN,INOUT)
      ENDIF
  999 RETURN
      END
