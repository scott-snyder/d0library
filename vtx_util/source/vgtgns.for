      SUBROUTINE VGTGNS(ID,VALUE,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract gain values from STP banks
C-
C-   Inputs  : ID = Electronic chan number for IFL = 1
C-                = Logical chan number for IFL = 2
C-   Outputs : VALUE(2): Gain average and sigma corresponding to ID
C-   Controls: IFL = 1 to read electronic structure
C-                 = 2 to read logical structure
C-
C-   Created  12-FEB-1991   Peter Grudberg Dummy at first
C-   Updated  19-OCT-1992   Peter M. Grudberg  Undummy (copy VGTPDS) 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVGNS.LINK'
      INCLUDE 'D0$LINKS:IZVGCH.LINK'
C
      INTEGER ID,IFL,LVGCH
      INTEGER LVGNS,NHEAD,LINKH,LDT,LZFIND
      INTEGER NEW_CRD,ICRD,NCH,CHNL,CRDCHN
      INTEGER LAYER,SECTOR,WIRE,UBIT,LVGNL,TYPE,STRIP,END
      INTEGER GZVGNL
      REAL VALUE(5)
      DATA NHEAD,NEW_CRD,CRDCHN /30,-1,16/
C----------------------------------------------------------------------
C
      IF (IFL.EQ.2) GO TO 100           ! Logical address input
C
      ICRD = ID/16                      ! Find FADC card number
      IF (ICRD.NE.NEW_CRD) THEN
        NEW_CRD = ICRD
        LVGCH = LC(LVGNH - IZVGCH)      ! Address of Pedestal Crate Header
        LINKH = LC(LVGCH - IZVGNS)      ! Address of Pedestal bank
        LVGNS = LZFIND(IDVSTP,LINKH,ICRD,10)    ! Find bank with Card
      ENDIF
C
      IF (LVGNS.EQ.0) GO TO 900

      NCH = ID - ICRD*CRDCHN            ! Channel number within FADC card
      LDT = LVGNS + NHEAD + 5*NCH       ! Pointer to location of channel
                                        ! in VGNS bank
      VALUE(1) = C(LDT + 1)             ! Intercept
      VALUE(2) = C(LDT + 2)             ! Intercept sigma
      VALUE(3) = C(LDT + 3)             ! Slope
      VALUE(4) = C(LDT + 4)             ! Slope sigma
      VALUE(5) = C(LDT + 5)             ! chisq
      GO TO 999
C
  100 CONTINUE
C
C  *** Logical address as input >>>
C
      CALL VCODER(ID,TYPE,LAYER,SECTOR,WIRE,STRIP,END,UBIT,1)   
C                                                 ! Decode Logical address
      IF ( UBIT .EQ. 0 ) THEN           ! Check if used channel
        IF ( TYPE .EQ. 0 ) THEN         ! Wire channel
          LVGNL=GZVGNL(LAYER)           ! Pointer to VGNL bank
          IF ( LVGNL .NE. 0 ) THEN
            LVGNL = LVGNL + 
     &         (SECTOR*IC(LVGNL+4)+2*WIRE+END)*IC(LVGNL+3) + 128
            VALUE(1) = C(LVGNL+1)     ! Read gain value from logical bank
            VALUE(2) = -1.
            VALUE(3) = -1.
            VALUE(4) = -1.
            VALUE(5) = -1.
            GO TO 999
          ENDIF
        ENDIF
      ENDIF
  900 CONTINUE
      VALUE(1) = -1.                    ! Return -1 if error
      VALUE(2) = -1.                    ! Return -1 if error
      VALUE(3) = -1.                    ! Return -1 if error
      VALUE(4) = -1.                    ! Return -1 if error
      VALUE(5) = -1.                    ! Return -1 if error
C
  999 RETURN
      END
