      SUBROUTINE TGTPDS(ID,VALUE,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract Pedestal average and sigma from STP banks
C-
C-   Inputs  : NID = Electronic channel number for IFL = 1
C-                 = Logical channel number for IFL = 2
C-   Outputs : VALUE(2) : Ped average and sigma corresponding to ID
C-   Controls: IFL = 1, to read Electronic structure
C-                 = 2, to read Logical bank structure
C-
C-   Created  28-FEB-1989   Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTPED.LINK'
      INCLUDE 'D0$LINKS:IZTPCH.LINK'
C
      INTEGER ID,IFL
      INTEGER LTPED,NHEAD,LINKH,LDT,LZFIND
      INTEGER NEW_CRD,ICRD,NCH,CHNL,CRDCHN
      INTEGER LTPCH
      DATA NHEAD,NEW_CRD,CRDCHN /30,-1,16/
      REAL VALUE(2)
C----------------------------------------------------------------------
C
      IF (IFL.EQ.2) GO TO 100           ! Logical address input
C
      ICRD = ID/16                      ! Find FADC card number
      IF (ICRD.NE.NEW_CRD) THEN
        NEW_CRD = ICRD
        LTPCH = LC(LTPDH - IZTPCH)      ! Address of Pedestal Crate Header
        LINKH = LC(LTPCH - IZTPED)      ! Address of Pedestal bank
        LTPED = LZFIND(IDVSTP,LINKH,ICRD,10)    ! Find bank with Card
      ENDIF
C
      NCH = ID - ICRD*CRDCHN            ! Channel number within FADC card
      LDT = LTPED + NHEAD + 2*NCH       ! Pointer to location of channel
                                        ! pedestl and sigma in FPED bank
      IF (LTPED.EQ.0) GO TO 900
      VALUE(1) = C(LDT + 1)             ! Read pedestal value
      VALUE(2) = C(LDT + 2)             ! Read sigma value
      GO TO 999
C
  100 CONTINUE
C
C  *** Logical address as input >>> dummy for now
C
  900 CONTINUE
      VALUE(1) = -1.                    ! Return -1 if error
      VALUE(2) = -1.                    ! Return -1 if error
C
  999 RETURN
      END
