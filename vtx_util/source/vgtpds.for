      SUBROUTINE VGTPDS(ID,VALUE,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract Pedestal average and sigma from STP banks
C-
C-   Inputs  : ID  = Electronic channel number for IFL = 1
C-                 = Logical channel number for IFL = 2
C-   Outputs : VALUE(2) : Ped average and sigma corresponding to ID
C-   Controls: IFL = 1, to read Electronic structure
C-                 = 2, to read Logical bank structure
C-
C-   Created  28-FEB-1989   Srini Rajagopalan
C-   Modified 14-MAR-1990   Domenico Pizzuto  -- adapted for CDC
C-   Modified 10-JUL-1990   Tom Trippe -- Adapted for VTX
C-   Updated   8-FEB-1991   Peter Grudberg  Include zstrip banks 
C-   Updated  19-OCT-1992   Peter M. Grudberg  Remove strip banks 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVPED.LINK'
      INCLUDE 'D0$LINKS:IZVPCH.LINK'
C
      INTEGER ID,IFL,LVPCH
      INTEGER LVPED,NHEAD,LINKH,LDT,LZFIND
      INTEGER NEW_CRD,ICRD,NCH,CHNL,CRDCHN
      INTEGER LAYER,SECTOR,WIRE,UBIT,LVPDL,TYPE,STRIP,END
      INTEGER GZVPDL, GZVPDZ, LVPDZ
      REAL VALUE(2)
      DATA NHEAD,NEW_CRD,CRDCHN /30,-1,16/
C----------------------------------------------------------------------
C
      IF (IFL.EQ.2) GO TO 100           ! Logical address input
C
      ICRD = ID/16                      ! Find FADC card number
      IF (ICRD.NE.NEW_CRD) THEN
        NEW_CRD = ICRD
        LVPCH = LC(LVPDH - IZVPCH)      ! Address of Pedestal Crate Header
        LINKH = LC(LVPCH - IZVPED)      ! Address of Pedestal bank
        LVPED = LZFIND(IDVSTP,LINKH,ICRD,10)    ! Find bank with Card
      ENDIF
C
      IF (LVPED.EQ.0) GO TO 900

      NCH = ID - ICRD*CRDCHN            ! Channel number within FADC card
      LDT = LVPED + NHEAD + 2*NCH       ! Pointer to location of channel
                                        ! pedestal and sigma in VPED bank
      VALUE(1) = C(LDT + 1)             ! Read pedestal value
      VALUE(2) = C(LDT + 2)             ! Read sigma value
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
          LVPDL=GZVPDL(LAYER)           ! Pointer to VPDL bank
          IF ( LVPDL .NE. 0 ) THEN
            LVPDL = LVPDL + 
     &            (SECTOR*IC(LVPDL+4)+2*WIRE+END)*IC(LVPDL+3) + 5
            VALUE(1) = C(LVPDL+1)     ! Read pedestal value from logical bank
            VALUE(2) = C(LVPDL+2)     ! Read sigma
            GO TO 999
          ENDIF
        ENDIF
      ENDIF
  900 CONTINUE
      VALUE(1) = -1.                    ! Return -1 if error
      VALUE(2) = -1.                    ! Return -1 if error
C
  999 RETURN
      END
