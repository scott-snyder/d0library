      SUBROUTINE DGTPDS(ID,VALUE,IFL)
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
C-   Updated   9-JUL-1991   Qizhong Li-Demarteau  handle error condition
C-                                                correctly 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDPED.LINK'
      INCLUDE 'D0$LINKS:IZDPCH.LINK'
C
      INTEGER ID,IFL,LDPCH
      INTEGER LDPED,NHEAD,LINKH,LDT,LZFIND
      INTEGER NEW_CRD,ICRD,NCH,CHNL,CRDCHN
      INTEGER LAYER,SECTOR,WIRE,UBIT,LDPDL
      INTEGER GZDPDL
      DATA NHEAD,NEW_CRD,CRDCHN /30,-1,16/
      REAL VALUE(2)
C----------------------------------------------------------------------
C
      IF (IFL.EQ.2) GO TO 100           ! Logical address input
C
      ICRD = ID/16                      ! Find FADC card number
      IF (ICRD.NE.NEW_CRD) THEN
        NEW_CRD = ICRD
        LDPCH = LC(LDPDH - IZDPCH)      ! Address of Pedestal Crate Header
        LINKH = LC(LDPCH - IZDPED)      ! Address of Pedestal bank
        LDPED = LZFIND(IDVSTP,LINKH,ICRD,10)    ! Find bank with Card
      ENDIF
C
      IF (LDPED.EQ.0) GO TO 900

      NCH = ID - ICRD*CRDCHN            ! Channel number within FADC card
      LDT = LDPED + NHEAD + 2*NCH       ! Pointer to location of channel
      ! pedestl and sigma in DPED bank
      VALUE(1) = C(LDT + 1)             ! Read pedestal value
      VALUE(2) = C(LDT + 2)             ! Read sigma value
      GO TO 999
C
  100 CONTINUE
C
C  *** Logical address as input >>>
C
      CALL DCODER(ID,LAYER,SECTOR,WIRE,UBIT,1)    ! Decode Logical address
      IF (UBIT.EQ.0) THEN                         ! Check if used channel
        LDPDL = GZDPDL(LAYER)                       ! Pointer to DPDL bank
        IF (LDPDL .LE. 0) GOTO 900
        LDPDL = LDPDL + (SECTOR * IC(LDPDL+4) + WIRE)
     &           * IC(LDPDL+3) + 4
        VALUE(1) = C(LDPDL+1)     ! Read pedestal value from logical bank
        VALUE(2) = C(LDPDL+2)     ! Read sigma
        GO TO 999
      ENDIF
  900 CONTINUE
      VALUE(1) = -1.                    ! Return -1 if error
      VALUE(2) = -1.                    ! Return -1 if error
C
  999 RETURN
      END
