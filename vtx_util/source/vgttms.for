      SUBROUTINE VGTTMS(ID, VALUE, IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract times average and sigma from STP banks
C-
C-   Inputs  : ID  = Electronic channel number for IFL = 1
C-                 = Logical channel number for IFL = 2
C-   Outputs : VALUE(2) : Times average and sigma corresponding to ID
C-   Controls: IFL = 1, to read Electronic structure
C-                 = 2, to read Logical bank structure
C-
C-   Created   3-JAN-1991   Qizhong Li-Demarteau    adapted from FGTTMS
C-   Updated   8-FEB-1991   Peter Grudberg  Create VGTTMS from DGTTMS 
C-   Updated  19-OCT-1992   Peter M. Grudberg  Handle both versions of VTMW;
C-                                             remove strips 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVTMS.LINK'
      INCLUDE 'D0$LINKS:IZVTCH.LINK'
C
      INTEGER ID, IFL, IVERS
      INTEGER LVTMS,NHEAD,LINKH,LVTM,LZFIND
      INTEGER NEW_CRD,ICRD,NCH,CHNL,CRDCHN
      INTEGER LAYER, SECTOR, WIRE, STRIP, END, TYPE, UBIT
      INTEGER GZVTMW, GZVTMZ, LVTMW, LVTMZ, LVTCH
      REAL VALUE(2)
      DATA NHEAD,NEW_CRD,CRDCHN /30,-1,16/
C----------------------------------------------------------------------
C
      IF (IFL.EQ.2) GO TO 100
C
      ICRD = ID/16
      IF (ICRD.NE.NEW_CRD) THEN
        NEW_CRD = ICRD
        LVTCH = LC(LVTMH - IZVTCH)
        LINKH = LC(LVTCH - IZVTMS)
        LVTMS = LZFIND(IDVSTP,LINKH,ICRD,10)    ! Find bank with Card
      ENDIF
C
      IF (LVTMS.EQ.0) GO TO 900
C
      NCH = ID - ICRD*CRDCHN
      LVTM = LVTMS + NHEAD + 2*NCH
      VALUE(1) = C(LVTM + 1)
      VALUE(2) = C(LVTM + 2)
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
          LVTMW = GZVTMW(LAYER)         ! Pointer to VTMW bank
          IF ( LVTMW .NE. 0 ) THEN
            IVERS = IBITS(LVTMW,13,5)
            IF ( IVERS .EQ. 1 ) THEN
              LVTMW = LVTMW + 
     &          (SECTOR*IC(LVTMW+4)+WIRE)*IC(LVTMW+3) + 2*END + 5
              VALUE(1) = C(LVTMW+1)     ! Read tzero value from logical bank
              VALUE(2) = C(LVTMW+2)     ! Read sigma
            ELSEIF ( IVERS .EQ. 0 ) THEN ! (Monte Carlo)
              LVTMW = LVTMW + 
     &          (SECTOR*IC(LVTMW+4)+WIRE)*IC(LVTMW+3) + 5
              VALUE(1) = C(LVTMW+1)   ! Tzero
              VALUE(2) = C(LVTMW+2)   ! Drift velocity
            ENDIF
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
