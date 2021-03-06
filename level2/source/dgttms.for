      SUBROUTINE DGTTMS(ID,LAYER,SECTOR,WIRE,VALUE,VEL,LENGTH,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract times average and sigma from STP banks
C-
C-   Inputs  : NID = Electronic channel number for IFL = 1
C-                 = Logical channel number for IFL = 2
C-   Outputs : VALUE(2) : Times average and sigma corresponding to ID
C-   Controls: IFL = 1, to read Electronic structure
C-                 = 2, to read Logical bank structure
C-
C-   Created   3-JAN-1991   Qizhong Li-Demarteau    adapted from FGTTMS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$LINKS:IZDTMS.LINK'
      INCLUDE 'D0$LINKS:IZDTCH.LINK'
C
      INTEGER ID,IFL
      INTEGER LDTMS,NHEAD,LINKH,LDT,LZFIND
      INTEGER NEW_CRD,ICRD,NCH,CRDCHN
      INTEGER LAYER, SECTOR, WIRE, UBIT
      INTEGER GZL2DTMW, GZL2DTMD, LKDTMW, ET0OFF, INDEX, LDTCH
      DATA NHEAD,NEW_CRD,CRDCHN /30,-1,16/
      REAL VALUE(2),VEL,LENGTH
C----------------------------------------------------------------------
C
      IF (IFL.EQ.2) GO TO 100
      IF (IFL.EQ.3) GO TO 200
C
      ICRD = ID/16
      IF (ICRD.NE.NEW_CRD) THEN
        NEW_CRD = ICRD
        LDTCH = LC(LDTMH - IZDTCH)
        LINKH = LC(LDTCH - IZDTMS)
        LDTMS = LZFIND(IDVSTP,LINKH,ICRD,10)    ! Find bank with Card
      ENDIF
C
      IF (LDTMS.EQ.0) GO TO 900
C
      NCH = ID - ICRD*CRDCHN
      LDT = LDTMS + NHEAD + 2*NCH
      VALUE(1) = C(LDT + 1)
      VALUE(2) = C(LDT + 2)
      GO TO 999
C
  100 CONTINUE
C
      CALL DCODER(ID,LAYER,SECTOR,WIRE,UBIT,1)    ! Decode Logical address
  200 CONTINUE
      IF (UBIT.EQ.0) THEN
        IF (WIRE .LE. MXSENS) THEN
          LKDTMW = GZL2DTMW(LAYER)   ! sense wires
          ET0OFF = 3               ! offset of electronic T0
        ELSE
          LKDTMW = GZL2DTMD(LAYER)   ! delay lines
          ET0OFF = 2               ! offset of electronic T0
        ENDIF
        INDEX = LKDTMW + (SECTOR * IC(LKDTMW + 4) + WIRE) *
     &                  IC(LKDTMW + 3) + 4 + ET0OFF
        IF (LKDTMW.NE.0) THEN
          VALUE(1) = C(INDEX + 1)
          VALUE(2) = C(INDEX + 2)
          IF (WIRE.GT.6) THEN
            LENGTH = C(INDEX+3)         ! Length of delay line
            VEL = C(INDEX + 4)          ! delay line velocity
          ENDIF
          GO TO 999
        ENDIF
      ENDIF
  900 CONTINUE
      VALUE(1) = 0.
      VALUE(2) = 0.
C
  999 RETURN
      END
