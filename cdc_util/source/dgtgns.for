      SUBROUTINE DGTGNS(ID,VALUE,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract times average and sigma from STP banks
C-
C-   Inputs  : ID = Electronic channel number for IFL = 1
C-                = Logical channel number for IFL = 2
C-   Outputs : VALUE(1) = Intercept value corresponding to ID
C-             VALUE(2) = Intercept sigma value  
C-             VALUE(3) = Slope value            
C-             VALUE(4) = Slope sigma value      
C-             VALUE(5) = chisq value            
C-   Controls: IFL = 1, to read Electronic structure
C-                 = 2, to read Logical bank structure (VALUE(3) only)
C-
C-   Created   3-JUL-1991   Qizhong Li-Demarteau    adapted from FGTGNS 
C-                                                  and DGTGNS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$LINKS:IZDGNS.LINK'
      INCLUDE 'D0$LINKS:IZDGCH.LINK'
C
      INTEGER ID,IFL
      INTEGER LDGNS,NHEAD,LINKH,LDT,LZFIND
      INTEGER NEW_CRD,ICRD,NCH,CHNL,CRDCHN
      INTEGER LAYER, SECTOR, WIRE, UBIT
      INTEGER GZDGNL, LDGNL, INDEX, LDGCH
      DATA NHEAD,NEW_CRD,CRDCHN /30,-1,16/
      REAL VALUE(5)
C----------------------------------------------------------------------
C
      IF (IFL.EQ.2) GO TO 100
C
      ICRD = ID/16
      IF (ICRD.NE.NEW_CRD) THEN
        NEW_CRD = ICRD
        LDGCH = LC(LDGNH - IZDGCH)
        LINKH = LC(LDGCH - IZDGNS)
        LDGNS = LZFIND(IDVSTP,LINKH,ICRD,10)    ! Find bank with Card
      ENDIF
C
      IF (LDGNS.EQ.0) GO TO 900
C
      NCH = ID - ICRD*CRDCHN
      LDT = LDGNS + NHEAD + 2*NCH
      VALUE(1) = C(LDT + 1)      ! Read Intercept value        
      VALUE(2) = C(LDT + 2)      ! Read Intercept sigma value  
      VALUE(3) = C(LDT + 3)      ! Read Slope value            
      VALUE(4) = C(LDT + 4)      ! Read Slope sigma value      
      VALUE(5) = C(LDT + 5)      ! Read chisq value            
      GO TO 999
C
  100 CONTINUE
C
      CALL DCODER(ID,LAYER,SECTOR,WIRE,UBIT,1)    ! Decode Logical address
      IF (UBIT.EQ.0) THEN
        LDGNL = GZDGNL(LAYER)                       ! Pointer to DGNL bank
        IF (LDGNL .LE. 0) GOTO 900
        INDEX = LDGNL + (SECTOR * IC(LDGNL + 4) + WIRE) 
     &    * IC(LDGNL+3) + 4
        VALUE(3) = C(INDEX + 1)
        GO TO 999
      ENDIF
C
  900 CONTINUE
      VALUE(1) = -1.     
      VALUE(2) = -1.     
      VALUE(3) = -1.     
      VALUE(4) = -1.     
      VALUE(5) = -1.     
C
  999 RETURN
      END
