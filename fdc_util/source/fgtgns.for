      SUBROUTINE FGTGNS(ID,VALUE,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract electronic gains etc., from
C-   electronic STP banks
C-
C-   Inputs  : ID = Electronic channel number 
C-   Outputs : VALUE(1) = Intercept value corresponding to ID
C-             VALUE(2) = Intercept sigma value  
C-             VALUE(3) = Slope value            
C-             VALUE(4) = Slope sigma value      
C-             VALUE(5) = chisq value            
C-   Controls: IFL ,not used kept in for backwards compatability
C-
C-   Created  25-FEB-1991   Jeffrey Bantly  based on FGTPDS.FOR
C-   Updated   8-APR-1991   Robert E. Avery  create separate routine to read
C-   logical bank. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZFGNS.LINK'
      INCLUDE 'D0$LINKS:IZFGCH.LINK'
C INPUT:
      INTEGER ID,IFL
C OUTPUT:
      REAL VALUE(5)
C LOCAL:
      INTEGER LFGNS,NHEAD,LINKH,LDT
      INTEGER NEW_CRD,ICRD,NCH,CHNL,CRDCHN
      INTEGER NWIRE,NDELAY,WPARAM
      INTEGER LKFGSE,LFGCH
      INTEGER LZFIND,GZFGSE
C
      DATA NHEAD,NEW_CRD,CRDCHN /30,-1,16/
C----------------------------------------------------------------------
C
      ICRD = ID/16                      ! Find FADC card number
      IF (ICRD.NE.NEW_CRD) THEN
        NEW_CRD = ICRD
        LFGCH = LC(LFGNH - IZFGCH)      ! Address of Gain Crate Header
        LINKH = LC(LFGCH - IZFGNS)      ! Address of Gain bank
        LFGNS = LZFIND(IDVSTP,LINKH,ICRD,10)    ! Find bank with Card
      ENDIF
C
      NCH = ID - ICRD*CRDCHN            ! Channel number within FADC card
      LDT = LFGNS + NHEAD + 5*NCH       ! Pointer to location of channel
C                                       ! gain and mipconv in FGNS bank
      IF (LFGNS.NE.0) THEN
        VALUE(1)        = C(LDT + 1)      ! Read Intercept value        
        VALUE(2)        = C(LDT + 2)      ! Read Intercept sigma value  
        VALUE(3)        = C(LDT + 3)      ! Read Slope value            
        VALUE(4)        = C(LDT + 4)      ! Read Slope sigma value      
        VALUE(5)        = C(LDT + 5)      ! Read chisq value            
      ELSE
        VALUE(1) = -1.                    ! Return -1 if error
        VALUE(2) = -1.                    ! Return -1 if error
        VALUE(3) = -1.                    ! Return -1 if error
        VALUE(4) = -1.                    ! Return -1 if error
        VALUE(5) = -1.                    ! Return -1 if error
      ENDIF
C
C-------------------------------------------------------------------------
  999 RETURN
      END
