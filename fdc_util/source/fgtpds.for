      SUBROUTINE FGTPDS(ID,VALUE,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract Pedestal average and sigma from 
C-   electronic STP banks
C-
C-   Inputs  : ID = Electronic channel number 
C-   Outputs : VALUE
C-   Outputs : VALUE(1) = Electronics pedestal offset corresponding to ID
C-             VALUE(2) = pedestal sigma
C-   Controls: IFL ,not used kept in for backwards compatability
C-
C-   Created  28-FEB-1989   Srini Rajagopalan
C-   Updated   5-MAR-1991   Jeffrey Bantly  add third control path
C-   Updated   8-APR-1991   Robert E. Avery  create separate routine to read
C-   logical bank. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZFPED.LINK'
      INCLUDE 'D0$LINKS:IZFPCH.LINK'
C INPUT:
      INTEGER ID,IFL
C OUTPUT:
      REAL VALUE(2)
C LOCAL:
      INTEGER LFPED,NHEAD,LINKH,LDT,LZFIND
      INTEGER NEW_CRD,ICRD,NCH,CHNL,CRDCHN
      INTEGER NWIRE,NDELAY,WPARAM
      INTEGER LKFPSE,LFPCH,GZFPSE
C
C
      DATA NHEAD,NEW_CRD,CRDCHN /30,-1,16/
C----------------------------------------------------------------------
C
      ICRD = ID/16                      ! Find FADC card number
      IF (ICRD.NE.NEW_CRD) THEN
        NEW_CRD = ICRD
        LFPCH = LC(LFPDH - IZFPCH)      ! Address of Pedestal Crate Header
        LINKH = LC(LFPCH - IZFPED)      ! Address of Pedestal bank
        LFPED = LZFIND(IDVSTP,LINKH,ICRD,10)    ! Find bank with Card
      ENDIF
C
      NCH = ID - ICRD*CRDCHN            ! Channel number within FADC card
      LDT = LFPED + NHEAD + 2*NCH       ! Pointer to location of channel
C                                       ! pedestal and sigma in FPED bank
      IF (LFPED.NE.0) THEN
        VALUE(1) = C(LDT + 1)               ! Read pedestal value
        VALUE(2) = C(LDT + 2)               ! Read sigma value
      ELSE
        VALUE(1) = -1.                    ! Return -1 if error
        VALUE(2) = -1.                    ! Return -1 if error
      ENDIF
C---------------------------------------------------------------------------
  999 RETURN
      END
