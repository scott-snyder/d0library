      SUBROUTINE FGTTMS(ID,VALUE,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract Time average and sigma from 
C-   electronic STP banks
C-
C-   Inputs  : ID = Electronic channel number 
C-   Outputs : VALUE(1) = Electronics zero time offset corresponding to ID
C-             VALUE(2) = tzero sigma
C-   Controls: IFL ,not used kept in for backwards compatability
C-
C-   Created  25-FEB-1991   Jeffrey Bantly  based on FGTPDS.FOR
C-   Updated   8-APR-1991   Robert E. Avery  create separate routine to read
C-   logical bank. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZFTMS.LINK'
      INCLUDE 'D0$LINKS:IZFTCH.LINK'
C
C INPUT:
      INTEGER ID,IFL
C OUTPUT:
      REAL VALUE(2)
C LOCAL:
      INTEGER LFTMS,NHEAD,LINKH,LDT
      INTEGER NEW_CRD,ICRD,NCH,CHNL,CRDCHN
      INTEGER NWIRE,NDELAY,WPARAM
      INTEGER LKFTSE,LFTCH
      INTEGER LZFIND,GZFTSE
C
C
      DATA NHEAD,NEW_CRD,CRDCHN /30,-1,16/
C----------------------------------------------------------------------
C
      ICRD = ID/16                      ! Find FADC card number
      IF (ICRD.NE.NEW_CRD) THEN
        NEW_CRD = ICRD
        LFTCH = LC(LFTMH - IZFTCH)      ! Address of TZERO Crate Header
        LINKH = LC(LFTCH - IZFTMS)      ! Address of TZERO bank
        LFTMS = LZFIND(IDVSTP,LINKH,ICRD,10)    ! Find bank with Card
      ENDIF
C
      NCH = ID - ICRD*CRDCHN            ! Channel number within FADC card
      LDT = LFTMS + NHEAD + 2*NCH       ! Pointer to location of channel
C                                       ! in FTMS bank
      IF (LFTMS.NE.0) THEN
        VALUE(1) = C(LDT + 1)           ! Read Electronic T-zero value
        VALUE(2) = C(LDT + 2)           ! Electronic T-zero sigma
      ELSE
        VALUE(1) = -1.                    ! Return -1 if error
        VALUE(2) = -1.                    ! Return -1 if error
      ENDIF
C--------------------------------------------------------------------------
  999 RETURN
      END
