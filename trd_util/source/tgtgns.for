      SUBROUTINE TGTGNS(ID,VALUE,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Gets gain values
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  26-MAR-1990   Srini Rajagopalan
C-   Updated   5-OCT-1990   JFG Adapted to TRD 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IFL,I,ID
      REAL VALUE(10)
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTGNS.LINK'
      INCLUDE 'D0$LINKS:IZTGCH.LINK'
C
      INTEGER LTGNS,NHEAD,LINKH,LDT,LZFIND
      INTEGER NEW_CRD,ICRD,NCH,CHNL,CRDCHN
      INTEGER LTGCH
      DATA NHEAD,NEW_CRD,CRDCHN /30,-1,16/
C----------------------------------------------------------------------
C
      IF (IFL.EQ.2) GO TO 900           ! Logical address input
C
      ICRD = ID/16                      ! Find FADC card number
      IF (ICRD.NE.NEW_CRD) THEN
        NEW_CRD = ICRD
        LTGCH = LC(LTGAI - IZTGCH)      ! Address of Gain Crate Header
        LINKH = LC(LTGCH - IZTGNS)      ! Address of Gain bank
        LTGNS = LZFIND(IDVSTP,LINKH,ICRD,10)    ! Find bank with Card
      ENDIF
C
      NCH = ID - ICRD*CRDCHN            ! Channel number within FADC card
      LDT = LTGNS + NHEAD + 5*NCH       ! Pointer to location of channel
                                        ! pedestl and sigma in TGNS bank
      IF (LTGNS.EQ.0) GO TO 900
      VALUE(1) = C(LDT + 1)             ! Read intercept value
      VALUE(2) = C(LDT + 2)             ! Read sigma intercept value
      VALUE(3) = C(LDT + 3)             ! Read slope value
      VALUE(4) = C(LDT + 4)             ! Read sigma slope value
      VALUE(5) = C(LDT + 5)             ! Read chi2 of the fit
      GO TO 999
C
  900 CONTINUE
C
      DO I = 1,10
        VALUE(I) = -1.
      ENDDO
C
  999 RETURN
      END
