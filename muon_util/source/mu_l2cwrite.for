      SUBROUTINE MU_L2CWRITE(LUN,OUTPUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write packed muon l2 data stored in STPN
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   4-JUN-1990   Ralph Zazula
C-   Modified    Jun-1990   J.Green      changed OPEN to D0OPEN
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPN.LINK'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSMUO.LINK'
      INCLUDE 'D0$LINKS:IZMPDH.LINK'
C
      INTEGER  LUN
      INTEGER  KSMUO,KSTPN
      CHARACTER*60 MSG
      CHARACTER*14 FITITLE
      CHARACTER*4 OUTPUT
      LOGICAL  LOK
C----------------------------------------------------------------------
C
C Write the entire SMUO bank under STPN
C
      write(*,*)'in l2cwrite...'
      KSTPN=LC(LSTPH-IZSTPN)
      IF (KSTPN.NE.0) THEN
        KSMUO=LC(KSTPN-IZSMUO)
        IF (KSMUO.NE.0) THEN
          IF(OUTPUT.EQ.'COMP') THEN
            FITITLE='MUON_L2.CONST'
          ELSEIF(OUTPUT.EQ.'NORM') THEN
            FITITLE='MU_NORM.CONST'
          ENDIF
          CALL D0OPEN(LUN, FITITLE, 'OU', LOK )
C                               TYPE='NEW',FORM='UNFORMATTED'
          CALL FZFILE(LUN,0,'O')
          CALL FZOUT(LUN,IDVSTP,KSMUO,1,' ',1,0,0)
          CLOSE(LUN)
          MSG = 'L2CWRITE: MUON_L2.CONST file created.'
          CALL INTMSG(MSG)
        ELSE
          MSG = 'L2CWRITE: SMUO bank not found.'
          CALL INTMSG(MSG)
        ENDIF
      ELSE
        MSG = 'L2CWRITE: STPN bank not found.'
        CALL INTMSG(MSG)
      ENDIF
C
  999 RETURN
      END
