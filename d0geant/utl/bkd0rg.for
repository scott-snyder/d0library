      SUBROUTINE BKD0RG(LD0RG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Books D0 Run header bank for Geant
C-
C-   Inputs  :
C-   Outputs : LINK OF CREATED BANK.
C-   Controls:
C-
C-   Created  17-JUL-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZD0RG.LINK'
      INCLUDE 'D0$INC:GCBANK.INC'
      INCLUDE 'D0$INC:GCLINK.INC'
      INCLUDE 'D0$INC:GCMAIL.INC'
      INTEGER NL,NS,ND,LD0RG,IXIO
      DATA NL,NS,ND/10,10,93/
C----------------------------------------------------------------------
      LD0RG = LQ(JRUNG-IZD0RG)
      IF(LD0RG.NE.0)THEN
        WRITE(CHMAIL,*)' D0RG BANK ALREADY PRESENT '
        CALL GMAIL(0,0)
        RETURN
      ELSE
        CALL MZFORM('D0RG','3I -H',IXIO)
        CALL MZBOOK(IXCONS,LD0RG,JRUNG,-IZD0RG,'D0RG',NL,NS,ND,IXIO,-1)
        IQ(LD0RG+1) = 1                 ! BANK VERSION NUMBER
        WRITE(CHMAIL,*)' CREATING D0RG BANK '
        CALL GMAIL(0,0)
        RETURN
      ENDIF
      END
