      SUBROUTINE BKGSWT(LGSWT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : BOOKS THE GSWT BANK
C-
C-   Inputs  :
C-   Outputs : LINK OF CREATED GSWT BANK
C-   Controls:
C-
C-   Created  17-JUL-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZGSWT.LINK'
      INCLUDE 'D0$INC:D0LOG.INC'
      INCLUDE 'D0$INC:GCBANK.INC'
      INCLUDE 'D0$INC:GCLINK.INC'
      INCLUDE 'D0$INC:GCMAIL.INC'
      INTEGER NL,NS,ND,LGSWT,IXIO,LD0RG,GZD0RG
      DATA NL,NS/0,0/
C----------------------------------------------------------------------
      LD0RG = GZD0RG()
      IF(LD0RG.EQ.0)CALL BKD0RG(LD0RG)
      LGSWT = LQ(LD0RG-IZGSWT)
      IF(LGSWT.NE.0)THEN
        WRITE(CHMAIL,*)' GSWT BANK ALREADY PRESENT '
        CALL GMAIL(0,0)
        RETURN
      ELSE
        CALL MZFORM('GSWT','9I 10F 10I 50F 6I 10F 2I 10F 2I 10F',
     &    IXIO)
        CALL MZBOOK(IXCONS,LGSWT,LD0RG,-IZGSWT,
     &    'GSWT',0,0,NSWTS+2,IXIO,-1)
C
        IQ(LGSWT+1) = 1                 ! VERSION NUMBER
        WRITE(CHMAIL,*)' CREATING GSWT BANK '
        CALL GMAIL(0,0)
        RETURN
      ENDIF
  999 RETURN
      END
