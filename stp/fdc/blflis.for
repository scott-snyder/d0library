      SUBROUTINE BLFLIS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book and fill bank FLIS, hanging from FGEH.
C-      Lists valid STP bank versions for MC data.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  20-AUG-1992   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZFLIS.LINK'
      INTEGER LFLIS
C----------------------------------------------------------------------
      CALL MZBOOK(IDVSTP,LFLIS,LSFDC,-IZFLIS,'FLIS',0,0,5,2,0)
      LFLIS=LC(LSFDC-IZFLIS)            !  oldest period
C
      IC(LFLIS+1)=0                     !  17-NOV-1858 00:00:00.00
      IC(LFLIS+2)=0
      IC(LFLIS+3)=1910079488            !  01-MAR-1992 00:00:00.00
      IC(LFLIS+4)=9793147
      IC(LFLIS+5)=1                     ! Version 1
C
      CALL MZBOOK(IDVSTP,LFLIS,LSFDC,-IZFLIS,'FLIS',0,0,5,2,0)
      LFLIS=LC(LSFDC-IZFLIS)            !  most recent period
C
      IC(LFLIS+1)=1910079488            !  01-MAR-1992 00:00:00.00 
      IC(LFLIS+2)=9793147
      IC(LFLIS+3)=-1495711744           !  01-JAN-3000 00:00:00.00
      IC(LFLIS+4)=83843238
      IC(LFLIS+5)=2                     ! Version 2
C
  999 RETURN
      END
