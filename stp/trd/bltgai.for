      SUBROUTINE BLTGAI
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Builds the ZEBRA bank structure hooked
C-                        to TGAI
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-   Created   5-OCT-1990   JFG Simplified version of TSTPBK
C-                          Do not book the whole structure.
C-   Updated  11-MAY-1992   Alain PLUQUET add MZFORM
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LTREL,LTRWG
      INTEGER LTELH,LTWGH
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTRD.LINK'
      INCLUDE 'D0$LINKS:IZTGAI.LINK'
      INCLUDE 'D0$LINKS:IZTELH.LINK'
      INCLUDE 'D0$LINKS:IZTWGH.LINK'
      INCLUDE 'D0$LINKS:IZTGCH.LINK'
      INTEGER IOTGAI,IOTELH,IOTREL,IOTWGH,IOTRWG
C Book header banks
      CALL MZFORM ('TGAI','-I',IOTGAI)
      CALL MZBOOK(IDVSTP,LTGAI,LSTRD,-IZTGAI,'TGAI',7,7,2,IOTGAI,0)
C Book electronics gains banks
      CALL MZFORM ('TELH','-I',IOTELH)
      CALL MZBOOK(IDVSTP,LTELH,LTGAI,-IZTELH,'TELH',6,6,2,IOTELH,0)
      CALL MZFORM ('TREL','-F',IOTREL)
      CALL MZBOOK(IDVSTP,LTREL,LTELH,-1,'TREL',0,0,256,IOTREL,0)
      CALL MZBOOK(IDVSTP,LTREL,LTELH,-2,'TREL',0,0,256,IOTREL,0)
      CALL MZBOOK(IDVSTP,LTREL,LTELH,-3,'TREL',0,0,512,IOTREL,0)
      CALL MZBOOK(IDVSTP,LTREL,LTELH,-4,'TREL',0,0,256,IOTREL,0)
      CALL MZBOOK(IDVSTP,LTREL,LTELH,-5,'TREL',0,0,256,IOTREL,0)
      CALL MZBOOK(IDVSTP,LTREL,LTELH,-6,'TREL',0,0,256,IOTREL,0)
C Book wire gains banks
      CALL MZFORM ('TWGH','-I',IOTWGH)
      CALL MZBOOK(IDVSTP,LTWGH,LTGAI,-IZTWGH,'TWGH',3,3,2,IOTWGH,0)
      CALL MZFORM ('TRWG','-F',IOTRWG)
      CALL MZBOOK(IDVSTP,LTRWG,LTWGH,-1,'TRWG',0,0,256,IOTRWG,0)
      CALL MZBOOK(IDVSTP,LTRWG,LTWGH,-2,'TRWG',0,0,256,IOTRWG,0)
      CALL MZBOOK(IDVSTP,LTRWG,LTWGH,-3,'TRWG',0,0,512,IOTRWG,0)
      END
