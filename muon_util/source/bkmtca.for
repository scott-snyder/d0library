      SUBROUTINE BKMTCA(LMUON,LMTCA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book MTCA bank
C-
C-   Inputs  :  LMUON 
C-   Outputs    LMTCA  address of MTCA
C-   Controls:
C-
C-   Created 9-FEB-1994   Daria Zieminska   
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LMUON,LMTCA,NIO,NDATA
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMTCA.LINK'
C
      LOGICAL FIRST
      SAVE FIRST, NIO
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        CALL MZFORM('MTCA','6I -F',NIO)
        FIRST = .FALSE.
        NDATA=40
      ENDIF
C
      CALL MZBOOK(IXMAIN,LMTCA,LMUON,-IZMTCA,
     &                      'MTCA',1,1,NDATA,NIO,-1)
      IQ(LMTCA + 1) = 0   ! version number
C
  999 RETURN
      END
