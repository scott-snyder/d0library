      SUBROUTINE BLDCBD_D0
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Drop existing DCBD bank and create new DCBD 
C-                         structure containing delay line nonlinearity
C-                         correction function coefficients 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  15-SEP-1992   Domenico Pizzuto
C-
C----------------------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDCBD.LINK'
      
      INTEGER MPDCBD (5),LAYER,LDTMD,GZDTMD,LDCBD

      DATA MPDCBD /0,0,0,133,0/

C- Drop existing DCBD bank and create new DCBD structure
      CALL UCTOH ('DCBD',MPDCBD(1),4,4)
      CALL MZFORM ('DCBD','5I -F',MPDCBD (5))
       DO 10 LAYER = 0, 3
        LDTMD = GZDTMD (LAYER)
        IF (LDTMD.EQ.0) GOTO 10
        LDCBD = LC (LDTMD-1)
        IF (LDCBD.NE.0) CALL MZDROP (IXSTP,LDCBD,' ')
C- Create bank DCBD hanging from DTMD via link -1
        CALL MZLIFT (IDVSTP,LDCBD,LDTMD,-IZDCBD,MPDCBD,0)
        IC (LDCBD+4) = 2          !# words per delay line
        IC (LDCBD+5) = 2          !# of delay lines per sector
   10  CONTINUE

  999 RETURN
      END
