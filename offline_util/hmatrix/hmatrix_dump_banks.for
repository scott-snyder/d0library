      SUBROUTINE HMATRIX_DUMP_BANKS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DUMP HMATRIX BANKS OF INTEREST
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  23-DEC-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER DMPUNI,DUNIT
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
C----------------------------------------------------------------------
      DUNIT = DMPUNI()
      WRITE(DUNIT,1)IC(LHMTR+2)
    1 FORMAT(' NUMBER OF EVENTS used in calculating HMATRIX ',I8)
      CALL PRAVER(DUNIT,LAVER,0,'ALL',0)
      CALL PREMAT(DUNIT,LEMAT,0,'ALL',0)
      CALL PRHMAT(DUNIT,LHMAT,0,'ALL',0)
      CALL PRHVIS(DUNIT,LHVIS,0,'ALL',0)
      CALL PRHINV(DUNIT,LHINV,0,'ALL',0)
      CALL PREIGN(DUNIT,LEIGN,0,'ALL',0)
      CALL PRUMAT(DUNIT,LUMAT,0,'ALL',0)
  999 RETURN
      END
