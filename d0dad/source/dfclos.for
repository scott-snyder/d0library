      SUBROUTINE DFCLOS(ILUN,IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Close a d0dad file
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  11-Jan-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:d0dadcom.inc'
      INTEGER ILUN,IERR,OUT_REC
C-----------------------------------------------------------------------
      IERR=0
      CALL DFLSET(ILUN,IERR)
      IF( IQ(LDFHD+NDDF+JXXVER).LT.106 ) THEN ! Cern I/O versions
        CALL DFCLOS_OLD(ILUN,IERR)
      ELSE
        IF( IQ(LDFHD+JDFDIRT).NE.0 .AND. IQ(LDFHD+JDFIOFF).GT.0 ) THEN
          CALL VZERO(DF_IOREC,JPRECDF)
          CALL UCOPY(IQ(LDFHD+NDDF+1),DF_IOREC,JDFHED)
          WRITE(ILUN,REC=1,ERR=997) DF_IOREC          
          OUT_REC = (JRECDF*(IQ(LDFHD+JDFIOFF)-1))/JPRECDF+1
          CALL UCOPY(IQ(LDFHD+NDDF+JDFHED+1),DF_IOREC,JPRECDF)
          WRITE(ILUN,REC=OUT_REC+1,ERR=998) DF_IOREC
        ENDIF
        CLOSE(ILUN)
      ENDIF
  999 RETURN
C
 997  CONTINUE
      IERR = -2
      RETURN
C
 998  CONTINUE
      IF( LDDBG.GT.10 ) THEN
        WRITE(D0DAD_ERRTXT,9001) OUT_REC
 9001   FORMAT(' Error writing record',I5,'.')
        CALL ERRMSG('WRITE_ERROR','DFCLOS',D0DAD_ERRTXT,'W') 
      ENDIF
      IERR = -1
      RETURN
      END
