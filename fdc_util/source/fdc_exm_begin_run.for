      FUNCTION FDC_EXM_BEGIN_RUN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book histograms for Examine2.  
C-                         Must do here, after the run number is known.
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Updated   7-JAN-1991   Susan K. Blessing   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IOS
      INTEGER RUN,RUNNO
      INTEGER OLDRUN
C
      LOGICAL FDC_EXM_BEGIN_RUN
C
      LOGICAL OK,FIRST
      LOGICAL FDBHST_EXM
C
      DATA FIRST/.TRUE./
      DATA OLDRUN/0/
C----------------------------------------------------------------------
C
      CALL GET_ZEBRA_IOS (IOS)
      IF (IOS.NE.0) GO TO 999
C
      RUN = RUNNO()
C
      IF (FIRST) THEN
C
        FIRST = .FALSE.
        OLDRUN = RUN
C
        OK = FDBHST_EXM()
        FDC_EXM_BEGIN_RUN = OK
C
      ELSE
        FDC_EXM_BEGIN_RUN = .TRUE.
C
C Check to see if FDC_EXM_BEGIN_RUN has already been called for this run
        IF (RUN.NE.OLDRUN) THEN
C
          OLDRUN = RUN
          CALL INTMSG(' New run started, histograms not rebooked.')
C
        END IF
      END IF
C
  999 RETURN
      END
