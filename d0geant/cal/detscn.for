      SUBROUTINE DETSCN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C  *******************************************************************
C  * Definition of hit and digitisation parameters for               *
C  * 'scintillator' ring                                             *
C- *******************************************************************
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created  17-JAN-1989   Chip Stewart
C-   Updated  27-JUN-1989   Chip Stewart  - new IUSET format for ICD
C    Originally  Written Feb 1987  A.P.White  
C    Modified Apr 1988 Z. Wolf,  New scintillator position.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INTEGER I,ISET,IDET
      CHARACTER*13 NAMES(2)
      DATA NAMES/ 'IUSET_ICD+Z','IUSET_ICD-Z'/
C----------------------------------------------------------------------
      DO 100 I=1,2
        CALL SETDET(NAMES(I),SCAL,ISET,IDET)
  100 CONTINUE
      RETURN
      END
