      SUBROUTINE SAMUSER_HIST(IHOFF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Interface to a user histogram package.
C-
C-       This routine is called from F/N SAMRECO_HST.  In order to 
C-   this routine to be called,  user have to set a positive ID offset
C-   in SAMRECO.RCP with a key word, 'HIST_USER'.    For example
C-       HIST_USER   10000
C-   where 10000 is the offset for the histogram id number.   In an
C-   execution section of this routine below,  an example how to use 
C-   IHOFF is shown.
C-
C-       The HBOOK derectory has been set to that defined in the same
C-   SAMRECO.RCP file before this routine is called. Unless use wants
C-   to create histograms in his/her own directory, he/she does NOT
C-   have to set the directory in this routine.
C-
C-
C-   Inputs  : IHOFF     I     offset for histogram id numbers.
C-   Outputs : 
C-   Controls: 
C-
C-   Created  10-MAY-1991   O.Eroshin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IHOFF
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C  Book histograms for the first call.
C  ===================================
C
      IF(FIRST) THEN
         FIRST=.FALSE.
C        -- example of booking...
CCC      CALL HBOOK1(IHOFF+1,'EXAMPLE 1$',100,0.0,1.0,0.0)
CCC      CALL HBOOK1(IHOFF+2,'EXAMPLE 2$',100,0.0,1.0,0.0)
      ENDIF
C
C  Fill histograms.
C  ================
C
CCC      CALL HFILL(IHOFF+1,X,0.0,1.0)
CCC      CALL HFILL(IHOFF+2,Y,0.0,1.0)
C
  999 RETURN
      END
