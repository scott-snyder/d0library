      SUBROUTINE PTRDXY_GEO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : display X-Y view of TRD geometry
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created   3-AUG-1990   Qizhong Li-Demarteau   extracted from PRTRDTK
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GRAPHF77.INC'
      CHARACTER*3  PARCOL
      INTEGER ILAY
      REAL R(4) ! RADIOUS OF TRD LAYERS
      DATA PARCOL/'GRE'/
      DATA R/17.50,28.05,38.60,49.15/
C----------------------------------------------------------------------
C
      CALL PUOPEN
C draw circle boundaries
      CALL PXCOLR(PARCOL)         
      DO 5 ILAY=1,4
    5 CALL JCIRCL(0.,0.,0.,R(ILAY),0)
      CALL JRCLOS
C
  999 RETURN
      END
