C DEC/CMS REPLACEMENT HISTORY, Element SAHIT.FOR
C *1    29-MAY-1991 16:43:57 ABACHI "OLEG EROSHIN: New SAMUS routines"
C DEC/CMS REPLACEMENT HISTORY, Element SAHIT.FOR
      LOGICAL FUNCTION SAHIT(LINK,N)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check corresponding hit
C-
C-   Returned value  :  .TRUE.   -    if hit may be used
C-                      .FALSE.  -    in other case
C-   Inputs  : 
C-              LINK    -     pointer on bank of hit
C-              N       -     number of hit
C-   Outputs : 
C-   Controls: 
C-
C-   Created  28-MAR-1991   O.Eroshin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      INTEGER  N,LINK
C
      SAHIT = .TRUE.
      IF (N.LE.0)                       RETURN
      IF (MOD(IQ(LINK+15*(N-1)+1),2).GT.0) SAHIT = .FALSE.
C----------------------------------------------------------------------
  999 RETURN
      END
