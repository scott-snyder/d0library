C CMS REPLACEMENT HISTORY, Element DGN_GET_SCALES.FOR
C *1    13-JAN-1996 13:13:01 HARRY "Update"
C CMS REPLACEMENT HISTORY, Element DGN_GET_SCALES.FOR
      SUBROUTINE DGN_GET_SCALES(MAXN,NSCALE,SCALE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return scales specified in RCP bank supplied
C-   to DGN_BEGIN. The scales are the divisors that can be appended to
C-   a field name using the syntax /N
C-
C-   Inputs  : MAXN     [I]   Maximum number of scales
C-            
C-   Outputs : NSCALE   [I]   Number of scales
C-             SCALE(*) [F]   Scale values (divisors)
C-   Controls: 
C-
C-   Created  10-JUL-1995   Harrison B. Prosper   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER MAXN, NSCALE
      REAL    SCALE(*)
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:DGNCOM.INC'
C----------------------------------------------------------------------
      INTEGER I, N
C----------------------------------------------------------------------
      NSCALE = NFIELD
      N = MIN(MAXN,NSCALE)
      DO I =  1, N
        SCALE(I) = XSCALE(I)
      ENDDO
  999 RETURN
      END
