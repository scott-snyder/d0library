      SUBROUTINE GTCGEV_HEAD(NH,NV,NSCALE,NR,ND,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns CGEV header contents 
C-
C-   Inputs  : none
C-   Outputs : NH    [I]  Number of header words
C-             NV    [I]  Version Number (1)
C-             NSCALE[I]  ADC SCALE 0 (*8) OR 1 (*1)
C-             NR    [I]  Repeat Number 1,2, or 3...
C-             ND    [I]  Number of word in bank 
C-             IER   [I]  IF IER = 0 OK
C-                        -1 = no CGEV bank
C-   Controls: none
C-
C-   Created   14-APR-1992   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NH,NV,NSCALE,NR,ND,IER
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER LCGEV,GZCGEV
C----------------------------------------------------------------------
      IER = 0
      LCGEV = GZCGEV ()
      IF (LCGEV.LE.0) THEN
        IER = -1
        GOTO 999
      END IF
      NH     = IC (LCGEV+1)
      NV     = IC (LCGEV+2)
      NSCALE = IC (LCGEV+3)
      NR     = IC (LCGEV+4)
      ND     = IC (LCGEV-1)
  999 RETURN
      END
