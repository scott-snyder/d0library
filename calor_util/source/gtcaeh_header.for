      SUBROUTINE GTCAEH_HEADER(NVERSION,NREPET,NCHANNEL,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns CAEH bank header words. 
C-
C-
C-   Inputs  : NONE
C-             
C-   Outputs : NVERSION  [I]  Bank version number
C-                              = 2 for live material,
C-                              = 1002 for dead (geant)
C-             NREPET    [I]  Repetition number
C-             NCHANNEL  [I]  Number of channels             
C-             IER       [I]     Error code; 0 --- OK
C-                              -1 --- No CAEH bank address
C-                              -3 --- No CAEH dead energy bank
C-   Controls: NONE
C-   
C-   ENTRY POINT GTCAEH_HEADER_DEAD(NVERSION,NREPET,NCHANNEL,IER)
C-   
C-   Created   27-FEB-1990   W.G.D.Dharmaratna, Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER NVERSION,NREPET,NCHANNEL,IER
C
      INTEGER LCAEH,GZCAEH
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
C
      IER = 0
C
      LCAEH=GZCAEH()
      IF ( LCAEH .LE. 0 ) THEN
          IER = -1
          GOTO 999
      ENDIF
C      
      NVERSION  = IQ(LCAEH+1)
      NREPET    = IQ(LCAEH+2)
      NCHANNEL  = IQ(LCAEH+3)
      RETURN
C
      ENTRY GTCAEH_HEADER_DEAD(NVERSION,NREPET,NCHANNEL,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns CAEH (GEAN DEAD ENERGY) bank header words 
C-
C----------------------------------------------------------------------
      IER = 0
C
      LCAEH=GZCAEH()
      IF ( LCAEH .LE. 0 ) THEN
          IER = -1
          GOTO 999
      ENDIF
      LCAEH = LQ(LCAEH)
      IF ( LCAEH .LE. 0 ) THEN
          IER = -3
          GOTO 999
      ENDIF
C      
      NVERSION  = IQ(LCAEH+1)
      NREPET    = IQ(LCAEH+2)
      NCHANNEL = IQ(LCAEH+3)
  999 RETURN
      END

