      SUBROUTINE GTCAEP_HEADER(NVERSION,NREPET,NCHANNEL,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns CAEP bank header words. 
C-
C-
C-   Inputs  : NONE
C-             
C-   Outputs : NVERSION  [I]  Bank version number
C-                              = 2 for live material,
C-                              = 1002 for dead (geant)
C-             NREPET    [I]  Repetition number
C-             NCHANNEL  [I]  Number of channels             
C-             IER      [I]     Error code; 0 --- OK
C-                              -1 --- Wrong bank address
C-                              -3 --- No CAEP dead energy bank
C-   Controls: NONE
C-   
C-   ENTRY POINT GTCAEP_HEADER_DEAD(NVERSION,NREPET,NCHANNEL,IER)
C-   
C-   Created   22-FEB-1990   W.G.D.Dharmaratna, Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER NVERSION,NREPET,NCHANNEL,IER
C
      INTEGER LCAEP,GZCAEP
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
C
      IER = 0
C
      LCAEP=GZCAEP()
      IF ( LCAEP .LE. 0 ) THEN
          IER = -1
          GOTO 999
      ENDIF
C      
      NVERSION  = IQ(LCAEP+1)
      NREPET    = IQ(LCAEP+2)
      NCHANNEL = IQ(LCAEP+3)
      RETURN
C
      ENTRY GTCAEP_HEADER_DEAD(NVERSION,NREPET,NCHANNEL,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns CAEP (GEAN DEAD ENERGY) bank header words 
C-
C----------------------------------------------------------------------
      IER = 0
C
      LCAEP=GZCAEP()
      IF ( LCAEP .LE. 0 ) THEN
          IER = -1
          GOTO 999
      ENDIF
      LCAEP = LQ(LCAEP)
      IF ( LCAEP .LE. 0 ) THEN
          IER = -3
          GOTO 999
      ENDIF
C      
      NVERSION  = IQ(LCAEP+1)
      NREPET    = IQ(LCAEP+2)
      NCHANNEL = IQ(LCAEP+3)
  999 RETURN
      END

