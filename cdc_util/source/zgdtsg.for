      SUBROUTINE ZGDTSG( LAYER, NBDTSG, LNDTSG, KPDTSG )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get pointer and params of Track Segments
C-
C-   Inputs  : LAYER  [I] : Layer number
C-   Outputs : NBDTSG [I] : number of segments. 0 may indicate non existent
C-                          bank
C-             LNDTSG [I] : number of words per segment
C-             KPDTSG [I] : Pointer on first segment ( 0 non existent )
C-
C-   Created  22-SEP-1987   Olivier Callot
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDLTRK.INC'
      INTEGER LAYER, NBDTSG, LNDTSG, KPDTSG, JP,GZDTSG
C----------------------------------------------------------------------
      KPDTSG = 0
      NBDTSG = 0
      IF(LDTSG(LAYER).EQ.0) LDTSG(LAYER)=GZDTSG(LAYER)
      JP = LDTSG( LAYER )
      IF ( JP .NE. 0 ) THEN
        NBDTSG = IQ( JP+1 )
        LNDTSG = IQ( JP+2 )
        KPDTSG = JP+2
      ENDIF
  999 RETURN
      END
