      FUNCTION FDC_QUADTYPE(QUAD,HALF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return type of theta quadrant.
C-
C-   Returned value  : Theta quadrant type (1=A type, 2=B type)
C-   Inputs  : QUAD,HALF
C-
C-   Created  14-MAY-1991   Robert E. Avery
C-   Updated  20-AUG-1992   Robert E. Avery   Choose quadtype based on
C-      version of MC STP.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER FDC_QUADTYPE
C INPUT:
      INTEGER QUAD,HALF
C LOCAL:
      INTEGER QUADTYPE_DATA(0:7,0:1)
      INTEGER QUADTYPE_MC(0:7,0:1)
      INTEGER GZFGEH
      LOGICAL FIRST,MC_OLD
C
      SAVE FIRST,QUADTYPE_MC,QUADTYPE_DATA,MC_OLD
C
      DATA FIRST /.TRUE./
      DATA QUADTYPE_DATA /
     &  2,   1,   2,   1,   2,   1,   2,   1,
     &  1,   2,   1,   2,   2,   1,   2,   1 /
      DATA QUADTYPE_MC /
     &  2,   1,   2,   1,   2,   1,   2,   1,
     &  2,   1,   2,   1,   2,   1,   2,   1 /
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        LFGEH=GZFGEH()
        IF ( LFGEH .GT. 0 ) THEN
          MC_OLD = ( IC(LFGEH+10) .EQ. 9002 )
        ENDIF
        FIRST = .FALSE.
      ENDIF
C
      IF ( MC_OLD ) THEN
        FDC_QUADTYPE = QUADTYPE_MC(QUAD,HALF)
      ELSE
        FDC_QUADTYPE = QUADTYPE_DATA(QUAD,HALF)
      ENDIF
C
  999 RETURN
      END
