      SUBROUTINE GMD_DRAW_MANY_OBJECTS(NFIRST, NLAST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw Objects NFIRST to NLAST.
C-
C-   Inputs  : NFIRST [I] Object ID of first object
C-             NLAST  [I] Object ID of last object.
C-   Outputs :
C-   Controls:
C-
C-   Created  11-DEC-1991   Harrison B. Prosper
C-   Updated   6-MAY-1993   Harrison B. Prosper
C-   Updated  25-MAY-1993   Marc Paterno  Corrected FLINT complaints
C-                                        Call to GMD_DRAW_OBJECT had too many
C-                                           arguments.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NFIRST, NLAST
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:GM_4VECT.INC'
      INTEGER I, IOBJECT
C----------------------------------------------------------------------
      DO IOBJECT =  NFIRST, NLAST
        I = IOBJECT
        CALL PUOPEN
        CALL PU_GET_SEGMENT_TYPE(SEGMENT(IOBJECT), SEGTYPE)
        CALL GMD_DRAW_OBJECT(I)
        CALL PUCLOSE
        VISIBLE(IOBJECT) = .FALSE.
      ENDDO
      RETURN
      END
