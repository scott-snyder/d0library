      SUBROUTINE PLTITL(TITLE,VMIN,UMIN,UMAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Pint title in a lego plot and calaculate the dir
C-
C-   Inputs  :  TITLE - Title of the lego plot.
C-              VMIN   - Minimum vertical coordenate in the window where the 
C-                       the display is.
C-              UMIN   - Minimum horizontal coordenate in the window where the 
C-                       the display is.
C-              UMAX   - Maximum horizontal coordenate in the window.
C-
C-   Outputs :  Title of lego plot.
C-
C-   Created  28-JUL-1988   LUPE ROSAS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C-   Argument Declaration:
C-   ---------------------
      CHARACTER*20 TITLE
      REAL VMIN,UMIN,UMAX
C----------------------------------------------------------------------
C-   Local Declaration:
C-   ------------------
      INTEGER SEGNUM
      REAL X,Y,DIS
C----------------------------------------------------------------------
      CALL JUPVEC(0.,1.,0.)
      CALL JNORML(0.,0.,-1.)
      CALL PUOPEN
C  CENTERING TITLE
      DIS = UMIN + ((UMAX-UMIN)-12.)/2.
      CALL JMOVE(DIS,VMIN+1.5)
C  CHECKING SIZE OF WINDOW TO DETERMINE THE SIZE OF THE TITLE
      IF (VMIN.GE.-2.) THEN
        X=.3
        Y=.2
      ELSEIF (VMIN.GE.-5.) THEN
        X=.45
        Y=.3
      ELSEIF (VMIN.GE.-8.) THEN
        X=1.
        Y=.67
      ELSE
        X=2.0
        Y=1.33
      ENDIF
      CALL JSIZE(X,Y)
      CALL JJUST(1,2)
      CALL PXCOLR('GRE')
      CALL JFONT(5)
      CALL J1STRG(TITLE)
      CALL JRCLOS(SEGNUM)
  999 RETURN
      END
