      SUBROUTINE PU_GET_UNIQUE_PORTS(CURRENT_PACKAGE,SCREEN_COMMAND,
     &  XMINPRT,XMAXPRT,YMINPRT,YMAXPRT,PACKAGE,
     &  ACTION_COMMAND,IDX,NPORT,TOTAL_PORTS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will reurn the number of indenpent viewports
C-   in a given combined screen.
C-
C-   Inputs  : CURRENT_PACKAGE [C*]: Current Package
C-             SCREEN_COMMAND  [C*]: Name of the screen
C-
C-   Outputs : XMINPRT        [R(*)]: Xmin viewport coordenates
C-             XMAXPRT        [R(*)]: Xmax viewport coordenates
C-             YMINPRT        [R(*)]: Ymin viewport coordenates
C-             YMAXPRT        [R(*)]: Ymax viewport coordenates
C-             PACKAGE       [C*(*)]: Packages in the screen command
C-             ACTION_COMMAND[C*(*)]: Actions in the screen command
C-             IDX            [I(*)]: Indexs of the screen comand
C-             NPORT            [ I]: Numberof independet ports
C-             TOTAL_PORTS      [ I]: Total number of ports
C-
C-   Created  31-MAY-1991   Lupe Howell
C-   Updated  19-JUN-1991   Lupe Howell  TOTAL_PORTS out parameter added 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) CURRENT_PACKAGE
      CHARACTER*(*) SCREEN_COMMAND
      REAL    XMINPRT(*),XMAXPRT(*),YMINPRT(*)
      REAL    YMAXPRT(*)
      INTEGER IDX(*),NPORT,TOTAL_PORTS
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:PIXIE.DEF'
C----------------------------------------------------------------------
      INTEGER VIEWPORTS,I
      REAL    OLDXMIN,OLDXMAX,OLDYMIN,OLDYMAX
      CHARACTER*32 PACKAGE(MAXCACT),ACTION_COMMAND(MAXCACT)
C----------------------------------------------------------------------
      NPORT = 0
      OLDXMIN = 0.0
      OLDXMAX = 0.0
      OLDYMIN = 0.0
      OLDYMAX = 0.0
C
C ****  Getting viewports and total viewports
C
      CALL PU_GET_VIEWPORT2(CURRENT_PACKAGE,SCREEN_COMMAND,-1.,1.,
     &   -1.,1.,XMINPRT,XMAXPRT,YMINPRT,YMAXPRT,PACKAGE,
     &   ACTION_COMMAND,IDX,VIEWPORTS)

      DO I = 1, VIEWPORTS
C
C ****  Check if the coordenates of the viewports do not
C ****  match.  If they don't it is an independent port
C
        IF ( (XMINPRT(I) .NE. OLDXMIN) .OR. (XMAXPRT(I) .NE. OLDXMAX)
     &   .OR.(YMINPRT(I) .NE. OLDYMIN) .OR. (YMAXPRT(I) .NE. OLDYMAX)
     &   ) THEN
          OLDXMIN = XMINPRT(I)
          OLDXMAX = XMAXPRT(I)
          OLDYMIN = YMINPRT(I)
          OLDYMAX = YMAXPRT(I)
          NPORT = NPORT + 1
        ENDIF
      ENDDO
      TOTAL_PORTS = VIEWPORTS
  999 RETURN
      END
