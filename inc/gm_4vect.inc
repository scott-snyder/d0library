C----------------------------------------------------------------------
C-
C-   Created  11-DEC-1991   Harrison B. Prosper
C-   Updated   5-MAY-1993   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      INTEGER LEFT,RIGHT,CENTER,BOTTOM,TOP
      INTEGER MAXPART
      PARAMETER( MAXPART =  200 )       
      PARAMETER( LEFT   = 1 )
      PARAMETER( TOP    = 1 )
      PARAMETER( RIGHT  = 3 )
      PARAMETER( BOTTOM = 3 )
      PARAMETER( CENTER = 2 )
C
      INTEGER PARTID(MAXPART)
      INTEGER ORIGIN(MAXPART),PARENT(MAXPART)
      INTEGER SEGMENT(MAXPART),SEGTYPE(MAXPART),GRID_SEGMENT
      LOGICAL VISIBLE(MAXPART)
      INTEGER NPART,NRECO,NOBJECT
C
      REAL    P(4,MAXPART)
C
      CHARACTER*40 LIST(MAXPART)
C
      COMMON /GM_4VECT/
     &                NPART,NRECO,NOBJECT,PARTID,ORIGIN,PARENT,P,
     &                SEGMENT,SEGTYPE,VISIBLE,GRID_SEGMENT
      COMMON /GM_4VECTC/ LIST
