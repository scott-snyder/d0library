      SUBROUTINE PLZAXS(NXMIN,NYMIN,ZMAX,ZMIN,ZMED,ZSCAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draws the up direction axis in a Lego plot
C-
C-   Inputs  :   XMIN - Minimum value for X axis
C-               YMIN - Minimum value for Y axis
C-               ZMAX - Maximum value for Z axis (up dir)
C-               ZMIN - Minimum value for Z axis
C-               ZMED - Medium value for Z axis
C-               ZSCAL- Scale for Z axis
C-   Outputs : 
C-
C-   Created   9-AUG-1988   LUPE ROSAS
C-   Updated  19-DEC-1989   Lupe Rosas Set up to use color table
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C-   Argument Declaration:
C-   ---------------------
      REAL ZMAX,ZMIN,ZMED,ZSCAL
      INTEGER NXMIN,NYMIN
C-   Local Declaration:
C-   ------------------
      INTEGER SEGNUM 
C----------------------------------------------------------------------
      CALL PUOPEN
      CALL JPINTR(0)
      CALL J3MOVE(FLOAT(NXMIN),FLOAT(NYMIN),ZMIN)
      CALL J3DRAW(FLOAT(NXMIN),FLOAT(NYMIN),ZMAX/(ZMAX*ZSCAL))
      CALL J3DRAW(FLOAT(NXMIN)-.2,FLOAT(NYMIN),ZMAX/(ZMAX*ZSCAL))
      CALL J3MOVE(FLOAT(NXMIN),FLOAT(NYMIN),ZMED/(ZMAX*ZSCAL))
      CALL J3DRAW(FLOAT(NXMIN)-.2,FLOAT(NYMIN),ZMED/(ZMAX*ZSCAL))
      CALL JRCLOS
  999 RETURN
      END
