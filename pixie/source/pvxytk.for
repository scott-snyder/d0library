      SUBROUTINE PVXYTK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draws the Vertex chamber inner and outer radii 
C-                          and the current event 
C-
C-   Inputs  : None
C-   Outputs : 
C-   Controls: 
C-
C-   Created  24-JAN-1991, S. Hagopian
C-            basaed on routine PVVIEW by Ghita Rahal-Callot
C-   Updated   6-MAR-1991   Lupe Howell  Implementing PIXIIE using COMPACK 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER VTXSEC,IER
      REAL RMIN, RMAX
      LOGICAL EZERROR
C    
      DATA RMIN,RMAX/3.7,16.2/
C----------------------------------------------------------------------
C
C ****  Pick PIXIE RCP bank
C
      CALL EZPICK('PX_VTXDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PVXYTK','Cannot find PX_VTXDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PUGETV('VTX DRAW SECTORS',VTXSEC)
      CALL PUSETV('VTX DRAW SECTORS',0)
      CALL PUOPEN
      CALL PXCOLR('GRE')
      CALL JCIRCL(0.,0.,0.,RMIN,0)
      CALL JCIRCL(0.,0.,0.,RMAX,0)
      CALL JRCLOS
      CALL PVTSEC ( 0, 31 )
      CALL PUSETV('VTX DRAW SECTORS',VTXSEC)
      CALL EZRSET
  999 RETURN
      END
