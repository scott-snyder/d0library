C VAX/DEC CMS REPLACEMENT HISTORY, Element ZVFIT.INC
C *1    24-NOV-1993 02:53:20 SRINI "Common block for constrained vertex fit"
C VAX/DEC CMS REPLACEMENT HISTORY, Element ZVFIT.INC
C----------------------------------------------------------------------
C-
C-   Created  23-AUG-1993   Srini Rajagopalan
C-
C----------------------------------------------------------------------
C
C GLobal variables accross all vertices
C
      INTEGER MAX_VERT              
      PARAMETER (MAX_VERT=10)   
C
      INTEGER NVERT                 
      REAL ZVERT(MAX_VERT),ZERR(MAX_VERT)
      REAL ZCHI(MAX_VERT),ZENT(MAX_VERT)
C
C Local variables, valid for current vertex only.
C
      INTEGER MAX_TRACK             
      PARAMETER (MAX_TRACK=100)
C
      INTEGER PID(MAX_TRACK),TPID(MAX_TRACK)
      REAL SLOPE(MAX_TRACK),ERR_SLOPE(MAX_TRACK),ERR_INT(MAX_TRACK)
      REAL TSLOPE(MAX_TRACK),TERR_SLOPE(MAX_TRACK),TERR_INT(MAX_TRACK)
      REAL SX(MAX_TRACK),SY(MAX_TRACK),SXX(MAX_TRACK),SXY(MAX_TRACK)
      REAL SWT(MAX_TRACK),INDEX(MAX_TRACK)
C
      COMMON /ZVFIT/ NVERT,ZVERT,ZERR,ZCHI,ZENT,
     &               PID,TPID,INDEX,
     &               SLOPE,ERR_SLOPE,ERR_INT,
     &               TSLOPE,TERR_SLOPE,TERR_INT,
     &               SX,SY,SXX,SXY,SWT
C
C--------------------------------------------------------------------------
C NVERT = Number of vertices found .LE. MAX_VERT
C ZVERT = Z position of each vertex
C ZERR  = Error of Z for each vertex
C ZCHI  = Global Chisquare of fit for each vertex
C ZENT  = Number of tracks used for each vertex
C
C NTRACK = Number of tracks used for current vertex .LE. MAX_TRACK
C PID    = Track ID number for all tracks (as in DTRK)
C SLOPE  = Slope of all above tracks
C ERR_SLOPE = Error in slope measurement
C ERR_INT   = Error in intercept measurement
C--------------------------------------------------------------------------
