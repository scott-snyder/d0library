      SUBROUTINE BKPARH(LPARH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-        Book PARH bank (note that it books a new bank for each call)
C-   
C-   Outputs : 
C-   LPARH = pointer to PARH
C-
C-   Created  28-JUN-1989   Serban D. Protopopescu
C-   Updated  10-NOV-1990   Daria Zieminska  
C-   Increased NDATA to store # of particles 
C-   Updated  20-FEB-1991   Daria Zieminska  
C-   Expanded to store roads for tracking 
C-   Updated  10-MAR-1992   Qizhong Li-Demarteau  if PARH already exists, 
C-                                                don't rebook it.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZPARH.LINK/LIST'
      INCLUDE 'D0$PARAMS:ROADS.PARAMS/LIST'  ! contains NROADMAX
      LOGICAL FIRST
      SAVE FIRST
      INTEGER LPARH,IOPARH,LPROC,GZPROC,GZHSTR
      INTEGER NDATA,NLNKS
      PARAMETER (NLNKS=8)
C
      DATA FIRST/.TRUE./
C--------------------------------------------------------------
C 
      IF(FIRST) THEN
         CALL MZFORM('PARH','10I -F',IOPARH)   ! format for PARH
         FIRST=.FALSE.
         NDATA=10+5*NROADMAX
      ENDIF
C
      CALL BKPROC(LPROC)      ! get pointer and book if not there
C
      LPARH = LQ(LPROC - IZPARH)
      IF (LPARH .GT. 0) THEN
          GOTO 999
      ENDIF
C
C   Create PARH bank 
C
      CALL MZBOOK(IXMAIN,LPARH,LPROC,-IZPARH,
     +            'PARH',NLNKS,NLNKS-1,NDATA,IOPARH,0)
C
      IQ(LPARH+1)=1             ! version number
      LQ(LPARH-NLNKS)=GZHSTR()  ! ref. link to history bank
C
  999 RETURN
      END
