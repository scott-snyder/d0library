C VAX/DEC CMS REPLACEMENT HISTORY, Element MUDVER.FOR
C *4     4-NOV-1993 17:33:56 FORTNER "pq fixes to zero return"
C *3     2-NOV-1993 13:30:58 FORTNER "fix negative version number"
C *2    21-OCT-1993 08:51:26 FORTNER "add terms for scintillator"
C *1    15-SEP-1993 17:50:47 DARIEN "New MF code for 1B MUD1"
C VAX/DEC CMS REPLACEMENT HISTORY, Element MUDVER.FOR
      INTEGER FUNCTION MUDVER(IRST)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Get version number from MUHT (after MUD1)
C-
C-    Input  :  IRST   - enter -1 to reset
C-
C-    Output :  MUDVER - Version number of raw data
C-                       =-1  Bad data
C-                       = 0  no MUHT bank
C-                       = 1  collider data run 1A
C-                       = 2  collider data run 1B
C-                       =11  monte carlo data run 1A
C-                       =12  monte carlo data run 1B
C-                       =21  squeezed data run 1A
C-                       =22  squeezed data run 1B
C-
C-    Created :  2-SEP-93  M. Fortner
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IRST, MUDSAVE
      INTEGER NWR,NWP,NWM,NSR,NSP,NSM,NP,NF,NSC,NV,LPMUOF(460)
      INTEGER GZMUHT
      EXTERNAL GZMUHT
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA MUDSAVE / -1 /
C
      MUDVER = MUDSAVE
      IF (IRST.EQ.-1) FIRST=.TRUE.
C
      IF (GZMUHT().EQ.0) THEN
          MUDVER = 0
	  RETURN		! no bank found
      ENDIF
C
      CALL GTMUHT(NWR,NWP,NWM,NSR,NSP,NSM,NP,NF,NSC,NV,LPMUOF)
      IF (NV.LT.0) THEN
          MUDVER = -1
          RETURN		! error found
      ENDIF
C				! from here on down all is okay
      IF (FIRST) THEN			
	  MUDVER = 1					! 1A format
          IF (IBITS(NV,20,1).EQ.1) MUDVER=2             ! 1B format
          IF (IBITS(NV,23,1).EQ.1) MUDVER=3             ! 1B - new centroids
          IF (IBITS(NV,29,1).EQ.1) MUDVER=MUDVER+10     ! Monte Carlo
          IF (IBITS(NV,21,1).EQ.1) MUDVER=MUDVER+20     ! squeezed format
	  MUDSAVE = MUDVER	! all good data should have the same format
          FIRST = .FALSE.
      ENDIF
C
      RETURN
      END
