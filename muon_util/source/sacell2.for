      SUBROUTINE SACELL2(NSTA,NSEC,NTUB,NUMT,SPAR1,XPAR1,SPAR2,XPAR2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns geometric parameters for the SAMUS drift
C-                         tubes
C-
C-   Inputs  : NSTA - SAMUS station number
C-             NSEC - SAMUS section number
C-             NTUB - tube number
C-
C-   Outputs : NUMT - number of tubes for this input (must be 1 or 2)
C-             SPAR1 - volume parameters for the 1st tube
C-             XPAR1 - coordinate of the center of volume in global system
C-             SPAR2 - volume parameters for the 2nd tube
C-             XPAR2 - coordinate of the center of volume in global system
C-
C-   Controls: if tube volume doesnot exist or there are problems with SAMUS
C-             geometry or SAM_STPFILE then output parameters will be:
C-             NUMT = 0
C-             others = 10000.
C-
C-   Created  13-MAY-1991   Andrei Kiryunin
C-   Rewrited 27-JAN-1993   Vladimir Glebov   
C-   Modified 12/93 MF IBM/unix compatibility
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER NSTA,NSEC,NTUB,NUMT
      REAL    SPAR1(3),SPAR2(3),XPAR1(3),XPAR2(3) 
      INTEGER SAGTUB,TUBE_TYPE(2),IOK,SAGTBL,I
      REAL    RTUBE(3,2),VTUBE(3,2),RINN,EC,DLEN
      DATA RINN/1.45/, EC/4.50/
C----------------------------------------------------------------------
C
C ****  Get tube coordinates in local system
C
      NUMT=SAGTUB(NSTA,NSEC,NTUB,RTUBE,VTUBE,TUBE_TYPE)
C -- problems with tube geometry banks
      IF (NUMT.NE.1.AND.NUMT.NE.2) GOTO 900
C
C ****  Calculate parameters for the 1st tube
C
      SPAR1(1)=0.0
      SPAR1(2)=RINN
      IOK=SAGTBL(TUBE_TYPE(1),DLEN)
      IF (IOK.LT.0) GOTO 900
      SPAR1(3)=DLEN-EC
      CALL SAGTB0 (NSTA, RTUBE(1,1), VTUBE(1,1))
      XPAR1(1)=RTUBE(1,1)
      XPAR1(2)=RTUBE(2,1)
      XPAR1(3)=RTUBE(3,1) 
      IF(NUMT.EQ.1) GOTO 950
C
C ****  Calculate parameters for the 2nd tube
C
      SPAR2(1)=0.0
      SPAR2(2)=RINN
      IOK=SAGTBL(TUBE_TYPE(2),DLEN)
      IF (IOK.LT.0) GOTO 900
      CALL SAGTB0 (NSTA, RTUBE(1,2), VTUBE(1,2))
      SPAR2(3)=DLEN-EC
      XPAR2(1)=RTUBE(1,2)
      XPAR2(2)=RTUBE(2,2)
      XPAR2(3)=RTUBE(3,2) 
      GOTO 999
C
C ****  Exit when problems with SAMUS geometric parameters or SAM_STPFILE
C
  900 NUMT = 0
      CALL VFILL (SPAR1,3,10000.)
      CALL VFILL (XPAR1,3,10000.)
C -- for 2nd tube
  950 CONTINUE 
      CALL VFILL (SPAR2,3,10000.)
      CALL VFILL (XPAR2,3,10000.)
C
  999 RETURN
      END
