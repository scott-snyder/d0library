      FUNCTION GZL2FTHF(HALF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FTHF 
C-
C-   Returned value  : 
C-   Inputs  : HALF
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-MAR-1989   Jeffrey Bantly
C-   Updated  15-MAR-1990   Jeffrey Bantly  use logical format 
C-   Updated  15-JUL-1991   Susan K. Blessing  Remove path checking.
C-    We only use STPC.
C-   Updated  15-JUL-1992   Yi-Cheng Liu  ( for Level2 stuff )
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INTEGER GZL2FTHF
      INTEGER HALF,LKFTMH
      INTEGER GZSL2H,LSL2H
C----------------------------------------------------------------------
      GZL2FTHF = 0      
      LSL2H = GZSL2H()
      LKFTMH = LC( LC(LSL2H-13) -3)
C                    !   L2FDC  FTMH
C
      LFTHF(HALF) = 0
      IF ( LKFTMH .NE. 0 ) LFTHF(HALF)=LC(LKFTMH-(HALF+1))
      GZL2FTHF=LFTHF(HALF)
C----------------------------------------------------------------------
  999 RETURN
      END
