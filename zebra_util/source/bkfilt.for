      SUBROUTINE BKFILT(LFILT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Lift FILT bank if needed
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   6-FEB-1990   Jan S. Hoftun
C-   Updated  15-JUN-1990   Jan S. Hoftun  (Rename to BKFILT like ZEBRA_UTIL) 
C-   Updated   5-OCT-1990   Jan S. Hoftun  (Increase to 4 links)
C-   Updated  29-FEB-1992   James T. Linnemann allow exactly 2 copies; add bits
C-   Updated  28-APR-1993   James T. McKinley add link for C2EM bank
C-   Updated  30-SEP-1994   James T. Linnemann  move to version 6: add I/O form 
C-
C----------------------------------------------------------------------
C=======================================================================
C
C  Bank Name : FILT
C  Author    : Serban D. Protopopescu
C  Date      : 14-NOV-1988
C  Tree description : EVENT_HEAD_TREE
C
C  Bank description : header for event reconstruction
C
C     LQ     Q/IQ
C-----------------------------------------------------------------------
C     -5    structural link to C2EM (compressed L2EM)
C     -4    structural link to FRES (filter-result)
C     -3    structural link to HSTR (history)
C     -2        "        "  to PROC (processed event)
C     -1        "        "  to HITS (event data hits)
C      0          Next   link to 
C     +1          Up     link to HEAD
C     +2          Origin link to HEAD
C.......................................................................
C             -5         Bank number
C             -4         Bank name, 'FILT'
C             -3         NL = 3
C             -2         NS = 3
C             -1         ND = 1
C              0         Status
C             +1         version number (=6)
C             +2-5       filter bits set (should be written) 
C             +6-9       filter bits actually tried
C             +10-13     filter bits actually PASSED
C             +14-17     filter bits written UNBIASED
C             +18        processing time in seconds
C             +19        0 if terminated normally
C                        1 if hit maximum processing time
C                           (+20 and +21 meaningful only if +19 = 1)
C             +20        L2 bit number being processed when time ran out
C             +21        L2 tool number being processed when time ran out
C=======================================================================
      IMPLICIT NONE
      INTEGER LFILT
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFILT.LINK'
      INTEGER IOFILT
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      IF(FIRST)THEN
C
        FIRST=.FALSE.
        CALL MZFORM('FILT','17I 1F -I',IOFILT)
      ENDIF
C----------------------------------------------------------------------
      LFILT = LQ( LHEAD - IZFILT) !currently active one
C ****  allow only 2 banks  
C... two banks allow rerunning and result comparison
C... more than two banks would make it impossible to run CAHITS_L2 and
C... FORCE CAHITS banks under the active FILT bank
C
      IF ( LFILT.GT.0 ) THEN  !see if a second one already
        IF ( LQ(LFILT).GT.0 ) GOTO 999
      ENDIF !no; zero or one.  Book another, which will be seen by GZFILT
      CALL MZBOOK(IXMAIN,LFILT,LHEAD,-IZFILT,'FILT',5,5,21,IOFILT,0)
      IQ(LFILT+1) = 6                 ! version number
  999 RETURN
      END
