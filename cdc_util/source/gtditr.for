      SUBROUTINE GTDITR(ITRK,TRKDAT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return CDC ISAJET track data from DITR
C-
C-   Inputs  : ITRK - track number
C-   Outputs : TRKDAT - track data
C-
C-            TRKDAT(1)        X origin
C-            TRKDAT(2)        Y origin
C-            TRKDAT(3)        Z origin
C-            TRKDAT(4)        Phi
C-            TRKDAT(5)        Theta
C-            TRKDAT(6)        P momentum
C-            TRKDAT(7)        Mass
C-            TRKDAT(8)        Isajet track number
C-            TRKDAT(9)        Association with a reconstructed track
C-                        =-999   ==> no association
C-                        >0      ==> number of the associated track
C-
C-   Created  03-MAY-1994   Norman A. Graf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER ITRK, I, LBASE, DUM
      INTEGER LDITR, GZDITR
C
      REAL    TRKDAT(9)
C----------------------------------------------------------------------
      LDITR = GZDITR(DUM)
      CALL VZERO(TRKDAT,9)
      IF ( LDITR .LE. 0 ) GO TO 999
      IF ( ITRK .GT. IQ(LDITR+1) ) GOTO 999
      LBASE = LDITR + 2 + IQ(LDITR+2)*(ITRK-1)
      DO 10 I = 1, IQ(LDITR+2)
        TRKDAT(I) = Q(LBASE+I)
   10 CONTINUE
C----------------------------------------------------------------------
  999 RETURN
      END
