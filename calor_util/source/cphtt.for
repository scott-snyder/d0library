       SUBROUTINE CPHTT(IETAC,IPHIC,L1ETAC,L1PHIC,ITSOK)
C----------------------------------------------------------------------
C-
C-   CPHTT = Convert PHysics (IETAC,IPHIC) to Trigger Tower (L1ETAC,L1PHIC)
C-           indices.
C-
C-   Purpose and Methods : This routine converts ofline IETAC,IPHIC index
C-                         pairs to level-1 eta,phi trigger tower pairs.
C-                         From the view of the code the mapping is straight
C-                         forward for IETAC thru 32. After IETAC=32 the
C-                         mapping becomes more special case like. Because of
C-                         this the code does not depend on parameters and is
C-                         hard-wired.
C-
C-                                       +++IMPORTANT NOTE+++
C-
C-                         IETAC=37 is a ROT with TT resolution, but it does
C-                         not contribute to the L-1 trigger. If this tower
C-                         were to be a part of the L-1 map it would corrospond
C-                         to L1ETAC=21, AND THAT VALUE IS RETURNED, THOUGH IT
C-                         IS NOT A VALID TRIGGER TOWER
C-
C-   Inputs  : IETAC, and IPHIC are supplied and are susquently converted...
C-
C-   Outputs : ...to L1ETAC and L1PHIC, the L-1 TT pair. ITSOK returns 
C-             .TRUE. if the conversion is without error; otherwise .FALSE.
C-
C-   Controls: None.
C-
C-   Created  31-JAN-1989   Dale A. Ross, MSU
C-
C----------------------------------------------------------------------
*
      IMPLICIT NONE!
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
*
C     Passed Variables:
*
        INTEGER   IETAC,IPHIC  !  offline eta,phi indexes
        LOGICAL   ITSOK        !  flags .TRUE. if conversion is w/o error.
        INTEGER   L1ETAC,L1PHIC  !  L-1 ETA,PHI indexes
*
C     Local Variables:
*
C       (seems to be none)
*
C     First: Convert IETAC to L1ETAC:
C     ______________________________
*
      ITSOK = .TRUE.
      IF (IETAC .EQ. 0) THEN
        ITSOK = .FALSE.
        GOTO 999
      ENDIF
      IF (ABS(IETAC) .LE. 32) THEN ! The first coarse cal tower:
          L1ETAC = SIGN((ABS(IETAC)+1),IETAC)/2
        ELSEIF (ABS(IETAC) .LE. NETAL) THEN ! The last ROT in 1st BLS that has
          L1ETAC = SIGN((ABS(IETAC)-16),IETAC) ! coarse towers.
        ELSE
          L1ETAC = 0  ! for lack of knowing what to do with invalid codes
          ITSOK  = .FALSE.
      ENDIF
*
C     Second: Convert IPHIC to L1PHIC:
C     _______________________________
*
      L1PHIC = (IPHIC+1)/2
*
C     ----A'a'and T'hats all folks!
*
  999 RETURN
      END
