      SUBROUTINE CTTPH(L1ETAC,L1PHIC,IETAC,IPHIC,
     &                 CRATE,IADDRL,IADDRH,ITSOK)
C----------------------------------------------------------------------
C-
C-   CTTPH : Convert Trigger Tower (L-1 indices) to level-2 PHysics 
C-           indices.
C-
C-   Purpose and Methods : Here we convert trigger tower 2-dimensional 
C-                         indices to IETAC,IPHIC indices. As an added 
C-                         benifit the corrosponding (crate,iaddr) 
C-                         hex addresses are returned as well. Since the 
C-                         transform is not unique by nature we choose
C-                         as the condition of tranform that IADDR be
C-                         the MINIMUM address (modulo the last 6 bits)
C-                         of the ROT's in the TT.
C-
C-                                  ++++ Special Note ++++
C-
C-                         NETAL1 defines the total number of towers at TT
C-                         resolution that contribute to the L-1 trigger.
C-                         However, at this time, there is one more tower with
C-                         TT resolution but does not contribute to the
C-                         L-1 trigger. Its energy is only avialable in the
C-                         CADx banks.
C-                         NETAL11 includes this "logical" missing trigger
C-                         tower
C-
C-   Inputs  : L1ETAC, L1PHIC are the inputs.
C-
C-   Outputs : IETAC,IPHIC,CRATE,IADDR are the long sought after results.
C-
C-   Controls: None.
C-
C-   Created  19-JUL-1989   Dale Ross, MSU
C-
C----------------------------------------------------------------------
*
      IMPLICIT NONE!
*
C     Passed Variables:
*
C        Input:
               INTEGER L1ETAC,L1PHIC  ! The level-1 2-dimensional indices.
C        Output:
               INTEGER IETAC,IPHIC    ! The level-2 indices.
               INTEGER CRATE          ! The CRATE part of the hex address:
               INTEGER IADDRL,IADDRH  ! The Low and High part of the hex
                                      ! addres.
               LOGICAL  ITSOK  !  IT'S OK is .true. when the coversion is good.
*
C     Local Variables:
*
         INCLUDE 'D0$PARAMS:L1PHP.PARAMS'  ! L-1 to PHysics [this routine]
                                        ! Parameters: contains volitile
                                        ! parameters that strongly
                                        ! affect this routine, if changed.
*
         INTEGER IETACL(NROTTT),IPHICL(NROTTT)
                                    ! Physics eta, phi indexes. They are
                                    ! the list of possible conversion 
                                    ! canadates. There are only as much
                                    ! as two possibilities for each.
         INTEGER NTOTTTIN ! n total TT indices
         PARAMETER (NTOTTTIN = NETAL11)
*
*
C     ---------------------------------
C     ---Let the stage come to life!---
C     ---------------------------------
*
*
C     No. 1  List the canadates for conversion
C     ________________________________________
*
*
C     --First, convert l1phic to iphicl:
*
      IPHICL(1) = 2*L1PHIC - 1  !  This gives the the lower of the two possible
      IPHICL(2) = 2*L1PHIC      !  index values; 2*l1phic gives the other.
*
C     --Second, convert l1etac to ietacl:
*
      IF (ABS(L1ETAC) .LT. MNCTTE) THEN
           IETACL(1) = SIGN((ABS(2*L1ETAC) - 1),L1ETAC)
C                                           ! This gives the lower
C                                           !  of the two possible index values;
           IETACL(2) = SIGN(ABS(2*L1ETAC),L1ETAC) ! 2*l1etac gives the other.
         ELSE IF (ABS(L1ETAC) .LE. NTOTTTIN) THEN
           IETACL(1) = SIGN((ABS(L1ETAC) + MNCTTE-1), L1ETAC)
           IETACL(2) = 0  !  ...since there is only one possible index in
                          !  this case.
           ITSOK = .TRUE. 
         ELSE
           IETACL(1) = 0  !  For lack of knowing what to do with invalid codes.
           IETACL(2) = 0
           ITSOK = .FALSE.
      ENDIF
*
*
*
*
C     No. 2  Of the canadates find which one has the smallest Hex translation
C     _______________________________________________________________________
*
*
      IF (ITSOK) THEN
         CALL CMNMXHX(IETACL,IPHICL,IETAC,IPHIC,
     &               CRATE,IADDRL,IADDRH,ITSOK)
C             --Must remember that CMNMXHX returns the (IETAC,IPHIC) that
C               corrosponds to the *smallest* hex value in the TT.
      ENDIF

  999 RETURN
      END
