      SUBROUTINE CDBIPH(L1INX,IETAC,IPHIC,
     &                  CRATE,IADDRL,IADDRH,ITSOK)  
C----------------------------------------------------------------------
C-
C-   CDBIPH = Convert level-1 Data Block Index to Physics and Hex vars.
C-
C-   Purpose and Methods : ...to convert an index into a physics (ietac,iphic)
C-                         index. Since there is often more than one conversion
C-                         possibility the one that is selected is that one 
C-                         which has the minimal crate,iaddr address.
C-
C-   Inputs  : L1INX passes the level-1 trigger table index.
C-             (See D0 note 706)
C-
C-   Outputs : IETAC, IPHIC gives Readout tower (ROT) in physics indices
C-             (See D0 note 774). CRATE is the crate number; IADDRL, IADDRH
C-             are the low and high, respectively, hex address- modulo
C-             the last six bits which contain layer+ information- of
C-             RO towers. In other words, after incorporating bits 2-5
C-             into IADDR IADDRL and IADDRH can be used as loop limit
C-             indices to look at all the towers in the TT.
C-
C-   Controls: ITSOK returns .TRUE. if the conversion was without problems.
C-
C-   Created  18-JAN-1989   Dale A. Ross, MSU
C-
C----------------------------------------------------------------------
*
C >>> Variable Declarations: <<<
*
      IMPLICIT NONE!
*
C     Passed Variables:
*
         INTEGER L1INX    !  ...Level-1 INdeX in the the canadate list in the
                          !  l-1 trigger data block (this is also the
                          !  realtive address to get the adc value in 
                          !  the L-1 data block).
         INTEGER  IETAC,IPHIC ! physics index for eta and phi.
         INTEGER  CRATE,IADDRL,IADDRH  ! The hex address variables
         LOGICAL  ITSOK  !  IT'S OK is .true. when the coversion is good.
*
C     Local Variables:
*
         INCLUDE 'D0$PARAMS:L1PHP.PARAMS'  ! L-11 to PHysics [this routine]
                                    ! Parameters: contains volitile parameters
                                    ! that strongly affect this routine,
                                    ! if changed.
         INTEGER L1PHIC, L1ETAC     ! Level-1 Phi, Eta index values.
         INTEGER IETACL(NROTTT),IPHICL(NROTTT) 
                                    ! Physics eta, phi indexes. They are
                                    ! the list of possible conversion 
                                    ! canadartes. There are only two 
                                    ! possibilities for each.
*
C                    ---------------------------
C                   |>>>                     <<<|
C                   |   Begin The Main Event!   |
C                   |>>>                     <<<|
C                    ---------------------------
*
*
*
C     No. 1: Compile the list of canadates for conversion
C     ___________________________________________________
*
*
      ITSOK = .TRUE.  !  ...until proven GUILTY...
*
      CALL CDBITT(L1INX,L1ETAC,L1PHIC,ITSOK)
*
C     ...Now convert the level-1 eta, phi indices to their offline equivalence:
*
C     ......first, convert l1phic to iphicl:
*
      IPHICL(1) = 2*L1PHIC - 1  !  This gives the the lower of the two possible
      IPHICL(2) = 2*L1PHIC      !  index values; 2*l1phic gives the other.
*
C     ......second, convert l1etac to ietacl:
*
      IF (ABS(L1ETAC) .LT. MNCTTE) THEN
           IETACL(1) = SIGN((ABS(2*L1ETAC) - 1),L1ETAC)
C                                           ! This gives the lower
C                                           !  of the two possible index values;
           IETACL(2) = SIGN(ABS(2*L1ETAC),L1ETAC) ! 2*l1etac gives the other.
         ELSE IF (ABS(L1ETAC) .LE. NETAL1) THEN
           IETACL(1) = SIGN((ABS(L1ETAC) + MNCTTE-1), L1ETAC)
           IETACL(2) = 0  !  ...since there is only one possible index in
                          !  this case.
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
                                              ! Find the canadate with
                                              ! the minimum hex address
                                              ! and call it the I-ETA/PHI-C
                                              ! value sought
      ENDIF
*
  999 RETURN
      END
