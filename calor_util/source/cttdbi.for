      SUBROUTINE CTTDBI(L1ETAC,L1PHIC,L1DINX)
C----------------------------------------------------------------------
C-
C-   CTTDBI = Convert Level-1 Trigger Data Block Index
C-
C-   Purpose and Methods : ...to convert Level-1 eta, phi indices to an
C-                         equivalent index in the Level-1 trigger data
C-                         block, as spelled out in D0 note 706.
C-
C-   Inverse Routine: CDBITT
C-
C-   Inputs  : L1ETAC and L1PHIC are the level-1 indices, and...
C-
C-   Outputs : ...L1DINX is the Level-1 Data block INdeX.
C-                        (range of L1INX is [0,2*NPHIL1*NETAL1-1] )
C-
C-   Controls: None.
C-
C-   Created   2-FEB-1989   Dale A. Ross, MSU
C-
C----------------------------------------------------------------------
*
      IMPLICIT NONE!
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'  ! RA 9-20-90 Include l1 params
*
C     Passed Variables:
*
      INTEGER  L1ETAC,L1PHIC  !  Level-1 eta,phi index vars.
      INTEGER  L1DINX         !  Level-1 Data block INdeX
*
C     Local Variables:
*
      INTEGER  INTINX  !  INTermediate trigger data block INdeX
      INTEGER  PRTINX  !  PaRT of the trigger data block INdeX
      INTEGER  IAND
*
      INTINX = NETAL1*NPHIL1*(1-SIGN(1,L1ETAC))/2 + NPHIL1*ABS(L1ETAC) +
     &  (L1PHIC-1)/(NPHIL1/2)
C               NETAL1*NPHIL1 is where the neg             Add 1 if l1phic
C               eta values are stored;                        is > NPHIL1/2.
C               i.e., add NETAL1*NPHIL1 when
C               l1etac is negative.      In general, the index goes as
C                                        NPHIL1e + p (similar to what is shown
C                                        in appendix A of D0 note 706), where
C     Note: These calculations           e is L1ETAC, odd p is for IPHIC=1...NPHIL1/2
C           utilize the properties       even p is for IPHIC=17...NPHIL1. Thus for
C           of integer division,         a given L1ETAC the data block organ-
C           eg, 9/5=1.                   izes the L1PHIC in a sequece something
C                                        like: 1,17,2,18,3,19,4,....
C
      IF (L1PHIC .EQ. NPHIL1/2 .OR. L1PHIC .EQ. NPHIL1) THEN
        PRTINX = NPHIL1 - 1
      ELSE
        PRTINX   = 2*IAND(L1PHIC,(NPHIL1/2)-1)-1
      ENDIF
*
      L1DINX = INTINX + PRTINX - (NPHIL1+1)  ! the indexing starts at zero
      ! (hence subtracting 33.)
*
  999 RETURN
      END

