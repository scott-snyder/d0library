      SUBROUTINE MAC_BIN_CODE_DEFAULTS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sets bin codes in common block to default
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  18-APR-1993   R. J. Genik II
C-   Updated  16-DEC-1993   R. J. Genik II  This routine is only called
C-   optionally from MAC_What_Now. The link order seems to be important
C-   here due to that Data statement. if we put the data staement in the
C-   include file, linking takes much longer... Calling this routine eats
C-   up several seconds.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      Include 'D0$INC:MAC_Global.inc'
      INTEGER I_count
C----------------------------------------------------------------------
c      DATA Bin_Code_Name/nbins_max*'Unlabeled bin', 
c     +  2*'Unlabeled bin'/
      Do 100 I_count = -1, nbins_max
        Bin_Code_Name(I_Count) = 'Unlabeled bin'
  100 Continue
  999 RETURN
      END
