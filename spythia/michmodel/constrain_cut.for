      LOGICAL FUNCTION CONSTRAIN_CUT(inum)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : checks for relations among susy parameters
C-
C-   Returned value  : True is solution should be dropped
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  22-NOV-1994   R. J. Genik II
C-   Updated  22-NOV-1994   R. J. Genik II  Note that some of the values in
C-   the common block are not what is written out, see write_dbase.f for
C-   details.
C-   Updated  24-JUN-1995   R. J. Genik II  Added in constraints described
C-   below. Also Arbitrary Cut of 50,000 on muz.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      Include 'D0$SPYTHIA$INC:VARS.INC'
      Include 'D0$SPYTHIA$INC:MASSMAT.INC'
      Include 'D0$SPYTHIA$INC:SOFT.INC'
      Integer inum
      Real*8 mxn1,mxc1
      Logical CCSym_Broken
C----------------------------------------------------------------------
      Constrain_cut = .True.
      If (gtop_too_big) return
      Constrain_Cut = .False.
      CCSym_Broken = .False.
      mxn1 = mxn(1)*eps(1)
      mxc1 = mxc(1)*eps(5)
C Cut out Grossly Huge muz values
      If (ABS(muz).gt.50000) Constrain_Cut = .True.
C Invalid theory, or non EWSB
      if ((abs(mmu).lt.0.25).or.(Mh(3).le.0)) Constrain_Cut = .True.
C Tachyons
      If (MIN(mstop(1),msbot(1),mstau(1), msl(1),msl(2)).LT.0)
     +  CONSTRAIN_CUT = .True.
C LEP Gaugino Constraint
      If (.not.((abs(mxc1).GT.45.2.and.abs(mxn1).GE.41).or.
     +  (abs(mxc1).GT.47 .and. abs(mxn1).LT.41))) Constrain_Cut =
     +  .True.
C Z1 or veL must be LSP
      If (MIN(MIN(ABS(MXN1),msl(1)),mstop(1),mstau(1),msbot(1),msl(2),
     +  abs(mxc1)).NE.MIN(ABS(MXN1),msl(1)))  Constrain_Cut = .True.
C Light Higgs Limit from LEP
      If (mh(1).lt.47) Constrain_Cut = .True.
C Selectron LEP Limit
      If (msl(2).lt.45) Constrain_Cut = .True.
C Sneutrino LEP Limit
      If (msl(1).lt.37.1) Constrain_Cut = .True.
C Charge and Color Symmetry Condition at Z scale
      If (Constrain_Cut) Return ! don't add in double failures
      If ((AtauZ*m0)**2.gt.3*(mh1sq+muz**2+msl(6)**2+msl(12)**2-2*mq(5)
     +  **2)) CCSym_Broken = .True.
      If ((AbotZ*m0)**2.gt.3*(mh1sq+muz**2+msq(6)**2+msq(12)**2-2*mq(5)
     +  **2)) CCSym_Broken = .True.
      If ((AtopZ*m0)**2.gt.3*(mh2sq+muZ**2+msq(5)**2+msq(11)**2-2*mq(5)
     +  **2)) CCSym_Broken = .True.
      If (CCSym_Broken) Then
        Constrain_Cut = .True.
        Call Write_DBase(3,inum) ! monitor bad solutions for this cut for w
                                 ! a while
      Endif
  999 RETURN
      END
