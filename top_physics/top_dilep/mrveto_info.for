      SUBROUTINE MRVETO_INFO(veto_flag)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  21-JAN-1995   Meenakshi
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER nveto,i
      PARAMETER( nveto  = 9 )
      INTEGER veto_flag(nveto)
      LOGICAL MRBS_LOSS, MICRO_BLANK, CAL_RECOVERY,  MU_HV_RECOVERY
      LOGICAL MR_VETO_HIGH, MR_VETO_LOW, MAX_LIVE, GOOD_CAL, GOOD_BEAM
      LOGICAL MRVETO
C----------------------------------------------------------------------
      do i=1,9
        veto_flag(i) = 0
      enddo
      MRBS_LOSS = mrveto('mrbs_loss')
      if (MRBS_LOSS ) then
        veto_flag(1 ) = 1
      endif
      MICRO_BLANK = mrveto('MICRO_BLANK')
      if (MICRO_BLANK ) then
        veto_flag(2 ) = 1
      endif
      CAL_RECOVERY = mrveto('CAL_RECOVERY')
      if (CAL_RECOVERY ) then
        veto_flag(3 ) = 1
      endif
      MU_HV_RECOVERY = mrveto('MU_HV_RECOVERY')
      if (MU_HV_RECOVERY ) then
        veto_flag(4 ) = 1
      endif
      MR_VETO_HIGH = mrveto('MR_VETO_HIGH')
      if (MR_VETO_HIGH ) then
        veto_flag(5 ) = 1
      endif
      MR_VETO_LOW = mrveto('MR_VETO_LOW')
      if (MR_VETO_LOW ) then
        veto_flag(6 ) = 1
      endif
      MAX_LIVE = mrveto('MAX_LIVE')
      if (MAX_LIVE ) then
        veto_flag(7 ) = 1
      endif
      GOOD_CAL = mrveto('GOOD_CAL')
      if (GOOD_CAL ) then
        veto_flag(8 ) = 1
      endif
      GOOD_BEAM = mrveto('GOOD_BEAM')
      if (GOOD_BEAM ) then
        veto_flag(9 ) = 1
      endif
C
  999 RETURN
      END
