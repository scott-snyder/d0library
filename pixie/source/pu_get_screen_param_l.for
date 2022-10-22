      SUBROUTINE PU_GET_SCREEN_PARAM_l( IDX, PARNAM, IVAL , IER )
      IMPLICIT NONE
      INTEGER IDX
      CHARACTER*(*) PARNAM
      logical IVAL
      INTEGER IER
      call pu_get_screen_param (idx, parnam, ival, ier)
      return
      end
