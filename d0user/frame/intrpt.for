      SUBROUTINE INTRPT
C---------------------------------------------------------------------
C-                                                                   -
C-      Control flags for Interrupt Menu
C-                                                                   -
C-                       SDP Nov.,1986                               -
C-                                                                   -
C---------------------------------------------------------------------
C
      IMPLICIT NONE
      LOGICAL YES
      DATA YES/.FALSE./
C
      YES=.TRUE.
      CALL GETPAR(1,' You want an interrupt menu? [Y]:','L',YES)
      CALL SETINT(YES)
      YES=.FALSE.
      CALL GETPAR(1,' Cancel interrupt menu for each begin run? [N]:'
     &  ,'L',YES)
      RETURN
C
      ENTRY IF_CANMEN
      IF(YES) CALL CANMEN
      RETURN
      END
