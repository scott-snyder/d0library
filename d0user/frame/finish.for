      SUBROUTINE FINISH
C-----------------------------------------------------------
C-                                                         -
C-     do end-of-job things                                -
C-                                                         -
C-    SDP April,1987                                       -
C-                                                         -
C-----------------------------------------------------------
C
      IMPLICIT NONE
      LOGICAL FLGVAL
C-----------------------------------------------------------
C          
      CALL STDOUT     ! standard output
C
C          store histograms only if requested
      CALL SETSTR(.FALSE.)
      IF(FLGVAL('STORE_HISTOS')) CALL SETSTR(.TRUE.)
      CALL STRHST
C
      CALL USRUSM    ! user supplied summaries
      CALL QUITS
      END
