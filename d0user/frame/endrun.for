       SUBROUTINE ENDRUN
C------------------------------------------------------------------
C-                                                                -
C-  Handle end of run records
C-                                                                -
C-                                                                -
C-     SDP Apr.,1987                                              -
C-                                                                -
C------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL INTAST
      INTEGER I,OUNIT,IRECTP
C                                                   
      IF(INTAST()) CALL CANMEN
      CALL USENDR
C
      IRECTP=MOD(IQ(LHEAD+1),1000)   ! find record type
      IF(IRECTP.EQ.2) THEN
C                                  ! check flags if only event records
        CALL OUT_ONLY_EVENTS(.TRUE.) ! are to be written out
        CALL EVTWOS
        CALL OUT_ONLY_EVENTS(.FALSE.) ! reset flags
      ENDIF
C     
      CALL STRHST      ! store histograms if requested
      RETURN
      END
