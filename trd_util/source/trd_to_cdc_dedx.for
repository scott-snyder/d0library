      SUBROUTINE TRD_TO_CDC_DEDX
     &  (LTRDT,ECDC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : gets the CDC dE/dx corresponding to a TRD track
C-
C-   Inputs  : LTRDT    integer            link to TRDT
C-
C-   Outputs : ECDC     real               dE/dx in CDC
C                                            
C-   Controls: none
C-
C-   Created  26-OCT-1994   Jean-Francois LEBRAT
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:TRD_NWORD.INC'
      INTEGER LTRDT,LZTRK,LDTRK
      REAL ECDC
C
      ECDC=0.
C
      LZTRK=LQ(LTRDT-4)
        IF (LZTRK.GT.0) THEN
          LDTRK=LQ(LZTRK-7)        
          IF (LDTRK.GT.0) THEN
             ECDC=Q(LDTRK+20)
          ELSE
             CALL ERRMSG
     &      (' TRD_TO_CDC_DEDX','TRD_TO_CDC_DEDX',
     &      'Bank DTRK not found','W')
          ENDIF
        ELSE
        CALL ERRMSG
     &    (' TRD_TO_CDC_DEDX','TRD_TO_CDC_DEDX',
     &    'Bank ZTRK not found','W')
        ENDIF
C
      END
