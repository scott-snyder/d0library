      SUBROUTINE INZSTP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       Initialize ZEBSTP (Zebra common for static parameters)
C-
C-   Created  28-OCT-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
C
C          Note that 10 words are set aside for reference links
C          but are not specifically named in the common block
C
        CALL MZSTOR (IXSTP,'/ZEBSTP/','C',FENSTP,LSTPH,ZCONS,ZCONS(10),
     &   ZCONS(10000),ENDZC)  
C
C **** IDVSTP is the 2nd division in the ZEBSTP store.
C
        IDVSTP=IXSTP+2
        CALL CONSTP                 ! construct STP headers
        FIRST=.FALSE.
      ENDIF
  999 RETURN
      END
