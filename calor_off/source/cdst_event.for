      FUNCTION CDST_EVENT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : EVENT HOOK FOR DST ANALYSIS
C-
C-   Returned value  : TRUE IF AOK
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  21-NOV-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CDST_EVENT
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      LOGICAL DO_ANALYSIS
      INTEGER IER
      INCLUDE 'D0$INC:ZLINKC.INC'
      INTEGER GZCAEP
C----------------------------------------------------------------------
      IF(FIRST)THEN                     ! LOCAL INIT
        FIRST = .FALSE.
        CALL EZPICK('CDST_RCP')
        CALL EZGET('DO_ANALYSIS',DO_ANALYSIS,IER)
        CALL EZRSET
      ENDIF
      CDST_EVENT = .TRUE.
      LCAEP = GZCAEP()
      IF(LCAEP.GT.0)THEN
C
C ****  ONLY CREATE CAEH ETC if caep bank present.
C
        CALL CAEHFL        ! fill CAEH
        CALL CPTCAF        ! fill PTCAEP pointer array
        CALL CPTCTZ        ! zero PTCATE pointer array
        CALL CATEFL        ! fill CATE
        CALL CPTCTF        ! fill PTCATE pointer array
      ENDIF
C RECREATE THESE BANKS
      CALL CDST_LINK  !SETUP LINKS IN ZLINKC
      IF(DO_ANALYSIS)CALL CDST_ANL
  999 RETURN
      END
