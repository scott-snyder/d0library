      FUNCTION CD_FIX()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Used by DSTFIX program to implement
C-                         available fixes at the DST level.
C-                         This routine has overall control of
C-                         TRD, CDC, and FDC fixes (VTX has no
C-                         current plans for fixes).
C-                         This supercedes the structure originally
C-                         set up by Srini Rajagopalan on 18-OCT-1994.
C-                         The corrections are now broken up by detector
C-                         instead of type of correction. Database usage
C-                         has also changed.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : "Fixed" CD banks.
C-   Controls: RCP
C-
C-   Created  25-JUL-1995   Norman A. Graf

C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CD_FIX,CD_FIX_INI
      LOGICAL FIRST,OK,TRDFIX
      LOGICAL EZERROR
      LOGICAL DO_TRD_CORR,DO_FDC_CORR,DO_CDC_CORR
C
      DATA FIRST /.TRUE./
      INTEGER IER
C
C----------------------------------------------------------------------
C
      CD_FIX = .TRUE.
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('CD_FIX_RCP')
C
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG
     &      ('NO_CD_FIX_RCP_BANK','CD_FIX','No CD_FIX_RCP bank','W')
          CALL INRCP('CD_FIX_RCP',IER)
          IF (IER.NE.0)
     &      CALL ERRMSG
     &      ('NO CD_FIX_RCP file','CD_FIX','No CD_FIX_RCP file','F')
        ENDIF
        CALL EZGET('DO_FDC_CORR',DO_FDC_CORR,IER)
        CALL EZGET('DO_TRD_CORR',DO_TRD_CORR,IER)
        CALL EZGET('DO_CDC_CORR',DO_CDC_CORR,IER)
        CALL EZRSET
      ENDIF
C
      IF(DO_FDC_CORR) CALL FDC_DSTFIX
      IF(DO_CDC_CORR) CALL CDC_FIX
      IF(DO_TRD_CORR) THEN
        OK= TRDFIX()
      ENDIF
C
  999 RETURN
C
      ENTRY CD_FIX_INI()
C
      CALL INRCP('CD_FIX_RCP',IER)    ! Read in RCP file
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG
     &      ('NO_CD_FIX_RCP','CD_FIX_INI','No CD_FIX.RCP file','W')
        CD_FIX_INI = .FALSE.
      ELSE
        CD_FIX_INI = .TRUE.
      ENDIF
C
      RETURN
      END
