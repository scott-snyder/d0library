      FUNCTION CHTFIN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Supply summary from CAHITS
C-
C-   Returned value  : TRUE 
C-
C-   Controls:
C-
C-   Created  27-NOV-1991   S. Protopopescu
C-   Updated  17-MAR-1993   Joan Guida  Added hot channel finding routines 
C-   Updated  12-DEC-1995   Andrew Brandt  Activate DO_ANALYSIS switch
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CHTFIN
      LOGICAL DO_HOTSUP,DO_HOTFIND,DO_ANALYSIS
      INTEGER IER
C----------------------------------------------------------------------
      CHTFIN = .TRUE.
C
      CALL EZPICK('CAHITS_RCP')
      CALL EZERR(IER)
      IF (IER.EQ.0) THEN
        CALL EZGET('DO_HOTSUP',   DO_HOTSUP,IER)
        IF(IER.NE.0) DO_HOTSUP=.FALSE.
        CALL EZGET('DO_HOTFIND',  DO_HOTFIND,IER)
        IF(IER.NE.0) DO_HOTFIND=.FALSE.
        CALL EZGET ( 'DO_ANALYSIS', DO_ANALYSIS, IER )
        IF(IER.NE.0) DO_ANALYSIS=.FALSE.
        IF(DO_HOTSUP.AND.DO_HOTFIND) DO_HOTFIND=.FALSE.
        IF(DO_HOTFIND)CALL CHOTINFO_EOP()
        IF(DO_ANALYSIS) CALL CHTANL_SUM    ! gives summaries
      ENDIF
 999  RETURN
      END
