      SUBROUTINE MU_CHK_MAC_FLAGS(SKIP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Loops over all MAC's checking their flags.
C-
C-   Inputs  : None
C-   Outputs : SKIP - .TRUE. if event should not be processed because at
C-                     least one MAC was not read out (Too many hits)
C-
C-   Controls: 
C-
C-   Created   9-APR-1993   Guilherme Lima, Gilvan Alves
C-   Updated  22-OCT-1993   Guilherme,Jussara   Upgrade for MUD1_1B format 
C-   Updated  22-MAR-2004   sss - compile with g77
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL SKIP
      INTEGER I,IER
      CHARACTER*80 MSG,MANYMSG

C-- Coarse centroid variables
      INTEGER MODNO,MODID,MODS(200),ISECT
      INTEGER ICRS(16),ITRUNC,ICENFL

      CHARACTER*80 STRING
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
      LOGICAL WAMCHK,SAMCHK
      DATA WAMCHK,SAMCHK /.FALSE.,.FALSE./
      DATA MANYMSG /'MUSIM: Too many hits, module not read out'/
C--------------------------------------------

C__ Read in skip falgs
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('MUSIM_RCP')
        CALL EZERR(IER)     ! Check if error
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' CANNOT EZPICK MUSIM.RCP',
     &			'MU_CHK_FLAGS',STRING,'S')
          GOTO 999
        ENDIF

        CALL EZGET('WAM_CHK',WAMCHK,IER)
        CALL EZGET('SAM_CHK',SAMCHK,IER)
        CALL EZRSET()
      ENDIF

C__  Initialize variables
      SKIP=.FALSE.

C__  Return if not to skip
      IF(.NOT.WAMCHK.AND..NOT.SAMCHK) RETURN 

C-- Get the module numbers
      CALL MU_TRIG_SECT(MODNO,MODID,ISECT,MODS)

C__ Check wamus
      IF( WAMCHK ) THEN
        DO I = 1 ,164
          MODNO=MODS(I)
          CALL MUMCRS(MODNO,ITRUNC,ICENFL,ICRS)
          IF(ITRUNC.AND.ICENFL.GT.0) THEN
            SKIP=.TRUE.
	    WRITE(MSG,125) MODNO,ITRUNC
            CALL INTMSG(MSG)
            WRITE(MSG,124) MODNO
            CALL ERRMSG(MSG,'MAC_FLAG',MANYMSG,'I')
          ENDIF
        ENDDO
      ENDIF

C__ Check samus
      IF ( SAMCHK ) THEN
        DO I = 165 ,200
          MODNO=MODS(I)
          CALL MUMCRS(MODNO,ITRUNC,ICENFL,ICRS)
          IF(ITRUNC.AND.ICENFL.GT.0) THEN
            SKIP=.TRUE.
	    WRITE(MSG,125) MODNO,ITRUNC
            CALL INTMSG(MSG)
            WRITE(MSG,124) MODNO
            CALL ERRMSG(MSG,'MAC_FLAG',MANYMSG,'I')
          ENDIF
        ENDDO
      ENDIF

  999 RETURN

  124 FORMAT('TOO_MANY_HITS_',I3)
  125 FORMAT(' Too many hits flag set in module ',I3,
     &              ', (flag=',I2,')')

      END
