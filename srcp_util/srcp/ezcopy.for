      SUBROUTINE EZCOPY(RCP_FROM,RCP_TO,LTO,IZLINK,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Copy RCP filename RCP_FROM to file name
C-   RCP_TO hanging at Link IC(LTO-IZLINK). If IZLINK = 0, it will create a
C-   Stand alone bank at LTO
C-
C-   Inputs  : RCP_FROM= bankname to copy from
C-             RCP_TO = Bankname to copy to
C-             LTO, IZLINK If IZLINK > 0, RCP_TO will be at IC(LTO-IZLINK)
C-             in /ZEBSTP/
C-   Outputs : LTO IS OUTPUT IF IZLINK =0. It will create  a standalone
C-             bank
C-             IER = Error flag
C-   Controls: IZLINK
C-
C-   Created  11-DEC-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCP_FROM,RCP_TO
      INTEGER LTO,IZLINK
      INTEGER LFROM,ID,IER
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
C----------------------------------------------------------------------
      IER = 0
      CALL EZZLOC(RCP_FROM,LFROM,ID)
      IF ( ID.EQ.0 ) THEN
        IER = 2                         ! RCP_FROM does not exist
        RETURN
      ENDIF
      IF ( IZLINK.LT.0 ) THEN
        IER = 1                         ! BAD CONTROL
        RETURN
      ELSEIF ( IZLINK.EQ.0 ) THEN       ! Create stand alone bank
        CALL MZCOPY(IDVSTP,LFROM,IDVSTP,LTO,2,' ')
      ELSE
        CALL MZCOPY(IDVSTP,LFROM,IDVSTP,LTO,-IZLINK,' ')
      ENDIF
      IER = IQUEST(1)
      IF(IER.NE.0)RETURN
      CALL EZNAME(RCP_TO,LTO,IZLINK)       ! NAME IT.
      CALL EZRNAM(RCP_TO,RCP_TO)        ! Kluge to get header done
  999 RETURN
      END
