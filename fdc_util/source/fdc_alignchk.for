      FUNCTION FDC_ALIGNCHK()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check that the alignment bank used for
C-   reconstruction is consitent with the one currently used.
C-   If not, then change.
C-
C-   Returned value  : .TRUE. if alignment banks were actually changed,
C-                            else .FALSE.
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  17-FEB-1993   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL FDC_ALIGNCHK
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZFALH.LINK'
C
      INTEGER LFTRH, GZFTRH
      INTEGER LKFALH 
      INTEGER ALIGN_RECO ,ALIGN_STP 
      INTEGER LALIGN, LZFIND
      INTEGER RUNSAV,IDSAV
      INTEGER RUN,ID
C
      LOGICAL MCDATA 
      LOGICAL ALL_DONE, CHECK_DONE  
C
      DATA CHECK_DONE  /.FALSE./
      DATA ALL_DONE  /.FALSE./
C
C----------------------------------------------------------------------
      IF ( ALL_DONE ) GOTO 999
C
      IF ( .NOT. CHECK_DONE ) THEN
        FDC_ALIGNCHK = .FALSE.
C
        IF ( LHEAD.LE.0 ) GOTO 999
        MCDATA =  IQ(LHEAD+1) .GT. 1000
        IF (MCDATA) THEN
          ALL_DONE = .TRUE.
          CHECK_DONE  = .TRUE.
          GOTO 999
        ENDIF
C
        LFTRH = GZFTRH()
        IF ( LFTRH.LE.0 ) GOTO 999
        LKFALH = LC(LSFDC-IZFALH)
        IF ( LKFALH.LE.0 ) GOTO 999
C
        ALIGN_RECO = IQ(LFTRH+9)
        IF ( ALIGN_RECO.EQ.0  ) ALIGN_RECO = 1
C
        ALIGN_STP = IC(LKFALH-5)
C
        CHECK_DONE  = .TRUE.
        IF ( ALIGN_STP.NE.ALIGN_RECO ) THEN
C
C Inconsistent, so switch to reco alignment banks:
C
          LALIGN=LZFIND(IXSTP,LKFALH,ALIGN_RECO,-5)
          IF ( LALIGN.LE.0 ) THEN
            ALL_DONE = .TRUE.
            FDC_ALIGNCHK = .FALSE.
            GOTO 999
          ENDIF
C
          CALL ZSHUNT(IXSTP,LALIGN,LSFDC,-IZFALH,0)
          CALL FZERO_FDCPRM
          CALL FDC_SHIFT
C
          FDC_ALIGNCHK = .TRUE.
          CALL INTMSG
     &      (' FDC-Switched Alignment banks to agree with Reco.')
          CALL EVNTID(RUNSAV,IDSAV)
C
        ELSE
          ALL_DONE = .TRUE.
          FDC_ALIGNCHK = .FALSE.
        ENDIF
C
      ELSE
C
        CALL EVNTID(RUN,ID)
        IF ( RUN .NE. RUNSAV .OR. ID .NE. IDSAV ) THEN
          ALL_DONE = .TRUE.
          FDC_ALIGNCHK = .FALSE.
        ENDIF
      ENDIF
C
  999 RETURN
      END
