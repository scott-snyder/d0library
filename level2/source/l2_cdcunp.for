      SUBROUTINE L2_CDCUNP(STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpack Crates & get pointers to Logical chnls
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: STATUS = Set false for bad data
C-
C-   Created  22-AUG-1990   Srini Rajagopalan - created as DCRUNP
C-   Updated  26-AUG-1991   D Claes - Unpack ONLY  crates for the HIT cells
C-                                    (not ALREADY unpacked for this event)
C-   Updated  06-JAN-1993   D Claes - protect against new hardware problems
C-                                    Check for zero crate and chnl lengths
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDCMAP.INC'
      INCLUDE 'D0$LINKS:IZCDD2.LINK'
      INCLUDE 'D0$LINKS:IZL2CD.LINK'
      INCLUDE 'D0$INC:SECTLIST.INC/LIST'
C
C Protection for abbreviated MAP array - Hailin
      INTEGER UNUSED_CHNLS, MIN_CHNL, MAX_CHNL, GOOD_CHNL_PER_CRT
      PARAMETER ( MIN_CHNL = 8192 , MAX_CHNL = 20031 )
      PARAMETER ( UNUSED_CHNLS = 32768 )  ! Unused channels flagged by bit 15
C
      INTEGER ICRT,LENCRT
      INTEGER LCDD2,ADDR,CHNL,LENGTH
      INTEGER POINT,CRTID,IDCRT,MASK,EVTOLD
      INTEGER PHIMIN,PHIMAX,START
      LOGICAL OK, STATUS, SET_BAD_CD_FLAG
C     PARAMETER (MASK = 'FFFF'X)
      PARAMETER (MASK = 65535)
      DATA EVTOLD/ -1 /
C
      CHARACTER*80 MSG
C----------------------------------------------------------------------
C
      STATUS = .TRUE.
      IF (LHEAD.EQ.0) THEN              ! Check if Top bank exists
        STATUS = .FALSE.
        GO TO 999
      ENDIF
C
      LCDD2 = LQ(LHEAD-IZCDD2)          ! Structural link to CDD2
      IF (LCDD2.LE.0) THEN              ! (raw data bank for CDC)
        STATUS = .FALSE.
        GO TO 999
      ENDIF
C
      IF (IQ(LHEAD+7) .NE. EVTOLD) THEN ! New event
        EVTOLD = IQ(LHEAD+7)
C
        DO 5 ICRT = 0,5                 ! Zero IMAP
          IMAP(ICRT) = .FALSE.
    5   CONTINUE
C
      ENDIF
C
      POINT = LCDD2 + IQ(LCDD2-1) - 16      ! Last data word
C
C  Loop over all Crates. (CDC has 6 crates - 4,14,24...54)
C
      DO 20 ICRT = 0,5
        IF (POINT.LE.IQ(LCDD2-1)+4) GO TO 999
C       CRTID = IAND(ISHFT(IQ(POINT-2), 0), MASK)       ! 0 offset
        CRTID = IBITS(IQ(POINT-2),0,16)        ! An eqivalent statement
C
        IDCRT = (CRTID-4)/10

        LENCRT = IQ(POINT-3)            ! crate length (word count)
        IF (LENCRT.LE.0) THEN
          WRITE(MSG,'(A,I)')'ZERO LENGTH FOR CRATE ',CRTID
          CALL ERRMSG('Bad CDC Data','L2_CDCUNP',MSG,'W')
          OK = SET_BAD_CD_FLAG()   !NOTE: This KILLS the event
          STATUS = .FALSE.
          GOTO 999
        ENDIF
        START = POINT - LENCRT
C
        IF (CRTLST(IDCRT)) THEN           ! Need this crate
          IF (IMAP(IDCRT)) GOTO 10        ! Already unpacked this crate
          IMAP(IDCRT) = .TRUE.            ! Crate data found
          POINT = POINT - 4
          GOOD_CHNL_PER_CRT = 0
          DO WHILE (POINT.GT.START+4)
            ADDR = POINT
            CHNL = IAND(ISHFT(IQ(POINT), -16), MASK)      ! Channel length
            LENGTH = IAND(IQ(POINT), MASK)
            IF (LENGTH.LE.0) THEN
              WRITE(MSG,'(A,I)')'ZERO LENGTH FOR CHANNEL ',CHNL
              CALL ERRMSG('Bad CDC Data','L2_CDCUNP',MSG,'W')
              OK = SET_BAD_CD_FLAG()   !NOTE: This KILLS the event
              STATUS = .FALSE.
              GOTO 999
            ENDIF

            IF ( CHNL .GT. UNUSED_CHNLS ) GOTO 99
            IF ( CHNL .LT. MIN_CHNL .OR. CHNL .GT. MAX_CHNL ) THEN
              WRITE(MSG,'(A,I)')
     &            ' out of array MAP bound for channel ',CHNL
              CALL ERRMSG('Out of MAP bound','L2_CDCUNP',MSG,'W')
              GOTO 99
            ENDIF
C
            IF (LENGTH.GT.4) THEN
              MAP(CHNL) = ADDR ! Store pointer to chanl address
              GOOD_CHNL_PER_CRT = GOOD_CHNL_PER_CRT + 1
            ENDIF
   99       POINT = POINT - LENGTH/4
          ENDDO

          IF ( GOOD_CHNL_PER_CRT .EQ. 0 ) THEN
            WRITE(MSG,'(A,I)')'All channels bad for crate ',CRTID
            CALL ERRMSG('No good channels','L2_CDCUNP',MSG,'W')
            STATUS = .FALSE.
            GOTO 999
          ENDIF

        ENDIF
   10   CONTINUE
        POINT = START
   20 CONTINUE
C
  999 RETURN
      END
