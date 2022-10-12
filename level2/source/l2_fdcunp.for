      SUBROUTINE L2_FDCUNP(STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpack Crates and get pointers to Logical channels.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: STATUS = Set false for bad data
C-
C-   Created :  20-AUG-1990    Srini Rajagoplan
C-   Modified:  05-JUL-1991   Yi-Cheng Liu
C-                            ( for FDC )
C-   Updated  21-JAN-1994   Hailin LI
C-                          Added protection against new hardware problems
C-                          based upon Dan Claes' CDC code
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDCMAP.INC'
      INCLUDE 'D0$LINKS:IZCDD3.LINK'
      INCLUDE 'D0$INC:SECTLIST.INC' ! exists locally for now
C
      INTEGER ICRT,IDCRT,LENCRT
      INTEGER LCDD3,ADDR,CHNL,LENGTH
      INTEGER POINT,CRTID,MASK,START
      INTEGER UNUSED_CHNLS, MIN_CHNL, MAX_CHNL, GOOD_CHNL_PER_CRT
      PARAMETER ( MIN_CHNL = 8192, MAX_CHNL = 20031 )
      PARAMETER ( UNUSED_CHNLS = 32768 )  ! Unused channels flagged by bit 15
C
      LOGICAL STATUS, OK, SET_BAD_CD_FLAG
C
      data MASK / z'FFFF' /
C
      CHARACTER*80 MSG
C----------------------------------------------------------------------
C
      STATUS = .TRUE.
      DO 5 ICRT = 0,11
        IMAP(ICRT) = .FALSE.
    5 CONTINUE
C
      IF (LHEAD.EQ.0) THEN              ! Check if Top bank exists
        STATUS = .FALSE.
        GO TO 999
      ENDIF
      LCDD3 = LQ(LHEAD-IZCDD3)
      IF (LCDD3.LE.0) THEN              ! Check if CDD3 bank exists
        STATUS = .FALSE.
        GO TO 999
      ENDIF
C
      POINT = LCDD3 + IQ(LCDD3-1) - 16      ! Last data word ( Checksum )
C                                           ! in the crate trailer block.
C  Loop over all sense wire ( in terms of FADC crates ).
C
      DO 10 ICRT = 0,11
        IF (POINT.LE.IQ(LCDD3-1)+4) GO TO 999
        CRTID = IAND(IQ(POINT-2), MASK)
        IDCRT = (CRTID-5)/10
        LENCRT = IQ(POINT-3)        ! Total word count for the crate
C
C   Protect against bad data
C
        IF (LENCRT.LE.0) THEN
          WRITE(MSG,'(A,I6)')'ZERO LENGTH FOR CRATE ',CRTID
          CALL ERRMSG('Bad FDC Data','L2_FDCUNP',MSG,'W')
          OK = SET_BAD_CD_FLAG()   !NOTE: This KILLS the event
          STATUS = .FALSE.
          GOTO 999
        ENDIF

        START = POINT - LENCRT      ! Pointer to last word of the
C                                   ! previous crate.
        IF (CRTLST(IDCRT)) THEN           ! Need this crate
          IF (IMAP(IDCRT)) GOTO 20        ! Already unpacked this crate
          IMAP(IDCRT) = .TRUE.     ! Crate data found
          POINT = POINT - 4        ! Pointer to last word of the
C                                  ! crate data block.
          GOOD_CHNL_PER_CRT = 0
          DO WHILE (POINT.GT.START+4)  ! When the data is more thsn just
C                                      ! a crate header block.
            ADDR = POINT
            CHNL = IAND(ISHFT(IQ(POINT), -16), MASK) ! Logical ch number
C                                             ! where this cluster in on.
            LENGTH = IAND(IQ(POINT), MASK)  ! Channel length ( byte count
C                                           ! ,including itself )
            IF (LENGTH.LE.0) THEN
              WRITE(MSG,'(A,I6)')'ZERO LENGTH FOR CHANNEL ',CHNL
              CALL ERRMSG('Bad FDC Data','L2_FDCUNP',MSG,'W')
              OK = SET_BAD_CD_FLAG()   !NOTE: This KILLS the event
              STATUS = .FALSE.
              GOTO 999
            ENDIF
C
            IF ( CHNL .GT. UNUSED_CHNLS ) GOTO 99
            IF ( CHNL .LT. MIN_CHNL .OR. CHNL .GT. MAX_CHNL ) THEN
              WRITE(MSG,'(A,I6)')
     &            ' out of array MAP bound for channel ',CHNL
              CALL ERRMSG('Out of MAP bound','L2_FDCUNP',MSG,'W')
              GOTO 99
            ENDIF
C            
            MAP(CHNL) = ADDR           ! Store pointer to channel address
            GOOD_CHNL_PER_CRT = GOOD_CHNL_PER_CRT + 1
   99       POINT = POINT - LENGTH/4      ! go the previous cluster
          ENDDO
C
          IF ( GOOD_CHNL_PER_CRT .EQ. 0 ) THEN
            WRITE(MSG,'(A,I6)')'All channels bad for crate ',CRTID
            CALL ERRMSG('No good channels','L2_FDCUNP',MSG,'W')
            STATUS = .FALSE.
            GOTO 999
          ENDIF
C
        ENDIF
   20   CONTINUE
        POINT = START          ! pick up the pointer to the previous crate
   10 CONTINUE
C
  999 RETURN
      END
