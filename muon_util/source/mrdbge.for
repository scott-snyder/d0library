      SUBROUTINE MRDBGE(OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read WAMUS geometry database
C-                         Include MGEH,MMAH,MSGH 
C-
C-   Inputs  : None
C-   Outputs : Replace MGEH,MMAH,MSGH
C-   Controls: \ARRAY STP_LIST at MURECO_RCP  
C-
C-   Created   6-JAN-1994   Atsushi Taketani
C-   Modified 29-mar -1994   D.Wood add EZRSET
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL  OK
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL FIRST
      INTEGER NMAXVAL           ! number of element at STP_LIST
      PARAMETER (NMAXVAL=1000)
      INTEGER ITYP(NMAXVAL),IVAL(NMAXVAL)
      INTEGER NVAL,IER
C
      INTEGER MAX_STP           ! Number of STP set        
      PARAMETER (MAX_STP=100)
      CHARACTER*50  STP_FILE(MAX_STP)
      INTEGER VSN_MGEH(MAX_STP),VSN_MMAH(MAX_STP) 
      INTEGER VSN_MSGH(MAX_STP)
      INTEGER MC_WORD(MAX_STP)
      INTEGER LOW_RUN(MAX_STP),HIGH_RUN(MAX_STP)
C
      INTEGER NSTP, WC, NCHR, ISTP,OLDISTP
C
      INTEGER IVSN(8), IER2, VMGEH,VMMAH, VMSGH
      EQUIVALENCE (VMGEH,IVSN(1)),(VMMAH,IVSN(5)),(VMSGH,IVSN(6))
      INTEGER FLAG_MUD1, IJK
C
      LOGICAL OK2
      INTEGER NRUN, RUNNO
      CHARACTER*80 MSG
C
      DATA    FIRST/.TRUE./
      DATA    ISTP/0/
      DATA    OLDISTP /0/
C----------------------------------------------------------------------
      OK = .FALSE.
C
      IF ( FIRST ) THEN        ! Get STP_LIST info.
        FIRST = .FALSE.
        CALL EZPICK('MURECO_RCP')
        CALL EZGET_VALUE_TYPE( 'STP_LIST',IVAL,ITYP,NVAL,IER)
        CALL EZRSET
        IF ( IER.NE.0 ) THEN
          MSG = 'Invalid or missing on STP_LIST at MURECO_RCP'
          CALL D0_ABORT( MSG )
        END IF
C
        WC   = 1
        NSTP = 0
C
  100   CONTINUE
        IF ( ITYP(WC).EQ.0 ) GOTO 200
        NSTP = NSTP + 1
        NCHR = ITYP(WC)-10
        CALL DHTOC( NCHR, IVAL(WC), STP_FILE(NSTP) )
  110   WC = WC + 1
        IF ( WC.GT.NMAXVAL ) THEN
          MSG = 'Invalid or missing on STP_LIST at MURECO_RCP'
          CALL D0_ABORT( MSG )
        END IF
        IF ( ITYP(WC).GE.10 ) GOTO 110
        VSN_MGEH(NSTP) = IVAL(WC)
          WC = WC + 1
        VSN_MMAH(NSTP) = IVAL(WC)
          WC = WC + 1
        VSN_MSGH(NSTP) = IVAL(WC)
          WC = WC + 1
        MC_WORD(NSTP)  = IVAL(WC)
          WC = WC + 1
        LOW_RUN(NSTP)  = IVAL(WC)
          WC = WC + 1
        HIGH_RUN(NSTP) = IVAL(WC)
          WC = WC + 1
        GOTO 100
C
  200   CONTINUE
      END IF
C
      IF ( IQ(LHEAD+1).LT.1000 ) THEN    ! Real Data
        NRUN = RUNNO()                       !Seek STP
        DO 400 ISTP=NSTP,1,-1                
          IF (MC_WORD(ISTP).EQ.0 ) GOTO 400
          IF ( NRUN.GE.LOW_RUN(ISTP).AND.
     1         NRUN.LE.HIGH_RUN(ISTP) ) GOTO 450
  400   CONTINUE
          WRITE( MSG,403) NRUN
  403     FORMAT( ' No valid MUON STPFILE for #run ', I7, 
     1             ', check STP_LIST at MURECO_RCP' )
          CALL D0_ABORT( MSG )
  450   CONTINUE
        IF ( ISTP.EQ.OLDISTP ) GOTO 998                   ! same STP
        IF ( STP_FILE(ISTP).EQ.STP_FILE(OLDISTP)) GOTO 998
        OLDISTP = ISTP
        CALL MRZCON( 'ALLGEO', STP_FILE(ISTP),0, OK2 )
        IF ( OK2 ) THEN
          WRITE(MSG,381) STP_FILE(ISTP)
  381     FORMAT( ' Success to pick up ', A50 )
          CALL INTMSG( MSG )
          WRITE(MSG,382) NRUN, LOW_RUN(ISTP), HIGH_RUN(ISTP)
  382     FORMAT( ' for #Run ', I7, ', valid range ', I7, ',',I7 )
          CALL INTMSG( MSG )
        ELSE
          WRITE(MSG,383) STP_FILE(ISTP)
  383     FORMAT( ' Invalid STP file, ', A50 )
          CALL D0_ABORT( MSG )
        END IF
C
      ELSE                               ! MC
        CALL SMUOVSN( IVSN, IER2 )
        IF ( IER2.NE.0 ) THEN                ! MUD1 not exist
          IJK = FLAG_MUD1(1)
          OK  = .TRUE.
          GOTO 999
        END IF
C
        IF ( ISTP.EQ.0 ) GOTO 310            ! 1st event
C
        IF ( VSN_MGEH(ISTP).NE.VMGEH ) GOTO 310  ! Matched previous one?
        IF ( VSN_MMAH(ISTP).NE.VMMAH ) GOTO 310
        IF ( VSN_MSGH(ISTP).NE.VMSGH ) GOTO 310
        GOTO 998                                 ! previous one is ok.
C
  310   CONTINUE                             ! Seek new STP
        DO 340 ISTP=1,NSTP
          IF ( VSN_MGEH(ISTP).NE.VMGEH ) GOTO 340
          IF ( VSN_MMAH(ISTP).NE.VMMAH ) GOTO 340
          IF ( VSN_MSGH(ISTP).NE.VMSGH ) GOTO 340
          GOTO 350                                    ! find STP
  340   CONTINUE                                      ! not find STP
        MSG = ' No matched MUON STP, check STP_LIST at MUREC_RCP'
        CALL D0_ABORT( MSG )
  350   CONTINUE
        CALL MRZCON( 'ALLGEO', STP_FILE(ISTP),0, OK2 )
        IF ( OK2 ) THEN                              ! status report
          WRITE(MSG,391) STP_FILE(ISTP)
  391     FORMAT( ' Success to pick up ', A50 )
          CALL INTMSG( MSG )
          WRITE(MSG,392) VMGEH, VMMAH, VMSGH
  392     FORMAT( ' For MGEH=',I3, ', MMAH=',I3, ', MSGH=',I3 )
          CALL INTMSG(MSG)
        ELSE
          WRITE(MSG,393) STP_FILE(ISTP)
  393     FORMAT( ' INVALID STP ', A50 )
          CALL D0_ABORT( MSG )
        END IF
      END IF
C
  998 OK = .TRUE.
C
  999 RETURN
      END
