      FUNCTION GET_BINF_BANK ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Grabs the BINF rcp bank from the BERD zeb bank and
C-   copies it to the rcp bank
C-
C-   Return Value : True if found.
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  23-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL GET_BINF_BANK
      INTEGER RUNNO, GZBERD
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZBERD.LINK'
      INTEGER IZBINF                    ! offset of binf bank
      PARAMETER (IZBINF=17)
      INTEGER LBERD, LBINF              ! BANK POINTER
      CHARACTER*32 FILENAME             ! LOGGER RCP FILE
      CHARACTER*80 EZERRMSG             ! HOLD EZ ERROR MESSASGE
      LOGICAL FLGVAL,EZERR
      INTEGER IER
C----------------------------------------------------------------------
C
C ****  Try to get rcp bank from logger rcp file or zebra bank
C
      IF (FLGVAL('DAQ_DATA')) THEN      ! ONLINE LOOK FOR LOGGER RCP file
C
C ****  drop binf bank to fetch new one
C
        CALL EZDROP('BINF')
        WRITE (FILENAME, 1000) RUNNO()  ! build filename
        CALL INRCP(FILENAME, IER)   ! read it
        IF (IER .NE. 0) THEN        ! could not read it
          CALL ERRMSG('CAN''T READ LOGGER RCP FILE',
     &                  'GET_BINF_BANK',FILENAME, 'W')
          GET_BINF_BANK = .FALSE.    ! could not fetch file
        ELSE                        ! read it
          CALL EZPICK('BINF')
          IF (EZERR(IER) ) THEN           ! could not do it
            CALL ERRMSG('EZPICK','GET_BINF_BANK',
     &                    'COULD NOT PICK BINF BANK','W')
            GET_BINF_BANK = .FALSE.
          ENDIF
          CALL EZRSET
          GET_BINF_BANK = .TRUE.
        ENDIF
      ELSE    ! look in data file assume data file
C
C ****  drop binf bank to fetch new one
C
        CALL EZDROP('BINF')
        LBERD = LQ(LHEADR-IZBERD)             ! find berd bank
        IF (LBERD .EQ. 0) THEN
          CALL ERRMSG('NO BERD BANK', 'GET_BINF_BANK', ' ', 'W')
        ELSE
          LBINF = LQ(LBERD-IZBINF)        ! Point at binf bank
          IF(LBINF.LE.0) THEN
            CALL ERRMSG('NO BINF BANK', 'GET_BINF_BANK', ' ', 'w')
          ELSE
            CALL EZMRCP(LBINF,IER)        ! copy it to rcp
            IF(IER.NE.0)
     &        CALL ERRMSG('CAN''T COPY BINF BANK', 'GET_BINF_BANK',
     &                  ' ', 'w')
            CALL EZPICK('BINF')
            IF (EZERR(IER)) THEN           ! could not do it
              CALL ERRMSG('EZPICK','GET_BINF_BANK',
     &          'COULD NOT PICK BINF BANK','W')
            ELSE
              CALL EZRSET
              GET_BINF_BANK = .TRUE.
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      RETURN
 1000 FORMAT('LOGGER$BRD:RCP_RINFO_', I7.7, '.DAT')
      END
