      SUBROUTINE ZPULSER_CONTROL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get Pulser information from a compack driven menu
C-                         and set it.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   1-NOV-1990   Srini Rajagopalan
C-   Updated  17-MAY-1993   Susan K. Blessing  Change name of routine from
C-    ZPULSER to avoid UNIX problem with common block/subroutine name.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:COOR_INFO.INC'
      INCLUDE 'D0$INC:ZPULSER.INC'
C
      INTEGER CALL_STATUS
      CHARACTER*1 QUAD
      CHARACTER*3 OLD_DECT
      CHARACTER*12 COMAND
      CHARACTER*80 MSG
      LOGICAL IOK
C
      DATA DECT,OLD_DECT /'   ','VTX'/
C
C----------------------------------------------------------------------
C
C  Attach to Coor
C
      GOT_TARGET = .FALSE.
      CALL DO_COOR_ATTACH
      IF (.NOT.CONNECT_MADE) GO TO 999
C
C  Call Compack menu and get input
C
      CALL SPLTIT
      CALL DEFSPA(1)
    1 CONTINUE
      CALL MENUDO('Pulser Control Menu','CDPULSER',COMAND)
      IF (COMAND.NE.'EXIT') THEN
        IF (COMAND.EQ.'TARGET') THEN
          IF (DECT.NE.' ') THEN
            CALL ZDEFINE_PULSER(GOT_TARGET)
          ELSE
            CALL INTMSG(' Detector must be selected to define pulser ')
          ENDIF
        ELSE IF (COMAND.EQ.'DECT') THEN
          WRITE(MSG,10)OLD_DECT
   10     FORMAT(' Enter detector name [',A3,'] > ')
          CALL GETPAR(1,MSG,'U',DECT)
          IF (DECT(1:1).EQ.'V') THEN
            DECT = 'VTX'
          ELSE IF (DECT(1:1).EQ.'C') THEN
            DECT = 'CDC'
          ELSE IF (DECT(1:1).EQ.'F') THEN
            DECT = 'FDC'
          ELSE IF (DECT(1:1).EQ.'T') THEN
            DECT = 'TRD'
          ELSE 
            IF (DECT.NE.' ') CALL INTMSG(' Invalid detector type ')
            DECT = OLD_DECT
          ENDIF
          OLD_DECT = DECT
          WRITE(MSG,15)DECT
   15     FORMAT(' Detector selected is ',A3)
          CALL INTMSG(MSG)
        ELSE IF (COMAND.EQ.'RESET') THEN
          IF (.NOT.GOT_TARGET) THEN
            CALL INTMSG(' Error : Pulser must be selected to download ')
          ELSE
            AMPLTDA = 0                 ! Zero out all parameters
            AMPLTDB = 0
            SHHALF = 0
            SHCARD = 0
            PREAMP = 0
            POLARITY = 0
            QUADRANT = 0                
            CRTSEL = 0                  ! Deselect shaper crate
            CALL ZSET_PULSER            ! Set the Pulser with zero's
          ENDIF
        ELSE IF (COMAND.EQ.'SHOW') THEN
          CALL ZSHOW_PULSER
        ELSE IF (COMAND.EQ.'AMPLTDA') THEN
          CALL GETPAR1(' Enter Amplitude of Pulse - A > ','I',AMPLTDA)
          IF (AMPLTDA.LT.0.OR.AMPLTDA.GT.255) THEN
            CALL INTMSG(
     &        ' Amplitude out of range (0-255). Value not set ')
            AMPLTDA = 0
          ENDIF
        ELSE IF (COMAND.EQ.'AMPLTDB') THEN
          CALL GETPAR1(' Enter Amplitude of Pulse - B > ','I',AMPLTDB)
          IF (AMPLTDB.LT.0.OR.AMPLTDB.GT.255) THEN
            CALL INTMSG(
     &        ' Amplitude out of range (0-255). Value not set ')
            AMPLTDA = 0
          ENDIF
        ELSE IF (COMAND.EQ.'SHCARD') THEN
          CALL GETPAR1(' Enter Shaper Card Number > ','I',SHCARD)
          IF (SHCARD.LT.0.OR.SHCARD.GT.15) THEN
            IF (SHCARD.EQ.24) THEN
              CALL INTMSG(' All shaper cards have been selected ')
            ELSE
              CALL INTMSG(' Error : Invalid shaper card number ')
              CALL INTMSG(' Valid values are 0-15 or 24 for all cards')
              SHCARD = 24
            ENDIF
          ENDIF
        ELSE IF (COMAND.EQ.'SHHALF') THEN
          IF (SHHALF.EQ.0) THEN
            SHHALF = 1
            CALL INTMSG(
     &        ' Lower Half of Shaper crate has been selected ')
          ELSE
            SHHALF = 0
            CALL INTMSG(
     &        ' Upper Half of Shaper crate has been selected ')
          ENDIF
        ELSE IF (COMAND.EQ.'POLARITY') THEN
          IF (POLARITY.EQ.0) THEN
            POLARITY = 1
            CALL INTMSG(
     &        ' POSITIVE Test pulse polarity has been selected ')
          ELSE
            POLARITY = 0
            CALL INTMSG(
     &        ' NEGATIVE Test pulse polarity has been selected ')
          ENDIF
        ELSE IF (COMAND.EQ.'QUADRANT') THEN
          CALL GETPAR(1,' Enter Quadrant [None/Top/Bottom/All] > ','U',
     &      QUAD)
          IF (QUAD.EQ.'N') THEN
            QUADRANT = 0
          ELSE IF (QUAD.EQ.'T') THEN
            QUADRANT = 1
          ELSE IF (QUAD.EQ.'B') THEN
            QUADRANT = 2
          ELSE IF (QUAD.EQ.'A') THEN
            QUADRANT = 3
          ELSE
            CALL INTMSG(
     &        ' Invalid Quadrant selection. Quadrant not set ')
          ENDIF
        ELSE IF (COMAND.EQ.'PREAMP') THEN
          IF (PREAMP.EQ.0) THEN
            PREAMP = 1
            CALL INTMSG(' Selected to pulse directly through shapers')
          ELSE
            PREAMP = 0
            CALL INTMSG(' Selected to pulse directly through preamps ')
          ENDIF
        ELSE IF (COMAND.EQ.'SETPULSER') THEN
          IF (.NOT.GOT_TARGET) THEN
            CALL INTMSG(' Error : Pulser must be selected to download ')
          ELSE
            CRTSEL = 3                  ! Select shaper crate
            CALL ZSET_PULSER            ! Set the Pulser
          ENDIF
        ENDIF
        GO TO 1
      ENDIF
C
C  Drop ownership of pulser and detach from Coor before exiting
C
      IF (GOT_TARGET) THEN
        CALL DROP_RESOURCES(CALL_STATUS)
        IF (CALL_STATUS.NE.0) THEN
          WRITE(MSG,20)TARGET
          CALL INTMSG(MSG)
          GO TO 999
        ELSE
          WRITE(MSG,25)TARGET
          CALL INTMSG(MSG)
          GOT_TARGET = .FALSE.
        ENDIF
      ENDIF
      CALL LIB$WAIT(3.0)                ! Wait before detaching from COOR
      CALL DETACH_COOR(IOK)
      IF (.NOT.IOK) THEN
        CALL INTMSG(' Error : Unable to detach from COOR ')
      ELSE
        CALL INTMSG(' Detached from COOR sucessfully ')
      ENDIF
C
   20 FORMAT(' Unable to drop ownership of Pulser ',A12)
   25 FORMAT(' Pulser ',A12,' ownership dropped ')
C
  999 RETURN
      END
