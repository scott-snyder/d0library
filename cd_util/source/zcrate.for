      SUBROUTINE ZCRATE(ICDD,ICRATE,ICARD,NID,CRATEID,TASK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get the crate ids from a particular CDDn bank
C-                         (task=1) or get the length of data on each card
C-                         in a chosen crate (task 2) or get the logical 
C-                         channel addresses associated with a particular 
C-                         card in a crate (task=3).
C-
C-   Inputs  : ICDD       = CDD bank to be examined, 0=all four CDDn banks
C-             ICRATE     = (task 2 only), Crate ID of crate to be examined
C-             ICARD      = (task 2 only), Card in crate ICRATE
C-   Outputs : NID        = Number of IDs to be output, crates or addresses
C-                          NID=-1  LENGTH of a channel is LT 4
C-                          NID=-2  LENGTH of a channel is GT 512
C-             CRATEID(*,1)= Crate IDs (task 1)
C-                         = Length of Card data (task 2) 
C-                         = Log Cha Addrs (task 3)
C-             CRATEID(*,2)= Number of cards in crate (task 1 only)
C-   Controls: TASK       = 1=get crate ids, 2=get card lengs, 3=get card LCAs
C-
C-   Created  28-SEP-1990   Jeffrey Bantly
C-   Updated   9-NOV-1990   Susan K. Blessing  reverse order of channels
C-                          within a card for TASK=3
C-   Updated  20-NOV-1990   Susan K. Blessing  Add check that LENGTH isn't
C-                          longer than number of digitizations (hard coded
C-                          to be 512)
C-   Updated  14-JAN-1991   Jeffrey Bantly  implement CDD bank version numbers,
C-                                          add version 4 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ICDD, ICRATE, ICARD, NID, CRATEID(100,2), TASK
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCDD1.LINK'
      INCLUDE 'D0$LINKS:IZCDD2.LINK'
      INCLUDE 'D0$LINKS:IZCDD3.LINK'
      INCLUDE 'D0$LINKS:IZCDD4.LINK'
C
C      INTEGER CH_MIN,CH_MAX,MAXBIN
C      PARAMETER (CH_MIN = 0)
C      PARAMETER (CH_MAX = 2047)
C      PARAMETER (MAXBIN = 500)
      INTEGER NDATA,HLENG,START,FRONT
      INTEGER POINT,ADDR,CHNL,FADCCH,CDD_BANK
      INTEGER LENGTH,HITLEN,HITADR,NHIT,LCDDN
      INTEGER INDEX,IPOINT,CHLEN,END,LOGCH,BIN
      INTEGER BEGCDD,ENDCDD,TOP_FRONT,NEXT_START
      INTEGER MASK8,MASK16,ERROR,IADR,DCRT,ITMP,TCARD
      INTEGER VERSION,VTYPE,IERR
      PARAMETER (MASK16 = 65535)        !  'FFFF'X
      PARAMETER (MASK8 =  255)          !  'FF'X
C
      CHARACTER*80 MESG
C----------------------------------------------------------------------
C
      CALL VZERO(CRATEID(1,1),200)
      NID = 0
      IF (TASK .EQ. 1) THEN
C
        BEGCDD=ICDD
        ENDCDD=ICDD
        IF(ICDD.EQ.0) THEN
          BEGCDD=1
          ENDCDD=4
        ENDIF
C
        DO 10 CDD_BANK=BEGCDD,ENDCDD
C
          IF (CDD_BANK.EQ.1) THEN
            LCDDN = LQ(LHEAD-IZCDD1)
          ELSE IF (CDD_BANK.EQ.2) THEN
            LCDDN = LQ(LHEAD-IZCDD2)
          ELSE IF (CDD_BANK.EQ.3) THEN
            LCDDN = LQ(LHEAD-IZCDD3)
          ELSE IF (CDD_BANK.EQ.4) THEN
            LCDDN = LQ(LHEAD-IZCDD4)
          ELSE
            GO TO 10
          ENDIF
          IF (LCDDN.EQ.0) GO TO 10
C
C   find the type of the raw data format
C
          CALL ZRD_VERSION(CDD_BANK, VERSION, VTYPE, IERR)
C
          NDATA = IQ(LCDDN - 1)
          HLENG = IQ(LCDDN + 1)
          START = LCDDN + NDATA - 16      ! Last data word of CDD bank
          TOP_FRONT = LCDDN + HLENG + 1   ! First data word of CDD bank
          FRONT = TOP_FRONT
C
          POINT = START
    1     CONTINUE                        ! Find Crate data.
          DCRT = IAND(IQ(POINT-2),MASK16)
          NID = NID + 1
          CRATEID(NID,1) = DCRT           ! Crate ID
          IF(VERSION.EQ.3) THEN           ! Set beginning of next crate 
            NEXT_START = POINT - IQ(POINT-3) - 1
          ELSE
            NEXT_START = POINT - IQ(POINT-3)
          ENDIF
          FRONT = POINT - IQ(POINT-3) + (HLENG + 1) + 1 ! First data
          ! word of current crate
          POINT = POINT - 4
C
C  Move backwards through CDDn bank, extracting information on channel
C  address, length and number of hits, until crate has been searched.
C
          DO WHILE (POINT.GT.FRONT)
C
C  Extract channel number and channel length
C
            ADDR = POINT
            CHNL = IAND(ISHFT(IQ(POINT), -16), MASK16)
C            CALL NUM_TO_ADDR(FADCCH,CHNL,2)
            LENGTH = IAND(IQ(POINT), MASK16)
C
C  Check if Channel Length is > 3, else issue a warning and return
C
            IF (LENGTH.LE.3) THEN
              POINT = LCDDN
C              WRITE(MESG,1010)LENGTH
C              CALL INTMSG(MESG)
              NID = -1
              GO TO 10
            ENDIF
C
C  Check if Channel Length is < 512, else issue a warning and return
C
            IF (LENGTH.GT.512) THEN
              POINT = LCDDN
C              WRITE(MESG,1010)LENGTH
C              CALL INTMSG(MESG)
              NID = -2
              GO TO 10
            ENDIF
C
C  Check if channel is within bounds
C
C            IF (FADCCH.GE.CH_MIN.AND.FADCCH.LE.CH_MAX) THEN
            CRATEID(NID,2) = CRATEID(NID,2) + 1
C            ENDIF
C
C  Increment pointer and go on
C
            POINT = POINT - LENGTH/4
          ENDDO
          CRATEID(NID,2) = ( (CRATEID(NID,2)-1) / 16 ) + 1
          POINT = NEXT_START
          IF(POINT .LE. TOP_FRONT) GOTO 10
          GOTO 1
   10   CONTINUE
C
C ****  Task 2 : Get length of data on each card in chosen crate.
C ****  Task 3 : Get Logical Channel Addresses for given Crate, Card
C
      ELSEIF( TASK .EQ. 2 .OR. TASK .EQ. 3 ) THEN
        IF (ICDD.EQ.1) THEN
          LCDDN = LQ(LHEAD-IZCDD1)
        ELSE IF (ICDD.EQ.2) THEN
          LCDDN = LQ(LHEAD-IZCDD2)
        ELSE IF (ICDD.EQ.3) THEN
          LCDDN = LQ(LHEAD-IZCDD3)
        ELSE IF (ICDD.EQ.4) THEN
          LCDDN = LQ(LHEAD-IZCDD4)
        ELSE
          GO TO 999
        ENDIF
        IF (LCDDN.EQ.0) GO TO 999
        CALL ZRD_VERSION(ICDD, VERSION, VTYPE, IERR)
C
        NDATA = IQ(LCDDN - 1)
        HLENG = IQ(LCDDN + 1)
        START = LCDDN + NDATA - 16      ! Last data word
        FRONT = LCDDN + HLENG + 1
C
        POINT = START
    2   CONTINUE                        ! Find Crate data.
        DCRT = IAND(IQ(POINT-2),MASK16)
        IF (DCRT.EQ.ICRATE) THEN
          FRONT = POINT - IQ(POINT-3) + (HLENG + 1) + 1
          POINT = POINT - 4
        ELSE
          IF(VERSION.EQ.3) THEN           ! Set beginning of next crate 
            POINT = POINT - IQ(POINT-3) - 1
          ELSE
            POINT = POINT - IQ(POINT-3) 
          ENDIF
          IF (POINT.LE.FRONT) GO TO 999
          GO TO 2
        ENDIF
C
C  Move backwards through CDDn bank, extracting information on channel
C  address, length and number of hits, until crate has been searched.
C
        ITMP = 257
        DO WHILE (POINT.GT.FRONT)
C
C  Extract channel number and channel length
C
          ADDR = POINT
          CHNL = IAND(ISHFT(IQ(POINT), -16), MASK16)
C          CALL NUM_TO_ADDR(FADCCH,CHNL,2)
          LENGTH = IAND(IQ(POINT), MASK16)
C
C  Check if Channel Length is > 3, else issue a warning and return
C
          IF (LENGTH.LE.3) THEN
            POINT = LCDDN
C            WRITE(MESG,1010)LENGTH
C            CALL INTMSG(MESG)
            NID = -1
            GO TO 999
          ENDIF
C
C  Check if Channel Length is < 512, else issue a warning and return
C
            IF (LENGTH.GT.512) THEN
              POINT = LCDDN
C              WRITE(MESG,1010)LENGTH
C              CALL INTMSG(MESG)
              NID = -2
              GO TO 999
            ENDIF
C
C  Perform task specific fills.
C
          ITMP = ITMP - 1
C
          IF(TASK.EQ.2) THEN
            TCARD = ((ITMP-1) / 16) + 1 
            IF(TCARD.GE.1 .AND. TCARD.LE.100) THEN
              CRATEID(TCARD,1) = CRATEID(TCARD,1) + LENGTH
            ENDIF
          ENDIF
C
          IF(TASK.EQ.3) THEN
            IF (ITMP.GE.(ICARD*16+1) .AND. ITMP.LE.(ICARD+1)*16) THEN
C              IF (FADCCH.GE.CH_MIN.AND.FADCCH.LE.CH_MAX) THEN
              NID = NID + 1
C Channels come out in reverse order since reading backwards through the bank
              CRATEID(17-NID,1) = CHNL
C             ENDIF
            ENDIF
            IF(ITMP .LE. ICARD*16 ) GOTO 999   ! Done for this card
          ENDIF
C
C  Increment pointer and go on
C
          POINT = POINT - LENGTH/4
        ENDDO
      ELSE
        WRITE(*,*) ' Bad Task Number, only use 1, 2, or 3 '
      ENDIF
 1010 FORMAT(' Error in Channel length : Length = ',I8)
C---------------------------------------------------------------------------
  999 RETURN
      END
