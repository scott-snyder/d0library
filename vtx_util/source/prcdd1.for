      SUBROUTINE PRCDD1 ( PRUNIT, KCDD1, NCDD1, CARFL, IPRFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print digitization banks CDD1 (VTX chamber)
C-
C-   Inputs  : PRUNIT : Unit number
C-             LCDD1  : Pointer to one bank ( or first of a linear set )
C-             NCDD1  : ID number of one bank
C-             CARFL  : Character flag, 'ALL' = all CDD1 banks
C-                      'LINEAR' = all linear structure from LCDD1
C-                      'SINGLE' = only the one with LCDD1 OR NCDD1
C-             IPRFL  : Level of print
C-                      0 = no printout
C-                      1 = length of the bank
C-                      2 = FADC # and # of clusters in each one
C-                      3 = Full content of the bank
C-
C-   Created  17-AUG-1987   Ghita Rahal-Callot
C-   Updated   8-DEC-1987   Ploutarchos Roumeliotis
C-   Updated  12-FEB-1988   Ghita Rahal-Callot  : add levels of printout
C-   Modified  6-OCT-1988   Tom Trippe : adapted to CDD1 (VTX chamber)
C-   Updated  23-NOV-1988   Ghita Rahal-Callot: add 1 level of minimum printout
C-   Updated  10-JAN-1989   Peter Grudberg-add z-strip printout
C-   Updated  20-JUN-1989   Peter Grudberg - descramble output
C-   Updated  21-JUN-1990   Peter Grudberg - upgrade for versions 0-4  
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCDD1.LINK'
      INTEGER PRUNIT, KCDD1, LCDD1, NCDD1, IPRFL, IVERS, VTYPE, IERR
      INTEGER LONG, LENFADC, LABFADC, ICLUS, NWFADC, I, LFADC, TYPE
      PARAMETER ( LFADC = 512 )
      INTEGER LAYER, SECTOR, WIRE, END, STRIP, UBIT, LENCLU, LOCCLU, IP
      INTEGER IPORIG, IPIN, LDHEAD, LCRATE, LTRAIL, LDCTRL, DATAS(LFADC)
C
C **** Number of 32 bit words per channel; must work for TB 512 bins
C
      INTEGER NWW, MXLABL
      PARAMETER (NWW = 130 )
      PARAMETER ( MXLABL = 32768 )      ! 15 bit label
      INTEGER HIGHOF, LOWOFF
      PARAMETER ( HIGHOF = 16, LOWOFF = 0 )
      INTEGER LEDGE, NWORDS, DETAILS(5,10), MINLON
      CHARACTER*(*) CARFL
      CHARACTER*8 ZFORMA
      DATA LTRAIL /4/                   ! length of crate trailer
      DATA LDCTRL /16/                  ! length of data cable trailer
C----------------------------------------------------------------------
      LCDD1 = KCDD1
      IF ( IPRFL .LE. 0 ) GO TO 999
      IF ( LCDD1 .LE. 0 ) THEN
        LCDD1 = LQ ( LHEAD - IZCDD1 )
      ENDIF
      IF ( LCDD1 .LE. 0 ) THEN
        CALL ERRMSG('Bank not booked','PRCDD1',
     &              'Bank CDD1 does not exist or wrong data cable','I')
        GO TO 999
      ENDIF
C
C ****  Get bank version number
C
      CALL ZRD_VERSION(1,IVERS,VTYPE,IERR)
      IF( IERR .NE. 0) THEN
        CALL ERRMSG('Bank not booked','PRCDD1',
     &              'Bank CDD1 does not exist or wrong data cable','I')
        GO TO 999
      ENDIF
      LONG = IQ( LCDD1 - 1 )
      WRITE (PRUNIT,101) IVERS, LONG
      IF( IVERS .GT. 4 ) THEN
        CALL ERRMSG('Bank version > 4','PRCDD1',
     &              'Bank CDD1 version > 4 not yet supported','I')
        GO TO 999
      ENDIF
C
C ****  Get header information
C
      CALL CD_DECODE_HEADER(LCDD1, IVERS, NWORDS, DETAILS)
      WRITE (PRUNIT,102) DETAILS(1,1),DETAILS(2,1),(DETAILS(3,I),I=1,4),
     &  (DETAILS(4,I),I=1,3)
      IF ( IPRFL .LE. 1 ) GO TO 999
C
C ****  Get cluster information
C
      IPORIG = LCDD1
      LCRATE = LONG
      IF ( IVERS .GE. 3 ) THEN
        LONG = LONG - LDCTRL
        LCRATE = IQ(IPORIG + LONG - 3)
      ELSEIF ( IVERS .EQ. 2 ) THEN
        LCRATE = IQ(IPORIG + LONG - 3) + LTRAIL
      ENDIF
      IF ( IVERS .EQ. 0 ) THEN
        MINLON = 0
      ELSE
        LDHEAD = IQ(IPORIG + 1) + 1
        MINLON = LDHEAD + LONG - LCRATE
      ENDIF
      IF ( IVERS .LE. 2 ) THEN
        LONG = LONG - 2 * IVERS
      ELSE
        LONG = LONG - LTRAIL
      ENDIF
C
C ****  Now loop over channels
C ****  LABFADC = logical channel address
C ****  LENFADC = total length of fadc (bytes)
C
   10 CONTINUE
      IPIN = IPORIG + LONG
      IF ( LONG .LE. MINLON ) GO TO 20
      LENFADC = IBITS(IQ(IPIN), LOWOFF, 16)
      LABFADC = IBITS(IQ(IPIN), HIGHOF, 15)
      IF ( LENFADC .GT. LFADC ) GO TO 900       ! deal with errors
      IF ( LABFADC .GT. MXLABL ) GO TO 900
C
C ****  Decode the fadc address
C
      CALL VCODER(LABFADC,TYPE,LAYER,SECTOR,WIRE,STRIP,END,UBIT,1)
      IF ( TYPE .EQ. 0 ) THEN
        WRITE (PRUNIT,103) ZFORMA(LABFADC), LAYER, SECTOR, WIRE, END, 
     &                                                     LENFADC
      ELSE
        WRITE (PRUNIT,104) ZFORMA(LABFADC), LAYER, STRIP, END, LENFADC
      ENDIF
      NWFADC = LENFADC / 4
      IF ( IVERS .EQ. 0 ) NWFADC = NWFADC - 1
      IF ( LENFADC .LE. 0 ) GO TO 910
C
C ****  Now unpack the clusters for this channel
C
      CALL ZDEXPD(1, LABFADC, DATAS)
      IP = 1
      ICLUS = 0
   30 CONTINUE
      LENCLU = DATAS(IP)
      IF ( LENCLU .GT. 0 ) THEN
        ICLUS = ICLUS + 1
        LOCCLU = DATAS(IP + 1)
        IP = IP + 2
        LEDGE = LOCCLU - LENCLU + 1
        IF ( IPRFL .GE. 2 ) THEN
          WRITE (PRUNIT,105) ICLUS, LEDGE, LOCCLU, LENCLU
        ENDIF
        IF ( IPRFL .GE. 3 ) THEN
          WRITE (PRUNIT,106) (DATAS(I), I = IP, IP + LENCLU - 1)
        ENDIF
C
        IP = IP + LENCLU
        GO TO 30
      ENDIF
C
C ****  Prepare for next channel
C
      LONG = LONG - NWFADC
      GO TO 10
C
   20 IF ( (IVERS .GT. 1) .AND. (MINLON .GT. LDHEAD) ) THEN
C
C ****  Prepare for next crate
C
        LONG = LONG - LDHEAD
        IF ( IVERS .EQ. 3 ) LONG = LONG - 1
        LCRATE = IQ(IPORIG + LONG - 3)
        IF ( IVERS .EQ. 2 ) LCRATE = LCRATE + LTRAIL
        MINLON = LDHEAD + LONG - LCRATE
        LONG = LONG - LTRAIL
        GO TO 10
      ENDIF
C
C ****  Done!
C
      GO TO 999
C
C ****  Handle errors
C
  900 CONTINUE
      WRITE (PRUNIT,107) LONG, LENFADC, LFADC, LABFADC, MXLABL
      GO TO 999
  910 CONTINUE
      WRITE (PRUNIT,108) LONG, LENFADC
      GO TO 999
C
  101 FORMAT(/10X,' Bank CDD1: Version ',I3,'   Total # of words:',I5)
  102 FORMAT(5X,' Header Length    =',I5,'   Sync Word =',I10,
     &  '  Crate ID      =',I10/
     &       5X,' Num of FADC Cards=',I5,'   Crate Mode=',I10,
     &  '  Data Type     =',I10/
     &       5X,' System word   =',I8,'   User word =',I10,
     &  '  Version Number=',I10)
  103 FORMAT(5X,' FADC#:',A8,2X,'( Layer=',I2,2X,'Sector=',I2,2X,
     &'Wire=',I2,2X,'End=',I2,2X,')',2X,'# of Bytes:',I3)
  104 FORMAT(5X,' FADC#:',A8,2X,'( Strip layer=',I2,2X,'Strip=',I3,2X,
     &'End=',I2,2X,')',2X,'# of Bytes:',I3)
  105 FORMAT(7X,'Cluster #:',I2,2X,' Leading Edge:',I3,2X
     &  ,' Trailing Edge:',I3, 2X,' # of time slices:',I3)
  106 FORMAT( (7X,'LSB/MSB  ',4(4(I3,2X),' / ')) )
  107 FORMAT(' Error in bank CDD1, offset=',I10,'  Fadc Length=',I10,
     &       '>',I10,' the maximum OR'/40X,' Fadc label=',I10,'>',I10,
     &       ' the maximum')
  108 FORMAT(' Error in bank CDD1, offset=',I10,'  Fadc Length=',I10)
C
  999 RETURN
      END
