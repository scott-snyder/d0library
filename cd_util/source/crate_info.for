      SUBROUTINE CRATE_INFO(ICDD,ICRATE,ICARD,LCHN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Ask questions about what hardware to
C-                         display, used for CD Electronics Examine
C-
C-   Inputs  : none
C-   Outputs : BANK = CDDn BANK
C-             ICRATE
C-             ICARD
C-             LCHN = ARRAY OF LOGICAL CHANNEL ADDRESSES FOR CHANNELS
C-                    IN SLOT
C-   Controls:
C-
C-   Created  12-OCT-1990   Susan K. Blessing
C-   Updated  02-APR-1992   Susan K. Blessing   Use INTMSG and GETPAR
C-    rather than PFUMES and PFRSTR.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCDD1.LINK'
C
      INTEGER I, J, K
      INTEGER ICDD, ICRATE, ICARD
      INTEGER CRATEID(100,2), NID, TASK, MCDD, NCDD, LENCDD(4),LKCDDN(4)
      INTEGER CID
      INTEGER LEN
      INTEGER ILINE, ISKIP, ID, NCARDS
      INTEGER LCHN(16)
      INTEGER IDMIN,IDMAX
C
      CHARACTER*60 PROM
      CHARACTER*80 STRING
C
C----------------------------------------------------------------------
C
      NCDD=0
      MCDD=0
      DO I=1,4
        LKCDDN(I)=LQ(LHEAD-IZCDD1-I+1)
        LENCDD(I)=0
        IF(LKCDDN(I).GT.0) THEN
          LENCDD(I)=IQ(LKCDDN(I)-1)
          NCDD=NCDD+1
          MCDD=I
        ENDIF
      ENDDO
      WRITE(STRING,99) (LENCDD(I),I=1,4)
   99 FORMAT('  LENGTHS OF CDD BANKS (1 TO 4) =',4I8)
      CALL INTMSG('/')
      CALL INTMSG(STRING)
      CALL INTMSG('/')
C
      IF(NCDD.EQ.1) THEN
        ICDD=MCDD
        IF(ICDD.LT.1 .OR. ICDD.GT.4) ICDD = 9
        WRITE(STRING,1020) ICDD
 1020   FORMAT(' Only bank CDD',I1,' present.')
        CALL INTMSG(STRING)
      ELSE
        LEN=0
        PROM = ' CDD 1,2,3, or 4? (1=VTX,2=CDC,3=FDC,4=TRD)>'
        CALL GETPAR(1,PROM,'I',ICDD) 
        IF (ICDD .LT. 1 .OR. ICDD .GT. 4) GOTO 800
      ENDIF
      TASK = 1
      CALL ZCRATE(ICDD,ICRATE,ICARD,NID,CRATEID,TASK)        ! Get Crate IDs.
      IF (NID.LT.0) THEN
        WRITE(STRING,1010) ICDD
 1010   FORMAT(' ERROR IN CDD BANK ',I1)
        CALL INTMSG(STRING)
        CALL INTMSG('Please go to the next event and try again.')
        GO TO 900
      END IF
C
      CALL INTMSG('/')
      DO 10 ILINE=0,(NID/6)
        ISKIP = ILINE * 6
        IDMIN = 1+ISKIP
        IDMAX = MIN(6+ISKIP,NID)
        IF (IDMIN.LE.IDMAX) THEN
         WRITE(STRING,1000) (CRATEID(ID,1),ID=IDMIN,IDMAX)
 1000     FORMAT(' Available Crates: ',6I5)
          CALL INTMSG(STRING)
        END IF
   10 CONTINUE
C
      IF (NID.EQ.1) THEN
        ICRATE = CRATEID(1,1)
        CID = 1
      ELSE
        PROM = ' Crate to display? (0=SKIP)>'
        CALL GETPAR(1,PROM,'I',ICRATE) 
        IF(ICRATE .EQ. 0) GOTO 900
        DO 20 ID=1,NID
          IF(ICRATE.EQ. CRATEID(ID,1) ) THEN
            CID = ID
            GOTO 30
          END IF
   20   CONTINUE
        GOTO 800                          ! Didn't pick valid crate.
   30   CONTINUE
      END IF
C
      NCARDS=CRATEID(CID,2)
      WRITE(STRING,1001) NCARDS,ICRATE
 1001 FORMAT(' There are ',I2,' cards in crate ',I3,'.')
      CALL INTMSG('/')
      CALL INTMSG(STRING)
C
      TASK = 2
      CALL ZCRATE(ICDD,ICRATE,ICARD,NID,CRATEID,TASK)
      IF (NID.LT.0) THEN
        WRITE(STRING,1010) ICDD
        CALL INTMSG(STRING)
        CALL INTMSG('Please go to the next event and try again.')
        GO TO 900
      END IF
C
      IDMAX = 15
      IDMIN = MAX(16-NCARDS,8)
      WRITE(STRING,1003) (I,I=IDMAX,IDMIN,-1)
 1003 FORMAT(' Card Number  =  ',8I5)
      CALL INTMSG(STRING)
      WRITE(STRING,1002) (CRATEID(ID+1,1),ID=IDMAX,IDMIN,-1)
 1002 FORMAT(' Words / Card =  ',8I5)
      CALL INTMSG(STRING)
      IF (NCARDS.GT.8) THEN
        IDMAX = 7
        IDMIN = 16 - NCARDS
        WRITE(STRING,1003) (I,I=IDMAX,IDMIN,-1)
        CALL INTMSG(STRING)
        WRITE(STRING,1002) (CRATEID(ID+1,1),ID=IDMAX,IDMIN,-1)
        CALL INTMSG(STRING)
      END IF
C
C      IDMIN = 0
C      IDMAX = MIN(NCARDS,8) - 1
C      WRITE(STRING,1003) (I,I=IDMIN,IDMAX)
C 1003 FORMAT(' Card Number  =  ',8I5)
C      CALL INTMSG(STRING)
C      WRITE(STRING,1002) (CRATEID(ID+1,1),ID=IDMIN,IDMAX)
C 1002 FORMAT(' Words / Card =  ',8I5)
C      CALL INTMSG(STRING)
C      IF (NCARDS.GT.8) THEN
C        IDMIN = 8
C        IDMAX = NCARDS - 1
C        WRITE(STRING,1003) (I,I=IDMIN,IDMAX)
C        CALL INTMSG(STRING)
C        WRITE(STRING,1002) (CRATEID(ID,1),ID=9,NCARDS)
C        CALL INTMSG(STRING)
C      END IF
C
      PROM = ' Display which card? (99=Back to Menu)>'
      CALL GETPAR(1,PROM,'I',ICARD) 
      IF(ICARD .EQ. 99) GOTO 900
      IF(ICARD .GT. 15 .OR. ICARD .LT. 16-NCARDS ) GOTO 800
C
      TASK = 3
      CALL ZCRATE(ICDD,ICRATE,ICARD,NID,CRATEID,TASK)
      IF (NID.LT.0) THEN
        WRITE(STRING,1010) ICDD
        CALL INTMSG(STRING)
        CALL INTMSG('Please go to the next event and try again.')
        GO TO 900
      END IF
C
      DO I = 1, 16
        LCHN(I) = CRATEID(I,1)
      END DO
      GO TO 999
C
  800 CONTINUE
      CALL INTMSG(' Improper value.')
C
  900 CONTINUE
      ICDD = -1
C---------------------------------------------------------------------------
  999 RETURN
      END
