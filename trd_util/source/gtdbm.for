C----------------------------------------------------------------------
      SUBROUTINE GTDBM (CLASS,DEV,NDEV,DI,TI,NVAL,VAL,DO,TO,ENDVL,IRET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will fetch from DBL3 database (defined by 
C-    logical DBM_DBFILE) values for devices DEV(1:NDEV) (where all 
C-    devices belongs to same class), closets back in time, to time DI,TI.
C-
C-   Inputs  : CLASS (C)  The class of which the devices belong to
C-             DEV   (C)  Devices to fetch
C-             NDEV  (I)  Number of devices (length of DEV and second array
C-                        index for VAL)
C-             DI    (I)  Search date (YYMMDD)
C-             TI    (I)  Search time (HHMMSS)
C-             NVAL  (I)  Number of data words for each device.att (first 
C-                        array index of VAL)
C-   Outputs : VAL   (R)  Array of read values (ok, if value # .0)
C-             DO    (I)  Corresponding dates
C-             TO    (I)  Corresponding times
C-             ENDVL (I)  Endvalidity of found element (dbl3 packed time)
C-             IRET  (I)  Return status, <0=ERROR, >=0 = number of devices
C-                        found.
C-   Controls: 
C-
C-   Created  23-JUL-1991   Lars Rasmussen
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D3U.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
C
      CHARACTER *(*)   CLASS
      INTEGER          NDEV
      CHARACTER *(*)   DEV(NDEV)
      INTEGER          DI,TI,DO,TO,ENDVL,IRET,NVAL
      REAL             VAL(NVAL,NDEV)
C
      CHARACTER*48     BPATH
      INTEGER          I,J,K,OFF1,OFF2
      INTEGER          KTIME
      INTEGER          KEYC(D3_NK),KEYOO(D3_NK),LNK(10),NLNK
      INTEGER          LP
      INTEGER*2        LS
      REAL             DSCAL
      LOGICAL          FND
      CHARACTER        CDEV*12,CATT*12,HDEV*12,HATT*12,DVAT*17,CLS*12
      CHARACTER        CHLP*12
      INTEGER          NATT,LDOT
C
      INTEGER         LW,WAT,WDA
      INTEGER*2       WD(2)
      BYTE            BY(4)
      EQUIVALENCE    (LW,BY(1))
      EQUIVALENCE    (LW,WD(1))
C----------------------------------------------------------------------
      CHLP = CLASS
      CALL STR$TRIM(CHLP,CHLP,LS)
      IF (LS .LE. 0) THEN
         CALL ERRMSG ('GTDBM','Class name invalid',' ','W')
         IRET = -1
         GOTO 999
      END IF
      ENDVL = 0
      IRET = 0
      DO I = 1,NDEV
      DO J = 1,NVAL
         VAL(J,I) = .0
      END DO
      END DO
      DO I=1,LEN(CLS)/4
        J = 4*(I-1)
        CLS(J+BYTE1:J+BYTE1) = CHLP(J+1:J+1)
        CLS(J+BYTE2:J+BYTE2) = CHLP(J+2:J+2)
        CLS(J+BYTE3:J+BYTE3) = CHLP(J+3:J+3)
        CLS(J+BYTE4:J+BYTE4) = CHLP(J+4:J+4)
      END DO
C
      CALL UCTOH (CLS,KEYC(8),4,12)
C
      CALL DBPKTS (DI,TI,KTIME)
      IF (DI .GT. 10000000) KTIME = 999999999      
      KEYC(3) = KTIME
      KEYC(4) = KTIME
C
      NLNK = 10
      CALL D3U_FETCH (KEYC,NLNK,LNK,IRET)
      DO I = 1,D3_NK
         KEYOO (I) = IC(LC(LNK(1)+1)+I)
      END DO
      IF (IRET .NE. 1) THEN
         CALL ERRMSG ('GTDBM','D3U_FETCH failed',' ','W')
         IRET = -1
         GOTO 999
      ELSE IF (LNK(1) .LE. 0) THEN
         CALL ERRMSG ('GTDBM','D3U_FETCH Invalid link',' ','W')
         IRET = -1
         GOTO 999
      ELSE IF (KEYOO(3) .LE. 1) THEN    ! First dummy element
         IRET = 0
         ENDVL = KEYOO(4)
         GOTO 999
      END IF
      ENDVL = KEYOO(4)
      CALL DBUPTS (DO,TO,IC(LNK(1)+2))
C
      LP = LC(LNK(1)-1)
      FND = .FALSE.
      IRET = 0
      DO WHILE (LP .GT. 0)
         LW = IC(LP+3)
         WAT = BY(BYTE2)
         WDA = WD(WORD2)
         IF (WDA .LE. 0) WDA = 1
         IF (WAT .LE. 0 .OR. WAT .GE. 4) THEN
            WAT = 1
            WDA = 1
         END IF
CCCCC         CALL LIB$MOVC3 (12,IC(LP+4),%REF(CDEV))
         CALL UHTOC (IC(LP+4),4,CDEV,12)
         NATT = (IC(LP-1)-IC(LP+2))/(WAT+WDA)
         HDEV = ' '
         HATT = ' '
         DO 101 I = 1,NDEV
            DVAT = DEV(I)
            LDOT = INDEX(DVAT,'.')
            HDEV = DVAT(1:LDOT-1)
            HATT = DVAT(LDOT+1:)
            IF (CDEV .EQ. HDEV .AND. NATT .GT. 0) THEN
               OFF1 = LP+IC(LP+2)
               OFF2 = OFF1+WAT*NATT
               DO J = 1,NATT
                  CATT = ' '
                  CALL UHTOC (IC(OFF1+1),4,CATT,4*WAT)
                  IF (HATT(1:4*WAT) .EQ. CATT(1:4*WAT)) THEN
                     FND = .TRUE.
                     IRET = IRET + 1
                     DO K = 1,WDA
                        VAL(K,I) = C(OFF2+K)
                     END DO
                     GOTO 101
                  END IF
                  OFF2 = OFF2 + WDA
                  OFF1 = OFF1 + WAT
               END DO
            END IF
101      CONTINUE
         LP = LC(LP)
      END DO
      CALL DBFREE (D3_PATH,0,KEYOO,'K')
991   IF (.NOT.FND) CALL ERRMSG ('GTDBM','Nothing found',' ','I')
C
999   RETURN
      END
