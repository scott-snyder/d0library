C----------------------------------------------------------------------
      SUBROUTINE DBMU_UNPDM (LINK,DEV,NDEV,NVAL,VAL,IRET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will unpack standard dbmon bank, pointed to
C-    by link.
C-
C-   Inputs  : LINK  (I)    Pointer to a DBMD bank or linier structure 
C-             DEV   (C)    Devices to fetch
C-             NDEV  (I)    Number of devices (length of DEV and second array
C-                          index for VAL)
C-             NVAL  (I)    Number of data words for each device.att (first
C-                          array index of VAL)
C-   Outputs : VAL   (R)    Array of read values (ok, if value # -999.0)
C-             IRET  (I)    Number of devices found
C-                          = 0  no channels found
C-                          =-1  something went wrong
C-
C-   Created   3-OCT-1992   Lars Rasmussen
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D3U.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER          LINK,NDEV,NVAL,IRET
      CHARACTER *(*)   DEV(NDEV)
      REAL             VAL(NVAL,NDEV),XHOU
C
      INTEGER          I,J,K,LP
      INTEGER          NATT,LDOT
      INTEGER          LW,WDA,WAT,WTY,WD1,OFF1,OFF2
      CHARACTER        CDEV*12,CATT*12,HDEV*12,HATT*12,DVAT*17
      CHARACTER        CHO*12,TOPD*24,C1
      LOGICAL          FND
C----------------------------------------------------------------------
      DO I = 1,NDEV
      DO J = 1,NVAL
         VAL(J,I) = -999.0
      END DO
      END DO
C
      FND = .FALSE.
      IRET = 0
      LP = LINK
      DO WHILE (LP .GT. 0)
         LW = IC(LP+3)
         WDA = ISHFT(LW,-16)
         WD1 = ISHFT(LW,16)
         WD1 = ISHFT(WD1,-16)
         WAT = ISHFT(WD1,-8)
         WTY = ISHFT(WD1,24)
         WTY = ISHFT(WTY,-24)
         IF (WDA .LE. 0) WDA = 1
         IF (WAT .LE. 0 .OR. WAT .GE. 4) THEN
            WAT = 1
            WDA = 1
         END IF
         CALL UHTOC (IC(LP+IC(LP+2)-2),4,CDEV,12)
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
C
999   RETURN
      END
