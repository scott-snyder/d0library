C----------------------------------------------------------------------
      SUBROUTINE DBMU_UNPHV (LINK,NVAL,VAL,IRET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will unpack a DBMON HV bank, into an double
C-    array VAL (1:4,1:NCHN), where :
C-
C-                    VAL(1,ICH)   is the voltage for channel ICH
C-                    VAL(2,ICH)   is the current for channel ICH
C-                    VAL(3,ICH)   is the status for channel ICH
C-                    VAL(4,ICH)   0 = channel not found in bank
C-                                 1 = channel found in bank
C-
C-   Inputs  :  LINK   Link to Zebra bank
C-              NVAL   Second dimension of VAL (first is fixed to 4)
C-   Outputs :  VAL    Voltage, current, status for found channels
C-              IRET   Number of channels unpacked
C-   Controls: 
C-
C-   Created  17-OCT-1992   Lars Rasmussen
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER ICFNBL
C
      INTEGER LINK,NVAL,IRET
      REAL VAL(4,*)
      INTEGER I,J,K,OFF1,OFF2,LNB,NATT,ICH
      CHARACTER CDEV*12, CATT*4,C3*3
      INTEGER*2 K2
C----------------------------------------------------------------------
      IRET = 0
      IF (LINK .LE. 0) THEN
         CALL MSGO ('w','DBMU_UNPHV','Invalid link',0)
         RETURN
      END IF
      DO I = 1,NVAL
         VAL(4,I) = 0.
      END DO
C
C- Unpack standard dbmon HV bank: The devices (channels) is defined by
C- attributes with Len(Attribute)=4 and one real data word pr. attribute.
C- Each device (channel) have three attributes Vnnn, Cnnn and Snnn. 
C
      CALL UHTOC (IC(LINK+4),4,CDEV,12)
      CALL STR$TRIM(CDEV,CDEV,K2)
      IF (K2 .LE. 0) K2 = 1
      NATT = (IC(LINK-1)-IC(LINK+2))/2
      OFF1 = LINK+IC(LINK+2)
      OFF2 = OFF1 + NATT
      DO I = 1,NATT,3
         CALL UHTOC (IC(OFF1+3),4,CATT,4)
         LNB = ICFNBL(CATT,1,LEN(CATT))
         IF (CATT(LNB:LNB) .NE. 'S') GOTO 991
         CATT = CATT(LNB:)
         READ (CATT,'(1x,I3)',ERR=991) ICH
         IRET = IRET + 1
         DO J = 1,3
            VAL(J,ICH) = C(OFF2+J)
         END DO
         VAL(4,ICH) = 1.
         OFF2 = OFF2 + 3
         OFF1 = OFF1 + 3
      END DO
C
  999 RETURN
C
C- error case
C
 991  CALL MSGO('w','DBMU_UNPHV','Oh Oh error in decoding HV data',0)
      IRET = -1
      RETURN

      END
