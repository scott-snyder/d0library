       SUBROUTINE PRTRG1(PRUNIT)
C-----------------------------------------------------------------------
C
C   Print out routine for TRG1 bank: this version for 1987 test beam
C
C   Bank contains trigger and pulser information
C       Word 1            Controller word count  (= 4)
C       Word 2            Sync word              (= xxxxFFFF hex)
C       Word 3            Pulser status word (calorimeter)
C       Word 4            Pulser status word (central detector)
C       Word 5            Trigger type
C
C       Word 6            Trigger word count (= 512)
C         next 512 words  Trigger information (this event)
C
C       Word 519          Trigger word count (= 512)
C         next 512 words  Trigger information (previous event)
C
C       Word 1032         Trigger word count (= 128)
C         next 128 words   ?
C
C       Word 1061         CAMAC word count (= 57?)
C         6 words PWCs    12 16-bit words:
C                          1-4  NWAPWC2, X1-X4
C                          5-12 NWAPWC2, Y1-Y8
C
C         ETC.
C
C-----------------------------------------------------------------------
C                                                Author:  Wyatt Merritt
C                                                Date:    2-JUL-87
C-----------------------------------------------------------------------
C
       IMPLICIT NONE
C
       INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
       INCLUDE 'D0$LINKS:IZTRGR.LINK/LIST'
C
       INTEGER PRUNIT
       INTEGER LTRG1,NWORDS,I
       INTEGER INDX,NC,I1,I2,J,NT1,NT2,NT3,LT
C
C
C-----------------------------------------------------------------------
C
C      Check whether bank is empty
C
       LTRG1 = LQ(LHEAD-IZTRGR)
       IF (LTRG1 .EQ. 0) THEN
         WRITE (PRUNIT,990)
         RETURN
       ELSE
         WRITE (PRUNIT,1000)
         LT = LTRG1 + 1
         NWORDS = IQ(LT)
         WRITE (PRUNIT,1001) (IQ(I),I=LT,LT+NWORDS)
       ENDIF
  990  FORMAT(' TRIGGER BANK TRG1 IS EMPTY ')
 1000  FORMAT(/,
     X'======================================================'/
     X'         TRG1: Trigger bank (+ pulser info)           '/
     X'======================================================'/)
 1001  FORMAT(/,' CONTROLLER WORD =            ',I12/
     X          ' SYNC WORD = (hex)            ',Z12/
     X          ' PULSER STATUS (CAL) = (octal)',O12/
     X          ' PULSER STATUS (CD) = (octal) ',I12/
     X          ' TRIGGER TYPE =               ',I12/)
       NT1 = IQ(LT+NWORDS+1)
       WRITE (PRUNIT,1998) NT1
 1998  FORMAT(/,10X,'TRIGGER WORDS (THIS EVT) = ',I12,/,
     X'-------------------------------------------------------',/)
       NT2 = IQ(LT+NWORDS+NT1+2)
       WRITE (PRUNIT,1999) NT2
 1999  FORMAT(/,10X,'TRIGGER WORDS (PREV EVT) = ',I12,/,
     X'-------------------------------------------------------',/)
       NT3 = IQ(LT+NWORDS+NT1+NT2+3)
       WRITE (PRUNIT,1997) NT3
 1997  FORMAT(/,10X,'TRIGGER WORDS (?) = ',I12,/,
     X'-------------------------------------------------------',/)
       INDX = LT + NWORDS + NT1 + NT2 + NT3 + 4
       NC = IQ(INDX)
       WRITE (PRUNIT,1996) NC
 1996  FORMAT(/,10X,'CAMAC WORDS = ',I12,/,
     X'-------------------------------------------------------',/)
       IF (NC .LE. 0) RETURN
       DO 100 I = 1,NC,5
         I2 = MIN0(NC,I + 4)
         WRITE (PRUNIT,2000) I,(IQ(INDX+J),J=I,I2)
  100  CONTINUE
 2000  FORMAT(1X,I5,5(1X,Z12))
       RETURN
       END
