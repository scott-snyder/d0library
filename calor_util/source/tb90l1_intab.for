      SUBROUTINE TB90L1_INTAB(ISTAT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reads in the TB90 calorimeter ordered
C-                         Binary versions of cal. address files.
C-                         Puts table into TB_SORT_ORD common.
C-
C-   Inputs  : none
C-   Outputs : ISTAT - error code returned
C-   Controls: none
C-
C-   Created  12-DEC-1989   Andrew P. White
C-   Updated   7-MAR-1990   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TB_SORT_ORD.INC'
      CHARACTER*48, MSG_STRING
      INTEGER I,II,ISTAT
      INTEGER KIUNIT,KIOSTAT,KJUNIT,KJOSTAT,KIIOSTAT,KJJOSTAT
C----------------------------------------------------------------------
      ISTAT=0
C
      CALL GTUNIT(6585,KIUNIT,ISTAT)
      IF (ISTAT.NE.0)THEN
        WRITE(MSG_STRING,200)
        CALL INTMSG(MSG_STRING)
        GO TO 999
      ENDIF
C
      CALL GTUNIT(6585,KJUNIT,ISTAT)
      IF (ISTAT.NE.0)THEN
        WRITE(MSG_STRING,200)
        CALL INTMSG(MSG_STRING)
        GO TO 999
      ENDIF
C
C- Read in the PHYSICS ordered table.
C
C&IF IBMAIX,LINUX
C&      OPEN(     UNIT=KIUNIT,
C&     &          FILE='TB90L1_PHYSICS_SORT_BINARY_DAT',
C&     &          FORM='UNFORMATTED',
C&     &          STATUS='OLD',ERR=99,IOSTAT=ISTAT)
C&ELSE
      OPEN(     UNIT=KIUNIT,
     &          FILE='TB90L1_PHYSICS_SORT_BINARY_DAT',
     &          FORM='UNFORMATTED',
     &          STATUS='OLD',ERR=99,IOSTAT=ISTAT,READONLY)
C&ENDIF
C
      DO 10 I=1,SIZE
        READ(KIUNIT,END=50,ERR=99,IOSTAT=ISTAT)
     &           II,SEQADC(I,1),ADCADD(I,1),
     C           CARD(I,1),BLS(I,1),TOWER(I,1),DEPTH(I,1),
     C           IFTB(I,1),CON(I,1),PIN(I,1),ING(I,1),BMOD(I,1),
     C           POINT(I,1),SECT(I,1),ETA(I,1),PHI(I,1),LAYER(I,1)
   10 CONTINUE
C
C- Read in the ADC ordered table.
C
50      CONTINUE
C&IF IBMAIX,LINUX
C&        OPEN(     UNIT=KJUNIT,
C&     &          FILE='TB90L1_ADC_SORT_BINARY_DAT',
C&     &          FORM='UNFORMATTED',
C&     &          STATUS='OLD',ERR=99,IOSTAT=ISTAT)
C&ELSE
        OPEN(     UNIT=KJUNIT,
     &          FILE='TB90L1_ADC_SORT_BINARY_DAT',
     &          FORM='UNFORMATTED',
     &          STATUS='OLD',ERR=99,IOSTAT=ISTAT,READONLY)
C&ENDIF
C
      DO 20 I=1,SIZE
        READ(KJUNIT,END=60,ERR=99,IOSTAT=ISTAT)
     &           II,SEQADC(I,2),ADCADD(I,2),
     C           CARD(I,2),BLS(I,2),TOWER(I,2),DEPTH(I,2),
     C           IFTB(I,2),CON(I,2),PIN(I,2),ING(I,2),BMOD(I,2),
     C           POINT(I,2),SECT(I,2),ETA(I,2),PHI(I,2),LAYER(I,2)
   20 CONTINUE
C
  200   FORMAT(' ERROR GETTING FREE UNIT, RETURN')
   60 CALL RLUNIT(6585,KIUNIT,KIIOSTAT)
      CALL RLUNIT(6585,KJUNIT,KJJOSTAT)
      CLOSE(UNIT=KIUNIT)
      CLOSE(UNIT=KJUNIT)
      ISTAT = 0
      GOTO 999
   99 WRITE(MSG_STRING,'(A,2X,I5)')'ERROR IN READING FILE IOSTAT',ISTAT
      CALL ERRMSG('TB90_UNPACK','TB90L1_INTAB',MSG_STRING,'F')
  999 RETURN
      END
