C DEC/CMS REPLACEMENT HISTORY, Element CVSEC_DELTA_TIME.FOR
C *1    25-MAR-1990 13:31:52 HARRY "Convert seconds into VMS delta time format"
C DEC/CMS REPLACEMENT HISTORY, Element CVSEC_DELTA_TIME.FOR
      SUBROUTINE CVSEC_DELTA_TIME (SECS,DELTA_TIME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert given number of seconds into a
C-   string with the VMS relative time format.
C-
C-   Inputs  : SECS     [R]     Number of seconds
C-   Outputs : DELTA_TIME  [C*]    Time in VMS DELTA time format.
C-   Controls: None
C-
C-   Created  24-MAR-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      REAL    SECS
      CHARACTER*(*) DELTA_TIME
C
      CHARACTER*16 TIME
      INTEGER DAY,HOUR,MIN,I
      REAL    SEC
C----------------------------------------------------------------------
C
C ****  Convert into days, hours, minutes and seconds
C
      DAY = SECS/86400.
      SEC = SECS - 86400*DAY
      HOUR= SEC/3600.
      SEC = SEC - 3600*HOUR
      MIN = SEC/60.
      SEC = SEC - 60*MIN
C
C ****  Convert from numbers to strings
C
      IF ( DAY .GT. 0 .AND. DAY .LT. 9999 ) THEN
        WRITE(UNIT=TIME,FMT='(I4,1X,I2.2,'':'',I2.2,'':'',F5.2)')
     &    DAY,HOUR,MIN,SEC
      ELSE
        IF ( HOUR .GT. 0 ) THEN
          WRITE(UNIT=TIME,FMT='(''0 '',I2.2,'':'',I2.2,'':'',F5.2)')
     &    HOUR,MIN,SEC
        ELSE
          WRITE(UNIT=TIME,FMT='(''0 :'',I2.2,'':'',F5.2)')
     &    MIN,SEC
        ENDIF
      ENDIF
C
      I = INDEX(TIME,': ')
      IF ( I .GT. 0 ) THEN
        TIME = TIME(1:I)//'0'//TIME(I+2:)
      ENDIF
C
      DELTA_TIME = TIME
  999 RETURN
      END
