      SUBROUTINE ROUND_FLOATING(FLOATING_BITS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Round floating point numbers in the Zebra banks.
C-                         Non-fractional bits are never rounded.
C-
C-   Inputs  : FLOATING_BITS [I] Number of significant bits to keep.
C-   Outputs :
C-   Controls:
C-
C-   Created  13-Oct-1994 Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INTEGER FLOATING_BITS
      INTEGER L, LZSCAN
      INTEGER I, ND, ID
      INTEGER NC, NCW, LC
      CHARACTER*256 FORM
      CHARACTER*1 TYPE
      REAL PRECISION, VALUE, SCALE
C----------------------------------------------------------------------
      PRECISION = 2.**FLOATING_BITS
C-
C- Visit each Zebra bank.
C-
      L = 0
 10   CONTINUE
      L = LZSCAN(IXMAIN, L)
      IF(L.EQ.0)GO TO 999
      IF(IQUEST(1).EQ.1)GO TO 10
      IF(IQUEST(1).NE.0)GO TO 999
C- 
C- Process live banks here.
C-
      ND = IQ(L-1)
      ID = 1
C-
C- Get I/O characteristic.
C-
      CALL MZIOTC(IXCOM, L, NC, FORM)
C-
C- Loop over words in I/O characteristic.
C-
      DO WHILE(NC.GT.0)
        DO WHILE(NC.GT.0.AND.FORM(1:1).EQ.' ')
          FORM(1:NC-1) = FORM(2:NC)
          NC = NC - 1
        ENDDO
        NCW = INDEX(FORM(1:NC),' ')-1
        IF(NCW.LT.0)NCW = NC
C-
C- Extract the length and type fields of this word.
C-
        TYPE = FORM(NCW:NCW)
        IF(FORM(1:1).EQ.'*' .AND. NCW.EQ.2)THEN
          LC = IQ(L+ID)
          ID = ID + 1
        ELSEIF(FORM(1:1).EQ.'-' .AND. NCW.EQ.2)THEN
          LC = ND-ID+1
        ELSE
          LC = 0
          DO I = 1,NCW-1
            IF(FORM(I:I).LT.'0' .AND. FORM(I:I).GT.'9')GO TO 1000
            LC = 10*LC + ICHAR(FORM(I:I)) - ICHAR('0')
          ENDDO
        ENDIF
        LC = MIN0(LC, ND-ID+1)
C-
C- Various types handled here.
C-
        IF(TYPE.EQ.'S')THEN
          GO TO 999
        ELSEIF(TYPE.EQ.'/')THEN
          FORM(NC+1:2*NC) = FORM(1:NC)
          NC = 2*NC
        ELSEIF(TYPE.EQ.'F')THEN
          DO I = 1,LC
            VALUE = Q(L+ID)
            IF(VALUE.NE.0.)THEN
              SCALE = 1.
              DO WHILE(ABS(VALUE).LT.PRECISION)
                VALUE = 2.*VALUE
                SCALE = 0.5*SCALE
              ENDDO
              VALUE = NINT(VALUE) * SCALE
              Q(L+ID) = VALUE
            ENDIF
            ID = ID + 1
          ENDDO
        ELSEIF(TYPE.EQ.'I' .OR. TYPE.EQ.'B' .OR. TYPE.EQ.'U' .OR.
     &         TYPE.EQ.'H' .OR. TYPE.EQ.'D')THEN
          ID = ID + LC
        ELSE
          GO TO 1000
        ENDIF
        IF(ID.GT.ND)GO TO 10
C-
C- Advance to next word.
C-
        FORM(1:NC-NCW-1) = FORM(NCW+2:NC)
        NC = NC-NCW-1
      ENDDO
      GO TO 10
C-
C- Come here if we fail to crack the I/O characteristic.
C-
 1000 CONTINUE
      CALL ERRMSG('Failed to crack I/O characteristic',
     &  'ROUND_FLOATING', ' ', 'F')
  999 RETURN
      END
