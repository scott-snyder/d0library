      SUBROUTINE CAL_UNPACK_D0PULSER(PULSERS,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Unpack calorimeter pulser status words from the
C-                          trigger bank
C-
C-   Inputs  : none
C-   Outputs : PULSERS(1:14)     pulser word for all cal pulsers
C-               IER=0           if ok
C-               IER=1           if cannot read trigger bank
C-               IER=2           if cannot find pulser block
C-               IER=3           if cannot read pulserword
C-   Controls: none
C-
C-   Created  24-APR-1992   Joan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZTRGR.LINK'
C
      INTEGER IER,LTRG,LTRG1,NHEAD,NDLV1,POINT,ITRG_VSN
      INTEGER IPBOX,PULSERS(*)
      INTEGER GZTRGR,GZFIND_CRATE
      CHARACTER*80 MSG_STRING
      integer z80
      data z80 / z'80' /
      integer zf000
      data zf000 / z'f0000' /
C
C----------------------------------------------------------------------
C
      IER=0
C
      LTRG= GZTRGR()
C
      IF (LTRG.EQ.0)THEN
        WRITE(MSG_STRING,100)LTRG
  100   FORMAT(' ERROR IN POINTER,LTRG= ',I5)
        CALL ERRMSG('CAL_UNPACK','CAL_UNPACK_D0PULSER',MSG_STRING,'W')
        IER=1
        GO TO 999
      ENDIF
      LTRG1= GZFIND_CRATE('TRGR',LTRG,11) - 1
      IF (LTRG1.EQ.0)THEN
        WRITE(MSG_STRING,100)LTRG1
        CALL ERRMSG('CAL_UNPACK','CAL_UNPACK_D0PULSER',MSG_STRING,'W')
        IER=1
        GO TO 999
      ELSE
        ITRG_VSN=IQ(LTRG1+4)
        IF (ITRG_VSN.LT.4)THEN
          NHEAD=IQ(LTRG1+1) + 1        !NUMBER OF HEADER WORDS
          NDLV1=IQ(LTRG1+NHEAD+1)      !NUMBER OF LEVEL 1 DATA WORDS
          POINT=NDLV1+NHEAD+64+2       !LOCATION OF PULSER BLOCK
        ELSEIF (ITRG_VSN.LT.6)THEN
          POINT=2527                   !LOCATION OF PULSER BLOCK
        ELSE
          POINT=2715                   !LOCATION OF PULSER BLOCK
          IF (IQ(LTRG1+POINT) .EQ. z80) THEN
            POINT = POINT+64            !LOCATION OF CAL PULSERS
          ELSE
            IER=2
            WRITE(MSG_STRING,101)
  101       FORMAT(' CANNOT FIND PULSER BLOCK')
            CALL ERRMSG('CAL_UNPACK','CAL_UNPACK_D0PULSER',
     &        MSG_STRING,'W')
            GO TO 999
          ENDIF
        ENDIF
        DO IPBOX=1,14
          PULSERS(IPBOX)=IQ(LTRG1+POINT+IPBOX)
          IF(PULSERS(IPBOX).EQ.-1.OR.PULSERS(IPBOX).EQ.zF000)THEN
            IER=3
            WRITE(MSG_STRING,102)IPBOX-1,PULSERS(IPBOX)
  102       FORMAT(' CANNOT READ PULSERWORD FOR PULSER ',Z1,1X,Z8)
            CALL ERRMSG('CAL_UNPACK','CAL_UNPACK_D0PULSER',
     &        MSG_STRING,'W')
            GO TO 999
          ENDIF
        ENDDO
      ENDIF
C
  999 RETURN
      END
