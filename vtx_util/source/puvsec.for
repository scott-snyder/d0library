      SUBROUTINE PUVSEC(ILAYV,ISECV)
C-----------------------------------------------------------------------
C- SUBROUTINE PUVSEC(ILAYV,ISECV) eliminates the unused portion of
C- bank VSEC after it has been filled.
C- 
C- T. Trippe, 7 Jan. 1987
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:VTXLNK.INC/LIST'
      INTEGER ILAYV,ISECV,NHITS,NWVSEC
      INTEGER NPTWHT,NWDSHT,MXVSEC
C
      IF(LVSEC(ISECV,ILAYV).NE.0) THEN
         NHITS =IQ(LVSEC(ISECV,ILAYV)+1)
         NPTWHT=IQ(LVSEC(ISECV,ILAYV)+2)
         NWDSHT=IQ(LVSEC(ISECV,ILAYV)+3)
         NWVSEC=3+NPTWHT*2+NWDSHT*NHITS   ! number of data words used
         MXVSEC=IQ(LVSEC(ISECV,ILAYV)-1)  ! number of data words booked
         CALL MZPUSH(IXCOM, LVSEC(ISECV,ILAYV),0,NWVSEC-MXVSEC,'R')
      ENDIF
C
      RETURN
      END
