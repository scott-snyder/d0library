      LOGICAL FUNCTION UINT_TO_DST
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Unpack a UINT bank of packed integers.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  14-Oct-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZUINT.LINK'
      INTEGER LUINT,LESUM,NFILT,NOBJS_FILT,NTRGR,NOBJS_TRGR
      INTEGER NL15,NOBJS_L15
      INTEGER GZUINT,GZESUM
      CHARACTER*80 ERRTXT
      LOGICAL DO_TRIGGER,DO_FILTER,DO_L15,LOK
C
      LOGICAL FIRST
      INTEGER MASK
      DATA    FIRST/.TRUE./
      SAVE    MASK,FIRST
C-----------------------------------------------------------------------
C
      IF(FIRST) THEN
        MASK=2**16-1
        FIRST=.FALSE.
      ENDIF
C
      UINT_TO_DST=.TRUE.
      DO_TRIGGER=.TRUE.
      DO_FILTER=.TRUE.
      DO_L15=.TRUE.
C
      LUINT=GZUINT()
      IF( LUINT.LE.0 ) THEN
        WRITE(ERRTXT,*) 'No UINT bank found'
        CALL ERRMSG('NO_UINT','UINT_TO_DST',ERRTXT,'W')
        GOTO 999
      ENDIF
C
C  Check for existing ESUM banks...
C
      LESUM=GZESUM('TRGR')
      IF( LESUM.GT.0 ) DO_TRIGGER=.FALSE.
      LESUM=GZESUM('TR15')
      IF( LESUM.GT.0 ) DO_L15=.FALSE.
      LESUM=GZESUM('FILT')
      IF( LESUM.GT.0 ) DO_FILTER=.FALSE.
C
C  Get the sizes of the new ESUM banks...
C
      NTRGR=IAND(IQ(LUINT+1),MASK)
      NOBJS_TRGR=(NTRGR-4)/3
      NL15=IAND(ISHFT(IQ(LUINT+1),-16),MASK)
      NOBJS_L15=(NL15-4)/3
      NFILT=IAND(IQ(LUINT+2),MASK)
      NOBJS_FILT=(NFILT-4)/3
C
C  Unpack the ESUM banks...
C
      LUINT=GZUINT()
      IF( DO_TRIGGER .AND. NTRGR.GT.0 ) THEN
        CALL BKESUM('TRGR',NOBJS_TRGR,LESUM)
        IF(LESUM.GT.0) THEN
          CALL ESUM_UNCOMPRESS(IQ(LUINT+3),IQ(LESUM+1),LOK)
          IF( .NOT.LOK ) THEN
            WRITE(ERRTXT,*) 'Error unpacking compress ESUM(TRGR) bank'
            CALL ERRMSG('UpTrgrErr','UINT_TO_DST',ERRTXT,'E')
          ENDIF
        ELSE
          WRITE(ERRTXT,*) 'Error booking ESUM(TRGR) bank'
          CALL ERRMSG('UpBookErr','UINT_TO_DST',ERRTXT,'E')
        ENDIF
      ELSE
        WRITE(ERRTXT,*) 'Not unpacking TRGR ESUM bank'
        CALL ERRMSG('NoUpTrgr','UINT_TO_DST',ERRTXT,'W')
      ENDIF
C
      IF( DO_L15 .AND. NL15.GT.0 ) THEN
        CALL BKESUM('TR15',NOBJS_L15,LESUM)
        IF(LESUM.GT.0) THEN
          CALL ESUM_UNCOMPRESS(IQ(LUINT+3+NTRGR),IQ(LESUM+1),LOK)
          IF( .NOT.LOK ) THEN
            WRITE(ERRTXT,*) 'Error unpacking compress ESUM(L15) bank'
            CALL ERRMSG('UpTR15Err','UINT_TO_DST',ERRTXT,'E')
          ENDIF
        ELSE
          WRITE(ERRTXT,*) 'Error booking ESUM(TR15) bank'
          CALL ERRMSG('UpBookErr','UINT_TO_DST',ERRTXT,'E')
        ENDIF
      ELSE
        WRITE(ERRTXT,*) 'Not unpacking TR15 ESUM bank'
        CALL ERRMSG('NoUpTR15','UINT_TO_DST',ERRTXT,'W')
      ENDIF
C
      IF( DO_FILTER .AND. NFILT.GT.0 ) THEN
        CALL BKESUM('FILT',NOBJS_FILT,LESUM)
        IF(LESUM.GT.0) THEN
          CALL ESUM_UNCOMPRESS(IQ(LUINT+3+NTRGR+NL15),IQ(LESUM+1),LOK)
          IF( .NOT.LOK ) THEN
            WRITE(ERRTXT,*) 'Error unpacking compress ESUM(FILT) bank'
            CALL ERRMSG('UpFiltErr','UINT_TO_DST',ERRTXT,'E')
          ENDIF
        ELSE
          WRITE(ERRTXT,*) 'Error booking ESUM(FILT) bank'
          CALL ERRMSG('UpBookErr','UINT_TO_DST',ERRTXT,'E')
        ENDIF
      ELSE
        WRITE(ERRTXT,*) 'Not unpacking FILT ESUM bank'
        CALL ERRMSG('NoUpFilt','UINT_TO_DST',ERRTXT,'W')
      ENDIF
C
 999  RETURN
      END

