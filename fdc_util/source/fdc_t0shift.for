C VAX/DEC CMS REPLACEMENT HISTORY, Element FDC_T0SHIFT.FOR
C *1     4-NOV-1993 10:52:59 AVERY "FDC changes for v12 RECO"
C VAX/DEC CMS REPLACEMENT HISTORY, Element FDC_T0SHIFT.FOR
      SUBROUTINE FDC_T0SHIFT(T0SHIFT,FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Shift the values of T0 stored in FDC STP banks
C-      by a constant amount (T0SHIFT) and apply to all FDC hits.
C-
C-   Inputs  : T0SHIFT,         amount of shift.
C-             FLAG             =0, SHIFT, =1 set to T0SHIFT
C-   Outputs : contents of FTSE,LFTDA,LFPDA banks
C-
C-   Created  28-MAY-1993   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      REAL    T0SHIFT
      INTEGER FLAG
C
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE
      INTEGER MAX_QUAD(0:1), MAX_SECT(0:1)
      INTEGER LFXDA,GZFXDA
      INTEGER LFTSE,GZFTSE
      INTEGER IPTR, LHIT
      INTEGER NEL,NWORDS
C
      REAL    TZERO_OLD 
C
      DATA MAX_QUAD, MAX_SECT
     &  / MXQUAD, 0, MXSECT, MXSECP/
C----------------------------------------------------------------------
      DO HALF = 0,1
        DO UNIT = 0,1
          DO QUAD =  0,MAX_QUAD(UNIT)
            DO  SECTOR =  0, MAX_SECT(UNIT)
C
C Modify time shift:
C
              LFTSE=GZFTSE(HALF,UNIT,QUAD,SECTOR)
              NEL = IC(LFTSE+3)
              NWORDS = IC(LFTSE+4)
              DO WIRE =  0, NEL-1
                IPTR = LFTSE + WIRE*NWORDS   + 6
                IF ( FLAG.EQ.0 ) THEN
                  TZERO_OLD = C(IPTR + 2) 
                ELSE
                  TZERO_OLD = 0.0
                ENDIF
                C(IPTR + 2) = TZERO_OLD + T0SHIFT
              ENDDO
C
C Apply to hits:
C
              LFXDA=GZFXDA(HALF,UNIT,QUAD,SECTOR)
              IF(LFXDA.GT.0) THEN
                IF ( UNIT.EQ.0 ) THEN
                  CALL FTSC_REFILL(HALF,QUAD,SECTOR)
                ELSE
                  CALL FPSC_REFILL(HALF,SECTOR)
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
  999 RETURN
      END
