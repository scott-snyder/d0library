      SUBROUTINE VTX_LUMDTM(LUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Search through the VDTM linear structures to find the
C-           banks which most closely match the luminosity.  Move the best match
C-           to the front of the chain (if it is not already there).  This 
C-           routine is called by VTX_LUMADJ and is to be used ONLY PRIOR TO
C-           DYNAMIC HV ADJUSTMENT
C-
C-   Inputs  : LUM Luminosity in units of 10E30
C-   Outputs : 
C-   Controls: 
C-
C-   Created  24-DEC-1992   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC' 
      INCLUDE 'D0$LINKS:IZVTMW.LINK'
      INCLUDE 'D0$LINKS:IZVDTM.LINK'
c I/O:
      REAL LUM
C Locals
      INTEGER LAYER,SECTOR,NUM_CATEG(0:2),NCAT,CATEG,NS(0:2)
      INTEGER LVTMW,LVDTM,ITEMS,OFFSET,ERR
      INTEGER MIN,ILUM,LLUM,INDX,LBEST,ILUMBEST,INDBEST
      INTEGER LEN
      CHARACTER*80 TXT
      LOGICAL FIRST,DONE(0:31)
c Externals:
      INTEGER TRULEN,GZVDTM
c Data:
      DATA FIRST/.TRUE./
      DATA NS/15,31,31/
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('NUM_CATEG',NUM_CATEG(0),ERR)
        CALL EZRSET
      ENDIF
C
C ****  normal entry
C
      WRITE(TXT,'(A,F5.2)')' LUMINOSITY = ',LUM
      LEN = TRULEN(TXT)
      CALL ERRMSG('VTX Luminosity','VTX_LUMDTM',TXT(1:LEN),'I')
      ILUM = NINT(100.*LUM)
      DO LAYER = 0,2
        DO CATEG = 0,NUM_CATEG(LAYER) - 1
          DONE(CATEG) = .FALSE.
        ENDDO
        NCAT = 0
        LVTMW = LC(LVTMH - IZVTMW - LAYER)
        ITEMS = IC(LVTMW+3)
        OFFSET= LVTMW + 6 + 8*ITEMS*IC(LVTMW+5)
        DO SECTOR = 0,NS(LAYER)
          CATEG = IC(OFFSET+SECTOR)
          IF (.NOT. DONE(CATEG)) THEN
            DONE(CATEG) = .TRUE.
            NCAT = NCAT + 1
            LVDTM = LC(LVTMW - (IZVDTM+CATEG) )
C
C ****  Search through linear structure for best DTM.  IDN = IC(LVDTM-5) 
C ****    is the luminosity in units of 10E28
C
            MIN = 99999
            INDX = 0
            DO WHILE (LVDTM .GT. 0)
              LLUM = IC(LVDTM-5)
              IF (ABS(LLUM-ILUM) .LT. MIN) THEN
                MIN = ABS(LLUM-ILUM)
                LBEST = LVDTM
                ILUMBEST = LLUM
                INDBEST = INDX
              ENDIF
              INDX = INDX + 1
              LVDTM = LC(LVDTM)
            ENDDO 
            IF (INDBEST .NE. 0) THEN
              CALL ZSHUNT(IXSTP,LBEST,LVTMW,-IZVDTM-CATEG,0)
              IF (LAYER .EQ. 0) THEN
                WRITE(TXT,'(A,F5.2,A,F5.2,A,I2)') ' Luminosity = ',LUM,
     &               '; Now using DTM file for lum = ',ILUMBEST/100.,
     &               ' for categ ',CATEG
                LEN = TRULEN(TXT)
                CALL ERRMSG('VTX new DTM','VTX_LUMDTM',TXT(1:LEN),
     &            'I')
              ENDIF
            ENDIF
            IF (NCAT .GE. NUM_CATEG(LAYER)) GO TO 50
          ENDIF
        ENDDO
   50 ENDDO
  999 RETURN
      END
