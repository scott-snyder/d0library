      REAL FUNCTION CTCOR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : For each date this rountine return the canary
C-                         correction (cannary is corrected by the
C-                         temperature)
C-                         reference year : 1992 (only for run 1a)
C-
C-   Returned value  : normalized canary correction.
C-                     = -9999. unknown date
C-   Inputs  : IDATE (YYMMJJ)  ITIME (HHMMSS) BOTH INTEGERS
C-   Controls:
C-
C-   Created  22-APR-1994   L.CHEVALIER
C-   Updated  20-JUN-1994   Alain PLUQUET   read canary fit from TCY1 bank
C-                                          (after unpacking)
C_
C-   Updated  22-JUN-1994   Laurent CHEVALIER removed date,time as arguments
C-   Updated   8-NOV-1994   A. ZYLBERSTEJN   : check if date of run is prior to
C-                                             may 1 st 93
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTCY1.LINK'
      INTEGER LTCY1,LNEXT,GZTCAN
      INTEGER ILOOP,NDIM
      INTEGER IDAT,ITIM,RUNNO
      INTEGER TIME_OF_RUN(2)
      INTEGER*2 UNPACKED_TIME(7)
      INTEGER YEAR,MONTH,DAY,HOUR,MINUTE,SECOND
      REAL    PAS,DREF,DATINF,DATSUP,DJ0
      PARAMETER( NDIM = 8000   )
      PARAMETER( PAS  = 2.6315127E-02 )
      PARAMETER( DREF = 0.0210  )
      PARAMETER( DJ0  = 273.9583)
      REAL    ENRJCAN(NDIM),QJR
      LOGICAL OK,SYS$NUMTIM
      LOGICAL FIRST
      EXTERNAL SYS$NUMTIM
      DATA FIRST /.TRUE./

      IF (FIRST) THEN
        FIRST=.FALSE.
        LTCAN=GZTCAN()
        LTCY1=LC(LTCAN-IZTCY1)
        CALL UNPACK_REAL_STP(ENRJCAN,10000.,1,NDIM,2,LTCY1+1,LNEXT)
      ENDIF
      TIME_OF_RUN(1) = IQ(LHEAD+4)
      TIME_OF_RUN(2) = IQ(LHEAD+5)
      OK     = SYS$NUMTIM(UNPACKED_TIME(1),TIME_OF_RUN(1))
      YEAR   = UNPACKED_TIME(1) - (UNPACKED_TIME(1)/100)*100
      MONTH  = UNPACKED_TIME(2)
      DAY    = UNPACKED_TIME(3)
      IDAT   = 10000*YEAR+100*MONTH+DAY
      HOUR   = UNPACKED_TIME(4)
      MINUTE = UNPACKED_TIME(5)
      SECOND = UNPACKED_TIME(6)
      ITIM   = HOUR*10000+100*MINUTE+SECOND
      CALL CLDR(92,IDAT,ITIM,QJR)
      QJR         = QJR - DJ0
      CTCOR       = -9999.
      IF(QJR.GT.210.)GO TO 999
      DO ILOOP    = 1 , NDIM-1
        DATINF    = DREF + ILOOP * PAS
        DATSUP    = DREF + (ILOOP + 1) * PAS
        IF(QJR.GT.DATINF.AND.QJR.LE.DATSUP) THEN
          CTCOR   = ENRJCAN(ILOOP)
          GOTO    999
        ENDIF
      ENDDO
C----------------------------------------------------------------------
  999 RETURN
      END

