      FUNCTION DTRUSM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Output summaries. Mainly write new set of constants
C-               if CDSURV was requested, and then a list of histo content.
C-
C-   Outputs : none
C-
C-   Created  21-JAN-1988   Olivier Callot
C-   Updated   6-SEP-1988   Qizhong Li-Demarteau   special treatment for
C-                                               average/sigma histograms
C-   Updated  12-JUL-1989   Qizhong Li-Demarteau  from CDCUSM to DTRUSM 
C-   Updated  24-OCT-1989   Qizhong Li-Demarteau  modified to fit Hbook4 calls 
C-   Updated   1-NOV-1991   Qizhong Li-Demarteau  moved LOGICAL from routine
C-                                         name to Type Declaration Statement
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CDPARA.INC'
C
      LOGICAL DTRUSM
      LOGICAL HSTFLG
      INTEGER USUNIT
      EXTERNAL USUNIT
      CHARACTER*80 TITLE
      INTEGER IUNIT, IDVECT(1000),NUMHIS,NUMENT,NX, N, LOC
      INTEGER UNDERF,OVERF,YDUM,MWOR,I,J, MAXBIN, RUNNO
      PARAMETER( MAXBIN= 200)
      REAL  MEAN, SIGMA, XLO, XHI, YDUM1, YDUM2, HSTATI, HI
      REAL  HBKMED, RAP, BINCON(0:MAXBIN+1), SUM
C-----------------------------------------------------------
      IUNIT = USUNIT()
      CALL CDCLOS( IUNIT )
      DTRUSM = .TRUE.
C
      CALL DHSTOU(HSTFLG)   ! to deal with histograms booked at user dialog
C
      IF (HSTFLG) THEN 
        CALL HOUTPU(IUNIT)
        CALL HINDEX
      ELSE
C
C ****  Write statistic on histograms, one line/histo
C
        WRITE(IUNIT, 1001) RUNNO()
        WRITE(IUNIT, 1000)
        CALL HID1(IDVECT,NUMHIS)
        DO 10 I=1,NUMHIS
          CALL HNOENT(IDVECT(I),NUMENT)
          MEAN  = HSTATI(IDVECT(I),1,' ',1)
          SIGMA = HSTATI(IDVECT(I),2,' ',1)
          CALL 
     &      HGIVE(IDVECT(I),TITLE,NX,XLO,XHI,YDUM,YDUM1,YDUM2,MWOR,LOC)
          UNDERF = HI(IDVECT(I),0)
          OVERF  = HI(IDVECT(I),NX+1)
C
C ****  Computes median
C
          HBKMED = XLO-1.
          IF( NX .LE. MAXBIN ) THEN
            CALL HUNPAK(IDVECT(I), BINCON(1), ' ',1)
            BINCON( 0 ) = UNDERF
            BINCON( NX+1 ) = OVERF
            SUM = 0.
            DO 15 N = 0, NX+1
              SUM = SUM + BINCON(N)
   15       CONTINUE
            IF ( SUM .GT. 0. ) THEN
              SUM = .5 * SUM
              DO 20 N = 0, NX+1
                SUM = SUM - BINCON(N)
                IF ( SUM .LT. 0. ) THEN
                  RAP = FLOAT(N) + SUM / BINCON(N)
                  HBKMED = XLO + RAP*(XHI-XLO)/FLOAT(NX)
                  GOTO 30
                ENDIF
   20         CONTINUE
            ENDIF
          ENDIF
   30     CONTINUE
          WRITE (IUNIT,1200) TITLE(1:30) ,IDVECT(I), NUMENT,
     &         MEAN, SIGMA, HBKMED, NX, XLO, XHI, UNDERF, OVERF
   10   CONTINUE
      ENDIF
  999 RETURN
C
 1001 FORMAT (10X,'Histogram statistic for run ',I10,/
     &       8X,'=====================================',//)
 1000 FORMAT (
     &  ' Title',30x,'ID  Entries     Mean       Sigma      Median    ',
     &  '  Nbin     Low    High Under  Over',/)
 1100 FORMAT (20A4)
 1200 FORMAT (' ',A30,I7,I8,2X,3G12.5,I6,F8.1,F8.1,I6,I6)
      END
