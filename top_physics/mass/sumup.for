      SUBROUTINE SUMUP(ID,XLOLIM,XHILIM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Paw routine SUMS up histograms
C-   and gives cumulative Histogram. Also prints out the
C-   point at which the  limit given by XLOLIM is reached.
C-   CALLABLE from PAW by CALL SUMUP.FOR(ID)
C-
C-   Inputs  : ID = Paw iD number of histogram to integrate
C-             XLOLIM ,xhilim limits when exceeeded, Used to find twosided
C-             confidence levels of the histogram
C-             routine prints message
C-   Outputs :
C-   Controls:
C-
C-   Updated  12-JUN-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*132 TITLE,TITLEI,CTIT
      REAL Y(0:200),CUM(0:200)
      REAL    SUM
      INTEGER I,ILO,IHI,ID,NX,NY,LOC,NWT,NENT,IMAX
      REAL    XLOLIM,XHILIM
      REAL    XLO,XHI,HI
      REAL    X1,X2,Y1,Y2
      REAL    YMAX,XMAX,DELX
      INTEGER SSUNIT,LOUT
      INTEGER KUN,IER
      CHARACTER*80 KUMAC_FILE
      INTEGER TRNLNM ,TRULEN
      CHARACTER*80 DILEPTON
      INTEGER LEND
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST / .TRUE. /

C----------------------------------------------------------------------
      IF( FIRST ) THEN
        FIRST = .FALSE.
        LOUT = SSUNIT()
        CALL EZ_FILE_OPEN(1,'KUMAC','OF',KUN,
     &    KUMAC_FILE,IER)

      ENDIF
C
C
      ILO = 0
      IHI = 0
C
      XLO = 0.0
      XHI=0.0
C
      CALL HGIVE(ID, TITLE,NX,X1,X2,NY,Y1,Y2,NWT,LOC)
      WRITE(LOUT,1)
    1 FORMAT(//)
      WRITE(LOUT,*) ' INTEGRATING ID ',ID,' ',TITLE(1:4*NWT)
      CALL HNOENT(ID,NENT)
      WRITE(LOUT,*) ' number of entries',NENT
C
      DO 110 I = 0 , 200
        CUM(I)=0
        Y(I)=0
  110 CONTINUE
C
      CUM(0) =  HI(ID,0)
      CALL HUNPAK(ID,Y,' ',0)
C
      CALL GET_LIMITS(XLOLIM,XHILIM,X1,X2,NX,Y,
     &  NENT,CUM,SUM,XLO,XHI,XMAX,YMAX, ILO,IHI,IMAX)
C
      WRITE(LOUT,*)XLOLIM, ' Limit reached at X= ',XLO,' BIN = ',ILO
      WRITE(LOUT,*)XHILIM, ' Limit reached at X= ',XHI,' BIN = ',IHI
      WRITE(LOUT,*)' MAXIMUM VALUE AT X= ',XMAX,' Y=',YMAX,' BIN=',IMAX
C
      CALL TRNLNM('DILEPTON',DILEPTON,LEND)
      WRITE(KUN,*)'h/file 1 ', DILEPTON(1:TRULEN(DILEPTON)) , ' 0'
      WRITE(KUN,*)'cd top_mass'
      WRITE(KUN,*)'title_global ',DILEPTON(1:TRULEN(DILEPTON))
      WRITE(KUN,*)'ID=',ID
      WRITE(KUN,*)'SET HTYP 158'
      WRITE(KUN,*)'H/PL ',ID
      WRITE(KUN,*)'SET HTYP 144'
      WRITE(KUN,201)'H/PL',ID,'(',ILO,':',IHI,') S'
  201 FORMAT(A4,1X,I4,A1,I3.3,A1,I3.3,A3)
      WRITE(KUN,202)
     &'TEXT 180.0 ',0.9*YMAX,
     &' Maximum likelihood value ',XMAX,' Gev/c^2',' 0.3'
  202 FORMAT(A11,1X,E8.3,1X,1H',A26,F5.1,A8,1H',A4)
      WRITE(KUN,203)
     &  'TEXT 180.0 ',0.8*YMAX,' Maximum likelihood = ',YMAX,' 0.3'
  203 FORMAT(A11,1X,E8.3,1X,1H',A21,E8.3,1H',A4)
      WRITE(KUN,204)
     &  'TEXT 180.0 ',0.7*YMAX,' 90"Y# CL ',XLO,XHI,' Gev/c^2',' 0.3'
  204 FORMAT(A11,1X,E8.3,1X,1H',A10,F5.1,'-',F5.1,A8,1H',A4)
C
      WRITE(KUN,205)
  205 FORMAT(' CLOSE(UNIT=1)',//)
C
  999 RETURN
      END

