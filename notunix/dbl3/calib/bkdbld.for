      SUBROUTINE BKDBLD(BNKNAM,LDBLD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book a dummy bank for DBL3.
C-
C-   Inputs  : BNKNAM = Name of Bank. If ' ', BNKNAM = 'DBLD' (Default)
C-   Outputs : LDBLD = Return address
C-   Controls:
C-
C-   Created  16-JAN-1990   Jan Guida, Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER LDBLD
      INTEGER NL,NS,ND,NIO
      INTEGER I,J,K,JDATE,JTIME
      CHARACTER*8 STIME
      CHARACTER*4 BNKNAM
      DATA NL,NS,ND,NIO /5,5,10,2/
C
C----------------------------------------------------------------------
C
      IF (BNKNAM.EQ.' ') BNKNAM = 'DBLD'
C
C   *** Book a standalone bank in ZEBSTP >
C
      CALL MZBOOK(IDVSTP,LDBLD,LDBLD,2,BNKNAM,NL,NS,ND,NIO,0)
C
      CALL IDATE(I,J,K)
      JDATE=I*10000+J*100+K
      CALL TIME(STIME)
      READ(STIME,'(I2,1X,I2,1X,I2)')I,J,K
      JTIME=I*10000+J*100+K
C
C  *** Fill up header for dummy bank >
C
      IF (BNKNAM.NE.'CLBH') THEN
        IC(LDBLD+3)=1                     ! Quality factor = Bad.
        IC(LDBLD+4)=1
      ELSE
        IC(LDBLD+1) = 1                   ! Version Number
        IC(LDBLD+2) = 0                   ! Store Histogram Flag
        IC(LDBLD+3) = NL                  ! Number of Links requested
        IC(LDBLD+4) = 0                   ! Number of Histograms
      ENDIF
      IC(LDBLD+5)=999999999                 ! End validity
      IC(LDBLD+6)=1                       ! Start validity
      IC(LDBLD+7)=JDATE                   ! Date
      IC(LDBLD+8)=JTIME                   ! Time
      IC(LDBLD+9)=0                       ! Crate number
C
  999 RETURN
      END
