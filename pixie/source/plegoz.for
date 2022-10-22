      SUBROUTINE PLEGOZ
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make LEGO plot from LEGO ZEBRA bank
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  13-NOV-1988   Sharon Hagopian
C-   Updated   4-JAN-1990   Lupe Howell Using color table
C-   Updated   5-FEB-1991   Lupe Howell  Parameter array name changed to the 
C-      generic name 'PXPARAMS' 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      INTEGER NX,NY,IMARK
      REAL XMIN,XMAX,YMIN,YMAX,ZMAX
      CHARACTER*3 COL1
      CHARACTER*4 XLAB,YLAB,ZLAB
      CHARACTER*40 PLTITL
      INTEGER NXMIN,NYMIN,N
      INTEGER NXG,NYG
      REAL ZSCAL
      REAL ZCUT   ! MINIMUM Value of Z for value to appear in LEGO plot
      LOGICAL GRDFLG,EZERROR
      INTEGER MXLEGO
      PARAMETER (MXLEGO=10000)
      REAL ARRAY1(MXLEGO)
      INTEGER NLEGOS
      INTEGER GZLEGO
      INTEGER NLEG ! Number of LEGO plots booked
      INTEGER ILEG ! Bank number of LEGO bank to be plotted
      INTEGER LL ! index of first word in LEGO ZEBRA bank
      INTEGER NTRP ! Number of data triplets in bank (x,y,z)     
      REAL X1,X2 ! Begining and end of LEGO bin in x
      REAL Y1,Y2 ! Begining and end of LEGO bin in y
      REAL DELX,DELY ! Bin widths in x and y
      INTEGER IX,IY ! LOOP indices for x and y
      INTEGER IP ! LOOP index over triplets of data
      INTEGER IER,IVAL,TYP,II,JJ
      REAL XD,YD,ZD ! Triplet of data points
      REAL NSTOR ! INDEX of stored bin
      INTEGER LEN
      CHARACTER*24 IMESS1
      CHARACTER*24 IMESS2
      CHARACTER*24 IMESS3
      CHARACTER*8 CVAR
      CHARACTER*32 MESS
      CHARACTER*60 PROM
      CHARACTER*80 STRING
      CHARACTER*4 REM,CVAL
C  Data Statements:
C =================
      DATA IMARK/0/
      DATA IMESS1/'Number of LEGO banks='/
      DATA IMESS2/'  Z CUT ='/ 
      DATA IMESS3/' ERROR in LEGO bank no.'/
      DATA XLAB,YLAB,ZLAB/' X  ',' Y  ',' Z  '/
      DATA ILEG/1/
      DATA PLTITL/' LEGO BANK                       '/
      DATA PROM/' Enter LEGO bank no. >'/
C----------------------------------------------------------------------
C  Executable Code:
C  ================
C
C ****  Picking PIXIE RCP 
C
      CALL EZPICK('PX_USER_LEGO_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PLEGOZ','Cannot find PX_USER_LEGO_RCP','W')
        GOTO 999
      ENDIF
C
C ****  Get number of x bins to be plotted
C
      CALL EZ_GET_ARRAY_i('PXPARAMS','LEGO NX',1,NX,CVAL,
     &       TYP,REM,IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('PIXIE','PLEGOZ','PXPARAMS NOT FOUND','W')
        GOTO 900
      ENDIF
      CALL EZ_GET_ARRAY('PXPARAMS','LEGO XMIN',1,XMIN,CVAL,
     &       TYP,REM,IER)
      CALL EZ_GET_ARRAY('PXPARAMS','LEGO XMAX',1,XMAX,CVAL,
     &       TYP,REM,IER)
      CALL EZ_GET_ARRAY_i('PXPARAMS','LEGO NY',1,NY,CVAL,
     &       TYP,REM,IER)
      CALL EZ_GET_ARRAY('PXPARAMS','LEGO YMIN',1,YMIN,CVAL,
     &       TYP,REM,IER)
      CALL EZ_GET_ARRAY('PXPARAMS','LEGO YMAX',1,YMAX,CVAL,
     &       TYP,REM,IER)
      CALL EZ_GET_ARRAY('PXPARAMS','LEGO ZCUT',1,ZCUT,CVAL,
     &       TYP,REM,IER)
      CALL EZ_GET_ARRAY('PXPARAMS','LEGO ZMAX',1,ZMAX,CVAL,
     &       TYP,REM,IER)
      CALL EZ_GET_ARRAY_i('PXPARAMS','LEGO XTITL',1,IVAL,XLAB,
     &       TYP,REM,IER)
      CALL EZ_GET_ARRAY_i('PXPARAMS','LEGO YTITL',1,IVAL,YLAB,
     &       TYP,REM,IER)
      CALL EZ_GET_ARRAY_i('PXPARAMS','LEGO ZTITL',1,IVAL,ZLAB,
     &       TYP,REM,IER)
      CALL EZ_GET_ARRAY_l('PXPARAMS','LEGO ALLGRID',1,GRDFLG,
     &       CVAL,TYP,REM,IER)
      COL1= 'RED'  ! Red 
      NXMIN=1
      NYMIN=1
      IF(.NOT.GRDFLG)THEN
        NXG=1
        NYG=1
      ELSE
        NXG=NX
        NYG=NY
      ENDIF
      N=NX
      ZSCAL=.2
      CALL VZERO(ARRAY1,NX*NY)
C Find number of LEGO banks
      NLEG=NLEGOS()
      CALL PXITOC(NLEG,8,CVAR)
      MESS=IMESS1//CVAR
      CALL PUMESS(MESS)
C Get LEGO bank number user wishes to plot
      CALL OUTMSG('1')
      CALL GETPAR(1,PROM,'U',STRING)
      CALL SWORDS(STRING,II,JJ,LEN)
      IF(LEN.NE.0)THEN
         READ(STRING(1:LEN),*,ERR=700)ILEG
      ENDIF
      IF(ILEG.LE.0.OR.ILEG.GT.NLEG)GO TO 700
C GET BANK TITLE
      CALL LEGOTT(ILEG,PLTITL)
C GET POINTER TO LEGO BANK
      LL=GZLEGO(ILEG)
      NTRP=IQ(LL+2)
      X1=XMIN
      DELX=(XMAX-XMIN)/FLOAT(NX)
      DELY=(YMAX-YMIN)/FLOAT(NY)
      DO 50 IX=1,NX           
      X2=X1+DELX
      Y1=YMIN
      DO 40 IY=1,NY
      Y2=Y1+DELY
      NSTOR=IX+(IY-1)*NX
      DO 20 IP=1,NTRP
      XD=Q(LL+11+(IP-1)*3)
      YD=Q(LL+12+(IP-1)*3)      
      ZD=Q(LL+13+(IP-1)*3)
      IF(XD.GE.X1.AND.XD.LT.X2)THEN
        IF(YD.GE.Y1.AND.YD.LT.Y2)THEN
          ARRAY1(NSTOR)=ARRAY1(NSTOR)+ZD
        ENDIF
      ENDIF
   20 CONTINUE
   40 Y1=Y2
   50 X1=X2
      N=NX
      CALL P1LEGO(NX,XMIN,XMAX,NY,YMIN,YMAX,ZCUT,ZMAX,PLTITL,
     X    XLAB,YLAB,ZLAB, COL1,ARRAY1,
     X     NXMIN,NYMIN,NXG,NYG,N,ZSCAL,IMARK)
      CALL PXFTOC(ZCUT,CVAR)
      MESS=IMESS2//CVAR
      CALL PUMESS(MESS)
      GO TO 900
  700 CALL PUMESS(IMESS3)  
  900 CONTINUE
C
C ****  Reseting RCP file
C
      CALL EZRSET
  999 RETURN
      END
