      SUBROUTINE PVTRAK(DRWLBL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draws the tracks found in the reconstruction
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  31-OCT-1988   Ghita Rahal-Callot
C-   Updated  28-NOV-1988   Ghita Rahal-Callot  : Draw the track from the first
C-                                                to the last wire fired
C-   Updated   8-AUG-1991   An-Dien Nguyen draw tk. no. in r-phi view
C-       parallel version   S. Hagopian 
C-   Updated  16-AUG-1991   T. Trippe  combine versions
C-   Updated   4-SEP-1991   S. Hagopian, add LEGEND_ROAD; counter NROAD
C-                          changed ZTRAKS.RCP to ZTRAKS.PARAMS  
C-   Updated  24-JAN-1992   Lupe Howell  Remove machine blocks 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVTXT.LINK'
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS/LIST'
C
      INTEGER GZVTRH
      INTEGER LVRFT, LVTXT, LVTRH
      REAL A, B, CE, R, RMIN, RMAX, DEL, SS, PHI, XG, YG
      REAL XHPOS, YHPOS, XTRKNM, YTRKNM, XOFF, YOFF
      REAL XMIN, XMAX, YMIN, YMAX, XSIZ, YSIZ, SIZSCAL
      INTEGER IWFIRS, IWLAST, ILFIRS, ILLAST, IXV(32), NXV
      INTEGER ITRK, DRWLBL
      CHARACTER*4 TRKNUM
      INTEGER ICALL,IER
      INTEGER NROAD
      LOGICAL IDEBUG
      LOGICAL INEL,INMU,INTAU,INVEE
      DATA IDEBUG/.FALSE./
      SAVE ICALL
      DATA ICALL/0/
C----------------------------------------------------------------------
      LVRFT = LC( LVGEH - 3 )
      CALL PUOPEN
      XOFF = 2.0   
      YOFF = 2.0   
      LVTRH = GZVTRH()
      IF(IDEBUG)THEN
        WRITE(41,*)' LVTRH=',LVTRH
      ENDIF
      IF ( LVTRH .LE. 0 ) GO TO 91
      LVTXT = LQ ( LVTRH - IZVTXT )
      IF(IDEBUG)THEN
        WRITE(41,*)' LVTXT=',LVTXT
      ENDIF
   90 CONTINUE
      IF ( LVTXT .LE. 0 ) GO TO 91
C set color for tracks in roads depending on particle type
      INMU=(IBITS(IQ(LVTXT),MUROAD,1).EQ.1)
      INEL=(IBITS(IQ(LVTXT),ELROAD,1).EQ.1)
      INTAU=(IBITS(IQ(LVTXT),TAUROAD,1).EQ.1)
      INVEE=(IBITS(IQ(LVTXT),9,1).EQ.1)
      IF(IDEBUG)THEN
        WRITE(41,*)' INMU',INMU,' INEL',INEL,' INTAU',INTAU
      ENDIF
      CALL PXCOLR('FOR')
      IF(INMU)THEN
        CALL PXCOLR('GRE')
        NROAD=NROAD+1
      ENDIF
      IF(INEL)THEN
        CALL PXCOLR('RED')
        NROAD=NROAD+1
      ENDIF
      IF(INTAU)THEN
        CALL PXCOLR('CYA')
        NROAD=NROAD+1
      ENDIF
      IF(INVEE)THEN
        CALL PXCOLR('YEL')
        NROAD=NROAD+1
      ENDIF
C ****  Get the first and last wire fired
C
      CALL UBITS ( IQ(LVTXT+3), 32, IXV, NXV)
      IWFIRS = IXV(1) - 1
      IWLAST = IXV(NXV) - 1
      ILFIRS = IWFIRS/8
      ILLAST = IWLAST/8
      IWFIRS = MOD ( IWFIRS, 8)
      IWLAST = MOD ( IWLAST, 8)
      R      = SQRT(C(LVRFT+23+IWFIRS)**2 + C(LVRFT+31+IWFIRS)**2)
      IF ( IWFIRS .LE. 3 ) R = - R
      RMIN = C(LVRFT+7+ILFIRS*7) + R
      R    = SQRT(C(LVRFT+23+IWLAST)**2 + C(LVRFT+31+IWLAST)**2)
      IF ( IWLAST .LE. 3 ) R = - R
      RMAX = C(LVRFT+7+ILLAST*7) + R
      IF(IDEBUG)THEN 
        WRITE(41,*) ' FIRST L,W,R',ILFIRS,IWFIRS,RMIN
        WRITE(41,*) ' LAST  L,W,R',ILLAST,IWLAST,RMAX
      ENDIF
C
C
      PHI   = Q(LVTXT+6)
      XG    = Q(LVTXT+7)
      YG    = Q(LVTXT+8)
      IF(IDEBUG)THEN
        WRITE(41,*) ' rmin,rmax ', RMIN,RMAX
        WRITE(41,*) ' phi,xg,yg ', PHI,XG,YG
      ENDIF
      A = 1.
      R = RMIN
      B = XG*COS(PHI) +YG*SIN(PHI)
      CE = XG**2 + YG**2 - R**2
      DEL = B*B - A*CE
      IF(IDEBUG)THEN
        WRITE(41,*) ' DEL ', DEL
      ENDIF
      IF ( DEL .LT. 0. ) GO TO 92
      SS = -B + SQRT(DEL)/A
      XHPOS = XG + SS*COS(PHI)
      YHPOS = YG + SS*SIN(PHI)
      IF(IDEBUG)THEN
        WRITE(41,*)' 1ER X,Y',XHPOS,YHPOS
      ENDIF
C                                    
C  *** TRACK NUMBER POSITION         
C                                    
      XTRKNM = XHPOS + XOFF*COS(PHI) 
      YTRKNM = YHPOS + YOFF*SIN(PHI) 
      CALL JMOVE( XHPOS, YHPOS )
      R = RMAX
      CE = XG**2 + YG**2 - R**2
      DEL = B*B - A*CE
      IF ( DEL .LT. 0. ) GO TO 92
      SS = -B + SQRT(DEL)/A
      XHPOS = XG + SS*COS(PHI)
      YHPOS = YG + SS*SIN(PHI)
      IF(IDEBUG)THEN
        WRITE(41,*)' V2EME X,Y',XHPOS,YHPOS
      ENDIF
      CALL JDRAW( XHPOS, YHPOS )
C                                                                      
C    get track number values                                           
C                                                                      
      ITRK = IQ(LVTXT - 5)                                             
      IF (ITRK .LT. 10) THEN                                           
         WRITE (TRKNUM, 1001) ITRK                                     
 1001    FORMAT (1X, I1, 2X)                                           
      ELSE                                                             
        IF (ITRK .LT. 100) THEN                                        
          WRITE (TRKNUM, 1002) ITRK                                    
 1002     FORMAT (1X, I2, 1X)                                          
        ELSE                                                           
          WRITE (TRKNUM, 1003) ITRK                                    
 1003     FORMAT (1X, I3)                                              
        ENDIF                                                          
      ENDIF                                                            
C                                                                      
      XTRKNM = XHPOS + XOFF*COS(PHI)                                   
      YTRKNM = YHPOS + YOFF*SIN(PHI)                                   
C                                                                      
C    draw track numbers                                                
C                                                                      
      CALL J4RGET(1,XMIN, XMAX, YMIN, YMAX)                            
      SIZSCAL = .015                                                   
      XSIZ = (XMAX - XMIN)*SIZSCAL                                     
      YSIZ = (YMAX - YMIN)*SIZSCAL                                     
      IF (MOD(DRWLBL,2) .NE. 0) THEN                                   
        CALL JJUST(2,2)                                                
        CALL JMOVE(XTRKNM, YTRKNM)                                     
        CALL JSIZE(XSIZ, YSIZ)                                         
        CALL J1STRG (TRKNUM)                                           
      ENDIF                                                            
   92 CONTINUE
      LVTXT = LQ ( LVTXT )
      GO TO 90
   91 CONTINUE
      CALL JRCLOS
      IF(NROAD.GT.0) CALL LEGEND_ROAD
  999 RETURN
      END

