      SUBROUTINE DLDTSG(NHIT,LRWIR,IHIT,PHI,X0,Y0,CHISQ,ERRD,ERRPHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prepare an array to send to routine ZFDTSG
C-                         which will load a track segment into DTSG.
C-
C-   Inputs  : 
C-   Input   :  LRWIR(1:NHIT) = WIRE*2+LR (LR=0/1 for phi(hit) >/< phi(wire))
C-              IHIT (1:NHIT) = pointer to hit in bank VSEC
C-              PHI    
C-              X0,Y0         = center of gravity in x,y plane
C-              ERRD          = error in the center of gravity
C-              ERRPHI        = error in phi
C-              CHISQ         = chi_squared for fit in x,y plane
C-   Outputs : none
C-
C-   Created   6-NOV-1989   joey thompson: based on LDVSEG
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDLOCA.INC'
      INCLUDE 'D0$INC:CDCLNK.INC'
      INCLUDE 'D0$INC:CDCPAR.INC'
      INTEGER NHIT,LRWIR(NCDCEL),IHIT(NCDCEL) 
      INTEGER HIT,DSEC,JLOC,WIRNUM,SIDE,JBIT,HITNUM
      REAL PHI,X0,Y0,ERRD,ERRPHI,CHISQ
      INTEGER LTRSEG
      PARAMETER(LTRSEG= 8 + 2*NCDCEL)
      REAL    TRASEG( LTRSEG )
      INTEGER KTRSEG( LTRSEG )
      EQUIVALENCE ( TRASEG(1), KTRSEG(1) )
C
C ****  Store the track segment
C
      CALL VZERO(KTRSEG,LTRSEG)
      KTRSEG( 1 ) = 0            ! status
      KTRSEG( 2 ) = NHIT-2   !This is "NDEGF"
      TRASEG( 3 ) = X0
      TRASEG( 4 ) = Y0
      TRASEG( 5 ) = PHI
      TRASEG( 6 ) = ERRD         !This was "ERRD" but I couldn't find it...
      TRASEG( 7 ) = ERRPHI       !This was "ERRPHI" but I couldn't find it...
      TRASEG( 8 ) = CHISQ
      DO 300 HIT = 1, NHIT
          DSEC = LDSEC(SECTOR,LAYER)
          JLOC = IHIT(HIT)
          WIRNUM = LRWIR(HIT)/2
          HITNUM=((JLOC - (DSEC+IQ(DSEC+4+IQ(DSEC+2)+WIRNUM)))/
     &      IQ(DSEC+3))+1
          SIDE = JBIT(LRWIR(HIT),1)
          KTRSEG(9+WIRNUM)=LAYER*2**16+SECTOR*2**11+WIRNUM*2**8+
     &      HITNUM*2+SIDE
          TRASEG( 9+NCDCEL+WIRNUM ) = 0        !removed "RESID( WIRE )"
  300 CONTINUE
      CALL ZFDTSG( TRASEG, LTRSEG )
  999 RETURN
      END
