      SUBROUTINE PMHEAD
C====================================================================
C
C  Description:  Output run number and event number on each display
C  ============
C
C  Conditions necessary before call:
C  ==================================
C  Graphics initialized
C  New event in Zebra bank
C
C  Author:
C  =======
C  Tami Kramer
C
C  Revision History:
C  =================
C  Original Creation - December 29,1986
C
C======================================================================
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
C  Local Declarations:
C  ===================
C
      INTEGER RUNNO,EVNTNO
      INTEGER NDIG
      REAL SXWMIN,SXWMAX,SYWMIN,SYWMAX
      REAL SXVMIN,SXVMAX,SYVMIN,SYVMAX
      CHARACTER CRUN*6,CEVNT*6
      CHARACTER CEVOUT*17,CRNOUT*16
C
C  Executable Code:
C  ================
C
      RUNNO = IQ(LHEAD+6)
      EVNTNO = IQ(LHEAD+9)
      CALL PXITOC(RUNNO,6,CRUN)
      CALL PXITOC(EVNTNO,6,CEVNT)
      CRNOUT = 'RUN NO. ='//CRUN
      CEVOUT = 'EVENT NO. ='//CEVNT
C
C  Set viewport to the lower right portion of the screen...
C  ====================================================================
C
      CALL J4RGET(1,SXWMIN,SXWMAX,SYWMIN,SYWMAX)
      CALL J4RGET(2,SXVMIN,SXVMAX,SYVMIN,SYVMAX)
      CALL JVPORT(0.,1.,-1.,0.)
      CALL JWINDO(-50.,50.,-50.,50.)
C
C  Open a temporary segment to display event and run number
C  ==========================================================
C
      CALL JWCLIP(.FALSE.)
      CALL JOPEN
C
         CALL JSIZE(3.5,3.5)
         CALL JJUST(2,2)
         CALL JMOVE(5.,3.)
         CALL J3STRG(CRNOUT)
         CALL JMOVE(5.,-2.)
         CALL J3STRG(CEVOUT)
         CALL JMOVE(-35.,-8.)
         CALL J3DRAW(50.,-8.,0.)
C
      CALL JCLOSE
      CALL JWCLIP(.TRUE.)
C
      CALL JWINDO(SXWMIN,SXWMAX,SYWMIN,SYWMAX)
      CALL JVPORT(SXVMIN,SXVMAX,SYVMIN,SYVMAX)
C
      RETURN
      END
