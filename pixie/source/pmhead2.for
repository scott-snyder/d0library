      SUBROUTINE PMHEAD2(ITRAK,VIEW)
C======================================================================
C-
C-   Purpose and Methods : To draw the heading for the blowup view 
C-
C-   Inputs  : ITRAK--track number
C                    VIEW - tells whether nonbend or bend view has 
C-                    been chosen 
C-
C-   Created   9-FEB-1990   Carol Francis
C-                                DH 4/90 add track                                     
C======================================================================
      IMPLICIT NONE
C======================================================================
C    Include statements
C    ==================
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
C    Local Declarations:
C    ===================
      INTEGER RUNNO,EVNTNO,ITRAK
      INTEGER NDIG
      INTEGER VIEW                ! Tells if bend (1) or nonbend (2)
      REAL SXWMIN,SXWMAX,SYWMIN,SYWMAX
      REAL SXVMIN,SXVMAX,SYVMIN,SYVMAX
      CHARACTER CRUN*6,CEVNT*6
      CHARACTER CEVOUT*17,CRNOUT*15
      CHARACTER CHVIEW*24,CTRACK*2,CTRK*13
C
C  Executable Code:
C  ================
C
      RUNNO = IQ(LHEAD+6)
      EVNTNO = IQ(LHEAD+9)
      CALL PXITOC(RUNNO,6,CRUN)
      CALL PXITOC(EVNTNO,6,CEVNT)
      CALL PXITOC(ITRAK,2,CTRACK)
      CRNOUT = 'RUN NO. ='//CRUN
      CEVOUT = 'EVENT NO. ='//CEVNT
      CTRK   = 'TRACK NO.  '//CTRACK
      IF (VIEW.EQ.1) CHVIEW = '  Blow up of bend view  '
      IF (VIEW.EQ.2) CHVIEW = 'Blow up of nonbend view '
C
C  Set viewport to the lower right portion of the screen...
C  ====================================================================
C
      CALL J4RGET(1,SXWMIN,SXWMAX,SYWMIN,SYWMAX)
      CALL J4RGET(2,SXVMIN,SXVMAX,SYVMIN,SYVMAX)
      CALL JVPORT(-1.,0.,0.,1.)
      CALL JWINDO(-50.,50.,-50.,50.)
C
C  Open a temporary segment to display event and run number
C  ==========================================================
C
      CALL JWCLIP(.FALSE.)
      CALL JOPEN
         CALL JSIZE(2.0,2.0)
         CALL JJUST(2,2)
         CALL JMOVE(-25.,35.)
         CALL J3STRG(CRNOUT)
         CALL JMOVE(-25.,30.)
         CALL J3STRG(CEVOUT)
         CALL JMOVE (-25.,20.)
         CALL J3STRG(CHVIEW)
         CALL JMOVE (-25.,15.)
         CALL J3STRG(CTRK)
      CALL JCLOSE
      CALL JWCLIP(.TRUE.)
C
C    If it is the nonbend view, put legend in upper rt. corner
C    =========================================================
      IF (VIEW.EQ.2) THEN
        CALL JVPORT(0.,1.,-.5,.5)
        CALL JWINDO(-50.,50.,-50.,50.)
        CALL JOPEN
          CALL JSIZE(2.,2.)
          CALL JMOVE(0.,25.)
          CALL J3STRG('O - Time division hits')
          CALL JMOVE(0.,20.)
          CALL J3STRG('+ - Vernier pad hits')
        CALL JCLOSE
      ENDIF
C
      CALL JWINDO(SXWMIN,SXWMAX,SYWMIN,SYWMAX)
      CALL JVPORT(SXVMIN,SXVMAX,SYVMIN,SYVMAX)
C
      RETURN
      END

