       SUBROUTINE PMBLOW(IVIEW)
C======================================================================
C
C    Purpose and Methods : To blow-up view for bend and non-bend view
C 
C    Inputs  : IVIEW 
C 
C    Created  31-DEC-1989   Carol Francis
C
C======================================================================
      IMPLICIT NONE
C======================================================================
C    Variable Declarations
C    =====================
      INTEGER IVIEW             !To pass to PMVIEW
      INTEGER ACT               !For string input in menus
      INTEGER NTRAKS,ITRAK
      INTEGER VIEW              ! Tells if bend or nonbend view
      INTEGER TRAK              !Tells PMSAVE this is the tracking view
      REAL XSIZE,YSIZE
      CHARACTER*1 STR,STR2
      DATA TRAK/1/
      DATA XSIZE,YSIZE/2.,4./
C
C    Executable Program
C    ==================
C
C    Determine number of tracks in event and if none, then
C    put message on screen
C    =====================================================
      CALL GTMTRH(NTRAKS)
      IF (NTRAKS.EQ.0) THEN
        CALL JCLEAR
        CALL JVPORT(-1.,1.,-1.,1.)
        CALL JWINDO(-50.,50.,-50.,50.)
        CALL JOPEN
        CALL JSIZE(.5*XSIZE,.5*YSIZE)
        CALL JMOVE(-25.,20.)
        CALL J1STRG('No tracks found in this event')
        CALL JCLOSE
        GOTO 999
      ENDIF
C
C    Put up menu for bend-nonbend choice
C    ===================================
   25 CONTINUE
      ITRAK=1
      CALL JVPORT(-1.,1.,-1.,1.)
      CALL JWINDO(-50.,50.,-50.,50.)
      CALL JOPEN
        CALL JSIZE(.5*XSIZE,.5*YSIZE)
        CALL JMOVE(-40.,15.)
        CALL J1STRG('Would you like:')
        CALL JMOVE(-40.,5.)
        CALL J1STRG('Bend view......B')
        CALL JMOVE(-40.,1.)
        CALL J1STRG('Nonbend view...N')
        CALL JMOVE(-40.,-3.)
        CALL J1STRG('Quit...........Q')
      CALL JCLOSE
      CALL JIENAB(1,4,1)
      CALL JKEYBS(1,1,1,1,STR,ACT)
      CALL JIDISA(1,4,1)
C
      IF ((STR.EQ.'B') .OR. (STR.EQ.'b')) THEN
        CALL JCLEAR
        VIEW = 1
        CALL PMHEAD2(ITRAK,VIEW)
        CALL PMCHNM2(ITRAK,VIEW)
        CALL PMPORT(ITRAK,VIEW)
      ENDIF
      IF ((STR.EQ.'N') .OR. (STR.EQ.'n')) THEN
        CALL JCLEAR
        VIEW = 2
        CALL PMHEAD2(ITRAK,VIEW)
        CALL PMCHNM2(ITRAK,VIEW)
        CALL PMPORT(ITRAK,VIEW)
C
      ENDIF
      IF ((STR.EQ.'Q') .OR. (STR.EQ.'q')) THEN
        CALL JCLEAR
C        CALL PMVIEW(IVIEW)
        GOTO 999
      ENDIF
C
   50 CONTINUE  
C
C    Give choice of quiting or showing next track
C    ============================================
      CALL JVPORT(0.,1.,-1.,0.)
      CALL JWINDO(-50.,50.,-50.,50.)
      CALL JOPEN
        CALL JSIZE(XSIZE,YSIZE)
        CALL JMOVE(10.,0.)
        CALL J1STRG('Would you like:')
        CALL JMOVE(10.,-7.)
        CALL J1STRG('Next track.......N')
        CALL JMOVE(10.,-14.)
        CALL J1STRG('Laser print......L')
        CALL JMOVE(10.,-21.)
        CALL J1STRG('Dump event.......D')
        CALL JMOVE(10.,-28.)
        CALL J1STRG('Quit this view...Q')
      CALL JCLOSE
      CALL JIENAB(1,4,1)
      CALL JKEYBS(1,1,1,1,STR2,ACT)
      CALL JIDISA(1,4,1)
C
      IF ((STR2.EQ.'N') .OR. (STR2.EQ.'n')) THEN
        CALL JCLEAR
        IF (ITRAK.LT.NTRAKS) THEN
          ITRAK = ITRAK + 1
          CALL PMHEAD2(ITRAK,VIEW)
          CALL PMCHNM2(ITRAK,VIEW)
          CALL PMPORT(ITRAK,VIEW)
          GOTO 50
        ELSE
          CALL JCLEAR
          CALL JVPORT(-1.,1.,-1.,1.)
          CALL JWINDO(-50.,50.,-50.,50.)
          CALL JOPEN
        CALL JSIZE(XSIZE,YSIZE)
          CALL JMOVE(-23.,35.)
          CALL J1STRG('No more tracks in this view')
          CALL JCLOSE
        ENDIF
      ENDIF
      IF ((STR2.EQ.'L') .OR. (STR2.EQ.'l')) THEN
        CALL PMSAVE(IVIEW,ITRAK,VIEW,TRAK)
        CALL PMCHNM2(ITRAK,VIEW)
        CALL PMPORT(ITRAK,VIEW)
        GOTO 50
      ENDIF
      IF ((STR2.EQ.'D') .OR. (STR2.EQ.'d')) THEN
        CALL HDSRES
        CALL MUPRTD(6)
        CALL MUPRTD(15)
        GOTO 50
      ENDIF
      IF ((STR2.EQ.'Q') .OR. (STR2.EQ.'q')) THEN
        CALL JCLEAR
      ENDIF
      GOTO 25
C        
  999 CONTINUE
      RETURN
      END
