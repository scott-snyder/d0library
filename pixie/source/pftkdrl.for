      SUBROUTINE PFTKDRL(TRACK, HALF, XHIT, YHIT, RHIT, PHI, IVIEW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw Track Labels.
C-
C-   Inputs  : TRACK, ZHIT, XHIT, YHIT, RHIT, IVIEW
C-   Outputs : 
C-   Controls: 
C-
C-   Created  11-OCT-1991   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Input:
      INTEGER TRACK, HALF, IVIEW
      REAL    XHIT, YHIT, RHIT, PHI
C  Local:
      INTEGER MAX_LABEL 
      PARAMETER( MAX_LABEL  =  61)
      REAL    XSIZE, YSIZE 
      REAL    XPOS, YPOS 
      REAL    Z_LABEL(0:1)
      REAL    EXTEN
C
      CHARACTER*1 LABEL
      CHARACTER*1 LABEL_KEY(0:MAX_LABEL)
C
      DATA Z_LABEL /142.,145./
      DATA EXTEN /2./
      DATA XSIZE, YSIZE /2.0,3.5/
      DATA LABEL_KEY /'0','1','2','3','4','5','6','7','8','9',
     &                'A','B','C','D','E','F','G','H','I','J',
     &                'K','L','M','N','O','P','Q','R','S','T',
     &                'U','V','W','X','Y','Z',
     &                'a','b','c','d','e','f','g','h','i','j',
     &                'k','l','m','n','o','p','q','r','s','t',
     &                'u','v','w','x','y','z'/
C----------------------------------------------------------------------
      IF ( TRACK .LE. MAX_LABEL ) THEN
        LABEL = LABEL_KEY(TRACK)
      ELSE
        LABEL = ' '              
      ENDIF
C
      IF ( IVIEW.LE.3 ) THEN
        XPOS = (-1)**(HALF+1) *  Z_LABEL(MOD(TRACK,2)) 
        IF(IVIEW.EQ.1) YPOS = XHIT
        IF(IVIEW.EQ.2) YPOS = YHIT
        IF(IVIEW.EQ.3) YPOS = RHIT 
        CALL JSIZE( 1.5*XSIZE,1.5*YSIZE )
      ELSE
        XPOS = XHIT + EXTEN*COS(PHI)
        YPOS = YHIT + EXTEN*SIN(PHI)
        CALL JSIZE( XSIZE,YSIZE )
      ENDIF
C
      CALL JJUST( 2,2 )
      CALL JMOVE( XPOS,YPOS )
      CALL J2STRG( LABEL )
C
  999 RETURN
      END
