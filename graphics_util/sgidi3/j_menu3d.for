      SUBROUTINE J_MENU3D(NCOM,ICOM)
C-----------------------------------------------------------------
C-
C-  Purpose and Method: Displays and executes the rotation menu.  This 
C-   Menu allows motion utilities: Zoom, Spin, Rolls (x, Y Z)'s, Move, 
C-   Near, far, and Perspective.   The menu also lets the user make some 
C-   system utilities: Color, Hardcopy, Pick, Add/Remove views.  The
C-   system utilities (non-motion) are NOT execute here, the routine 
C-   simple returns a number representing the system utility command and the 
C-   number of commands output.  The PIXIE system will perfomr the system
C-   utility by pushing the corresponding command in to the command queue and
C-   and it will return to this rouinte by executing the ROTATE command.
C-   For example, if hardcopy is requested:
C-      ICOM(1) = 2, ICOM(2)= 1, and NCOM=2
C-   This will tell the PIXIE system to execute the system command hardcopy(2)
C-   and Rotate (1).   Here is a list of the system commands and their
C-   corresponding numbers:
C-        Rotate    -> 1
C-        Hardcopy  -> 2
C-        Redraw    -> 3
C-        Pick      -> 4
C-        COMMAND   -> 5
C-        Add/Remove-> 6
C-
C-  Inputs:  None
C-  
C-  Outputs: NCOM   [I ]: Number of commands returned
C-           ICOM [I(*)]: Array containing the list of command codes.
C-                        
C-  Author:  XX-XXX-XX  Michael Shupe.  From routines by Jon Streets.
C-  Updated: 13-AUG-92  Lupe Howell-Redraw the window in case
C-                       of window damage was added.  System utilities
C-                       implemented.
C-  Updated:  01-DEC-92 Lupe Howell Implicit None added.  Hardcopy, Pick,
C-                       etc implemented
C-  Updated:  12-APR-93 Lupe Howell Turn on front and back buffers
C-   			 at entry and exit times
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
c
      INCLUDE 'fgl.h'
      INCLUDE 'fdevice.h'
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
C------------------------------------------------------------------
      INTEGER ICOM(*),NCOM
C
      COMMON/J_RSEGMS/NSEG,IRSEG(512)
      CHARACTER*9 HCPOST
      CHARACTER*5 CTEM
      CHARACTER*14 HCFILE
      INTEGER NSEG,IRSEG,IX1,INPREV,ISEG
      INTEGER JTTYPE,MVAL,II,JJ,IY1,IVPREV,IOBJEC
      INTEGER*2 QVAL
      INTEGER*4 MONTYPE,IOBROT,MEMAIN,I3VAL,IOJECT,IDEV,IXL,IYL
      INTEGER*4 IX,IY,IMVAL,BUFFER_NUM
      LOGICAL FIRST,UPDATE,second
      REAL INPMAT(4,4),ROLMAT(4,4),SPNMAT(4,4),TMPMAT(4,4),INPVIEW(4,4)
      REAL TEMP1(3),TEMP2(3),TEMP3(3),TEMP4(3)
      REAL PIB180,ZFSCAL,XTRAN,YTRAN,ZTRAN,XROT,YROT,XROLL,YROLL,ZROLL
      REAL DUINP,UCINP,DVINP,VCINP,DUNOW,DVNOW,DX,DY,SX,SY,CX,CY
      REAL FSCA
C
      DATA HCFILE/'     view.pict'/
      DATA FIRST/.TRUE./
      data SECOND/.FALSE./
      SAVE
C----------------------------------------------------------------------
C
C *** TEMP
C
      IF ( SECOND ) THEN
      CALL GETMAT(INPMAT)
      CALL COLOR(0)
      CALL CLEAR()
      CALL LOADMA(INPMAT)
      IF(IOBROT.GT.0) THEN
	CALL CALLOB(IOBROT)
      ENDIF
      IF(IOBJEC.GT.0) THEN
	CALL CALLOB(IOBJEC)
      ENDIF
      CALL SWAPBU()
      ENDIF
      BUFFER_NUM = GETBUF()
C
C *** Make the permanent menu if not done yet.
C
      IF(FIRST) THEN
        CALL QDEVIC(LEFTMO)
        CALL QDEVIC(RIGHTM)
        CALL QRESET()
        CALL GETORI(IX1,IY1)
        PIB180=3.1415927/180.
C
C *** VIEW menu
C
        MEMAIN=NEWPUP()
        CALL ADDTOP(MEMAIN,'3D MENU%t',9,1)
        CALL ADDTOP(MEMAIN,'Zoom',4,1)
        CALL ADDTOP(MEMAIN,'Spin',4,1)
        CALL ADDTOP(MEMAIN,'Roll Z',6,1)
        CALL ADDTOP(MEMAIN,'Roll Y',6,1)
        CALL ADDTOP(MEMAIN,'Roll X',6,1)
        CALL ADDTOP(MEMAIN,'Move',4,1)
        CALL ADDTOP(MEMAIN,'Reset View',10,1)
        CALL ADDTOP(MEMAIN,'Hcpy',4,1)
        CALL ADDTOP(MEMAIN,'Pick',4,1)
        CALL ADDTOP(MEMAIN,'Add/Remove',10,1)
        CALL ADDTOP(MEMAIN,'Exit',4,1)
C
C ***  VIEW menu
C
        MEVIEW=NEWPUP()
        CALL ADDTOP(MEVIEW,'Reset Viewing Options%t',23,1)
        CALL ADDTOP(MEVIEW,'Reset ALL Viewing Parameters',28,1)
        CALL ADDTOP(MEVIEW,'UNZOOM',6,1)
        CALL ADDTOP(MEVIEW,'Reset ROTATIONS ONLY',20,1)
        FIRST = .FALSE.
	SECOND = .TRUE.
      ENDIF
C
C *** Initialize the transformations for this session
C
      ZFSCAL=1.
      XTRAN=0.
      YTRAN=0.
      ZTRAN=0.
      XROT=0.
      YROT=0.
      XROLL=0.
      YROLL=0.
      ZROLL=0.
C
C *** PICK UP CURRENT WINDOW PARAMETERS
C
      DUINP=UMAX-UMIN
      UCINP=.5*(UMIN+UMAX)
      DVINP=VMAX-VMIN
      VCINP=.5*(VMIN+VMAX)
      WNEAR=-2.*(DUINP)
      WFAR=-WNEAR
      IVPREV=0
      CALL GETMAT(INPMAT)
      CALL D_MATUNI(SPNMAT)
      CALL D_MATUNI(ROLMAT)
      CALL D_MATCPY(TVIEW,CURMAT)
      CALL D_MATCPY(TVIEW,INPVIEW)
C
C *** Create objects containing complete current event
C
      IF(IOBROT.NE.0) THEN
	CALL DELOBJ(IOBROT)
      ENDIF
      IF(IOBROT.EQ.0) THEN
   	IOBROT=GENOBJ()
      ENDIF
      CALL MAKEOB(IOBROT)
      DO ISEG=1,NSEGS
        CALL COLOR(7)
        CALL CALLOB(OBJID(ISEG))
      ENDDO
      CALL CLOSEO
C
      IF(NONROT.EQ.1) THEN
        IF(IOBJEC.NE.0) THEN
	   CALL DELOBJ(IOBJEC)
 	ENDIF
        IF(IOBJEC.EQ.0) THEN
	  IOBJEC=GENOBJ()
	ENDIF
        CALL MAKEOB(IOBJEC)
C
C *** This section is very non-standard
C
        CALL COLOR(7)
        CALL NON_ROTATABLE_OBJECTS
        CALL CLOSEO
      ENDIF
C
      IF(IDUBL.EQ.1) THEN
        CALL FRONTB(.FALSE.)
        CALL BACKBU(.TRUE.)
      ENDIF
      BUFFER_NUM = GETBUF()
      CALL COLOR(0)
      CALL CLEAR()
      CALL LOADMA(INPMAT)
      IF(IOBROT.GT.0) THEN
	CALL CALLOB(IOBROT)
      ENDIF
      IF(IOBJEC.GT.0) THEN
	CALL CALLOB(IOBJEC)
      ENDIF
      IF(IDUBL.EQ.1) CALL SWAPBU()
      BUFFER_NUM = GETBUF()
      CALL LOADMA(INPMAT)
      UPDATE = .FALSE.
C
C *** Draw the menu
C
  200 I3VAL=DOPUP(MEMAIN)
C
C *** Re-display the menu if the user makes no
C *** menu selection
C
      IF ( I3VAL .LT. 0 ) GO TO 200
C
C *** Motion Tasks
C
      IF(( I3VAL .LT. 7 ) .AND. ( I3VAL .GT. 0 ) ) THEN
C
C *** Motion Tasks use the Leftmost mouse botton to get the coordenates
C *** of the motion.   While button is down poll x,y positions and rotate
C *** according to displacement from original position
C
  100   IDEV=QREAD(QVAL)
C
C *** Check for return to popup menu
C
        IF(IDEV.EQ.RIGHTM.AND.QVAL .EQ.1) GO TO 200
C
C *** Redraw the displasplay if window needs to be redrawn
C *** and returns to the input from the mouse
C
        IF( IDEV .EQ. REDRAW ) THEN
          CALL RESHAP
          CALL COLOR(0)
          CALL CLEAR()
          CALL CALLOB(IOBROT)
          GO TO 100
        ENDIF
C
C *** Reset the view change reference point
C
        IF(IDEV.EQ.LEFTMO.AND.QVAL .EQ.1) THEN
          IXL=GETVAL(MOUSEX)
          IYL=GETVAL(MOUSEY)
C
C *** Loop while button down
C
   55     CONTINUE
          IX=GETVAL(MOUSEX)
          IY=GETVAL(MOUSEY)
C
C *** ZOOM
C
          IF(I3VAL.EQ.1) THEN
            ZFSCAL=ZFSCAL-ZFSCAL*FLOAT(IY-IYL)/500.
            IF (ZFSCAL.LT.0.001) ZFSCAL=0.001
            IF (ZFSCAL.GT.1000.) ZFSCAL=1000.
C
C *** Scale the window boundaries and recalculate the device
C *** transformation in TSCREE.
C
            DUNOW=ZFSCAL*DUINP
            UMAX=UCINP+.5*DUNOW
            UMIN=UMAX-DUNOW
            DVNOW=ZFSCAL*DVINP
            VMAX=VCINP+.5*DVNOW
            VMIN=VMAX-DVNOW
            CALL J_DEVTRN
            JTTYPE=0
C
C *** SPIN
C
          ELSEIF(I3VAL.EQ.2) THEN
C
C ***   Sign and axes conventions insure picture rotates in
C ***   direction of mouse movement.
C
            DX=FLOAT(IX-IXL)
            DY=FLOAT(IY-IYL)
            IF(IRIGHT.EQ.1) THEN
              DX=-DX
              DY=-DY
            ENDIF
            XROT=XROT+DY/5.
            YROT=YROT-DX/5.
            JTTYPE=1
C
C *** ROLL Z
C
          ELSEIF(I3VAL.EQ.3) THEN
            ZROLL=ZROLL+FLOAT(IY-IYL)/5.
            JTTYPE=2
C
C *** ROLL Y
C
          ELSEIF(I3VAL.EQ.4) THEN
            YROLL=YROLL+FLOAT(IY-IYL)/5.
            JTTYPE=2
C
C *** ROLL X
C
          ELSEIF(I3VAL.EQ.5) THEN
            XROLL=XROLL+FLOAT(IY-IYL)/5.
            JTTYPE=2
C
C *** MOVE
C
          ELSEIF(I3VAL.EQ.6) THEN
C
C ***   2 rotations followed by a translation; Z is changed
C
            DX=FLOAT(IX-IXL)*(UMAX-UMIN)/1000.
            DY=FLOAT(IY-IYL)*(VMAX-VMIN)/1000.
            SX=SIN(XROT*PIB180)
            CX=COS(XROT*PIB180)
            SY=SIN(YROT*PIB180)
            CY=COS(YROT*PIB180)
            XTRAN=XTRAN+(DX*CY+DY*SY*SX)
            YTRAN=YTRAN+(DY*CX)
            ZTRAN=ZTRAN+(DX*SY-DY*CY*SX)
            JTTYPE=1
          ENDIF
C  (Comment out next 2 lines for continuous movement.)
          IXL=IX
          IYL=IY
C
C *** Clear the screen to draw the transformed object
C
          CALL COLOR(0)
          CALL CLEAR()
C
C *** Transform and draw the rotatable object
C
          IF(JTTYPE.EQ.0) THEN
          ELSEIF(JTTYPE.EQ.1) THEN
            CALL D_MATUNI(SPNMAT)
            CALL D_MATROT(XROT,'x',SPNMAT)
            CALL D_MATROT(YROT,'y',SPNMAT)
            CALL D_MATTRA(XTRAN,YTRAN,ZTRAN,SPNMAT)
          ELSEIF(JTTYPE.EQ.2) THEN
            IF(I3VAL.EQ.3) THEN
              IF(IVPREV.EQ.4.OR.IVPREV.EQ.5) THEN
                CALL D_MATCPY(ROLMAT,TMPMAT)
                CALL D_MATMUL(TVIEW,TMPMAT)
                CALL D_MATCPY(TMPMAT,TVIEW)
                XROLL=0.
                YROLL=0.
              ENDIF
              CALL D_MATUNI(ROLMAT)
              CALL D_MATROT(ZROLL,'z',ROLMAT)
              IVPREV=3
            ELSEIF(I3VAL.EQ.4) THEN
              IF(IVPREV.EQ.3.OR.IVPREV.EQ.5) THEN
                CALL D_MATCPY(ROLMAT,TMPMAT)
                CALL D_MATMUL(TVIEW,TMPMAT)
                CALL D_MATCPY(TMPMAT,TVIEW)
                XROLL=0.
                ZROLL=0.
              ENDIF
              CALL D_MATUNI(ROLMAT)
              CALL D_MATROT(YROLL,'y',ROLMAT)
              IVPREV=4
            ELSEIF(I3VAL.EQ.5) THEN
              IF(IVPREV.EQ.4.OR.IVPREV.EQ.3) THEN
                CALL D_MATCPY(ROLMAT,TMPMAT)
                CALL D_MATMUL(TVIEW,TMPMAT)
                CALL D_MATCPY(TMPMAT,TVIEW)
                ZROLL=0.
                YROLL=0.
              ENDIF
              CALL D_MATUNI(ROLMAT)
              CALL D_MATROT(XROLL,'x',ROLMAT)
              IVPREV=5
            ENDIF
          ENDIF
C
C *** COMPUTE THE FULL TRANSFORMATION
C
          CALL D_MATUNI(TMPMAT)
          CALL D_MATMUL(ROLMAT,TMPMAT)
          CALL D_MATMUL(TVIEW,TMPMAT)
          CALL D_MATMUL(SPNMAT,TMPMAT)
C
C *** SAVE THE CURRENT VIEWING MATRIX
C
          CALL D_MATCPY(TMPMAT,CURMAT)
C
C *** COMPUTE THE FULL TRANSFORMATION
C
          CALL D_MATMUL(TSCREE,TMPMAT)
C
C *** SET THE NEAR AND FAR PLANES
C
          CALL ORTHO(-1.,1.,-1.,1.,WNEAR,WFAR)
C
C *** LOAD UP THE COMPLETE TRANSFORMATION
C *** Pre multiplies the current transformation matrix
C
          CALL MULTMA(TMPMAT)
C
C *** DRAW THE ROTATABLE OBJECT
C
CC        CALL COLOR(0)
CC        CALL CLEAR()
          CALL CALLOB(IOBROT)
C
C *** DISPLAY THE BACK BUFFER
C
          IF(IDUBL) THEN 
	    CALL SWAPBU()
	  ENDIF
          IF(GETBUT(LEFTMO)) GO TO 55
        ENDIF
        GOTO 100
C
C  *** DO THE NON-MOTION TASKS *****
C

C
C *** RESET VIEW
C
      ELSEIF(I3VAL.EQ.7) THEN
C
C *** Pop up RESET VIEW SUB-MENU 
C
 300    IMVAL=DOPUP(MEVIEW)
        IF ( MVAL .LT. 0 ) GO TO 300
C
C *** Reset ROTATION variables
C
        IF( (IMVAL.EQ.1) .OR. (IMVAL .EQ. 3) ) THEN       
C
C *** Reset ALL the viewing parameters to original position
C *** Set the rolling, spin, and view matrices to zero.
C
          XTRAN  = 0.
          YTRAN  = 0.
          ZTRAN  = 0.
          XROT   = 0.
          YROT   = 0.
          XROLL  = 0.
          YROLL  = 0.
          ZROLL  = 0.
          CALL D_MATUNI(ROLMAT)
          CALL D_MATUNI(SPNMAT)
          CALL D_MATCPY(INPVIEW,TVIEW)
        ENDIF
C
C *** Reset zoom parameters for UNZOOM
C
	IF ( IMVAL .NE. 3 ) THEN
C
C *** Setting the zoom scale to 1 to return to original window
C *** values.   Scale the window boundaries. 
C
           ZFSCAL = 1.
           DUNOW=ZFSCAL*DUINP
           UMAX=UCINP+.5*DUNOW
           UMIN=UMAX-DUNOW
           DVNOW=ZFSCAL*DVINP
           VMAX=VCINP+.5*DVNOW
           VMIN=VMAX-DVNOW
C
C *** Recalculate the device transformation in TSCREE if reset ALL
C
 	   CALL J_DEVTRN 
         ENDIF
C
C *** Compute the current matrix with the new reseted values
C
         CALL D_MATUNI(TMPMAT)
         CALL D_MATMUL(ROLMAT,TMPMAT)
         CALL D_MATMUL(TVIEW,TMPMAT)
         CALL D_MATMUL(SPNMAT,TMPMAT)
         CALL D_MATCPY(TMPMAT,CURMAT)
         CALL D_MATMUL(TSCREE,TMPMAT)
         CALL ORTHO(-1.,1.,-1.,1.,WNEAR,WFAR)
         CALL MULTMA(TMPMAT)
C
C *** Draw the object after reset
C
	 CALL COLOR(0)
	 CALL CLEAR()
         CALL CALLOB(IOBROT)
         IF(IDUBL) THEN
	    CALL SWAPBU()
	 ENDIF
         GO TO 200
C
C *** HCPY
C
      ELSEIF (I3VAL.EQ.8) THEN
C
C *** Make hardcopy of display screen.  Return command sequence
C *** to JESCAP through NCOM, ICOM array
C *** The command sequence should be Hardcopy, Command view, and 
C *** Rotate to get back to the rotate submenu.
C
        NCOM = 3
        ICOM(1) = 1  
        ICOM(2) = 5  
        ICOM(3) = 2  
	UPDATE = .TRUE.
C
C *** PICK
C
      ELSEIF ( I3VAL .EQ. 9 ) THEN
        NCOM = 2
        ICOM(1) = 1   
        ICOM(2) = 4   
	UPDATE = .TRUE.
C
C *** ADD/REMOVE
C
      ELSEIF ( I3VAL .EQ. 10 ) THEN
        NCOM = 2
        ICOM(1) = 1   
C        ICOM(2) = 5   
        ICOM(2) = 6   
	UPDATE = .TRUE.
C
C *** EXIT
C
      ELSEIF(I3VAL.EQ.11) THEN
        NCOM = 0
	UPDATE = .TRUE.
      ENDIF
C
C *** Update views beore leaving.
C *** TVIEW from CURMAT, and TTOTAL from TMPMAT
C
      IF ( UPDATE ) THEN
        CALL D_MATCPY(CURMAT,TVIEW)
        CALL D_MATCPY(TMPMAT,TTOTAL)
C
C *** Extract UVN matrix from CURMAT.  (Take out scale factor)
C
        FSCA=SQRT(CURMAT(1,1)**2+CURMAT(1,2)**2+CURMAT(1,3)**2)
        DO II=1,3
          DO JJ=1,4
            CURMAT(II,JJ)=CURMAT(II,JJ)/FSCA
          ENDDO
        ENDDO
        CALL D_DMPMAT(' J_MENU3D - UVNMAT on EXIT:',CURMAT)
C
C *** Update view vectors.  (Window is updated already.)
C
        UVECX= CURMAT(1,1)
        UVECY= CURMAT(1,2)
        UVECZ= CURMAT(1,3)
C
        VVECX= CURMAT(2,1)
        VVECY= CURMAT(2,2)
        VVECZ= CURMAT(2,3)
C
        VNORMX=CURMAT(3,1)
        VNORMY=CURMAT(3,2)
        VNORMZ=CURMAT(3,3)
C
C *** Update the viewpoint
C
        XVW=-TVIEW(1,4)
        YVW=-TVIEW(2,4)
        ZVW=-TVIEW(3,4)
C
C *** TEMP
C
      CALL GETMAT(INPMAT)
      CALL COLOR(0)
      CALL CLEAR()
      CALL LOADMA(INPMAT)
      IF(IOBROT.GT.0) THEN
	CALL CALLOB(IOBROT)
      ENDIF
      IF(IOBJEC.GT.0) THEN
	CALL CALLOB(IOBJEC)
      ENDIF
      BUFFER_NUM = GETBUF()
      CALL SWAPBU()
      BUFFER_NUM = GETBUF()
C
C *** Set the front buffer ON
C
      BUFFER_NUM = GETBUF()
	CALL FRONTB(.TRUE.)
	CALL BACKBU(.FALSE.)
      BUFFER_NUM = GETBUF()
      ENDIF
      RETURN
      END
