      SUBROUTINE ZYAXIS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draws the Z-Y axis for the vertex Z-Y view
C-                         display 
C-
C-   Created  15-MAY-1989   Lupe Rosas 
C-   Updated  10-JAN-1990   Lupe Howell
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    XMIN, PXMED, NXMED, XMAX, YMIN, PYMED, NYMED, YMAX
      CHARACTER*4 CXMIN, CNXMED, CPXMED, CXMAX, CYMIN, CPYMED, CNYMED, 
     X            CYMAX, TEMP
      INTEGER KCOL, KINTEN, KFILL, KSTL, DSPDEV
      CHARACTER*3 DRVNAM
C----------------------------------------------------------------------
      DATA XMIN, PXMED, NXMED, XMAX /-60., 30., -30., 60./
      DATA YMIN, PYMED, NYMED, YMAX /-25., 12., -12., 25./ 
C----------------------------------------------------------------------
C Converting the labels' numbers into charcter values
C
      DSPDEV = 1
      CALL D0HDRV( DSPDEV, DRVNAM)
      CALL PXCOLR('FOR')
      CALL JJUST(1,1)
      CALL PUFTOC( XMIN, 'F4.0', TEMP, CXMIN )
      CALL PUFTOC( PXMED, 'F4.0', TEMP, CPXMED )
      CALL PUFTOC( NXMED, 'F4.0', TEMP, CNXMED )
      CALL PUFTOC( XMAX, 'F4.0', TEMP, CXMAX )
      CALL PUFTOC( YMIN, 'F4.0', TEMP, CYMIN )
      CALL PUFTOC( PYMED, 'F4.0', TEMP, CPYMED )
      CALL PUFTOC( NYMED, 'F4.0', TEMP, CNYMED )
      CALL PUFTOC( YMAX, 'F4.0', TEMP, CYMAX )
      CALL JSIZE(1.5,1.)
      CALL JFONT(5)
      CALL PXCOLN('CDC',3,0,0,KCOL,KINTEN,KFILL,KSTL)
      CALL JLSTYL(KSTL)
C Drawing X-axis
C
      CALL J3MOVE( XMIN,  1., 0. )
      CALL J3DRAW( XMIN, -1., 0. )     ! Drawing label's mark
      CALL J3MOVE( XMIN-3.5,-.5, 0. )
      CALL JHSTRG('<')
      CALL J3MOVE( XMIN-2., 0., 0. )
      CALL J3DRAW( XMAX+2., 0., 0. )
      CALL J3MOVE( XMAX+2.,-.5, 0. )
      CALL JHSTRG('> Z')
      CALL J3MOVE( XMAX,  1., 0. )
      CALL J3DRAW( XMAX, -1., 0. )     ! Drawibg label's mark
      CALL J3MOVE(PXMED,  1., 0. )
      CALL J3DRAW(PXMED, -1., 0. )     ! Drawing labels' mark
      CALL J3MOVE(NXMED,  1., 0. )
      CALL J3DRAW(NXMED, -1., 0. )     ! Drawing labels' mark
C Drawing X-labels
C
      CALL J3MOVE( XMIN-2., -3., 0. )
      CALL J3STRG(CXMIN)
      CALL J3MOVE(NXMED-2., -3., 0. )
      CALL J3STRG(CNXMED)
      CALL J3MOVE(  0., -1., 0. )
      CALL J3STRG('0.')
      CALL J3MOVE(PXMED-2.5, -3., 0. )
      CALL J3STRG(CPXMED)
      CALL J3MOVE(XMAX-2.5, -3., 0. ,)
      CALL J3STRG(CXMAX)
C Drawing Y-axis
C
      CALL J3MOVE( -1., YMAX, 0. )
      CALL J3DRAW(  1., YMAX, 0. )      ! Drawing labels'mark
      CALL JBASE(0.,1.,0.)
      CALL JPLANE(-1.,0.,0.)
      CALL J3MOVE( .5, YMAX+1.5, 0. )
      CALL JHSTRG('> ')
      CALL JBASE(1.,0.,0.)
      CALL JPLANE(0.,1.,0.)
      CALL J3MOVE( .5, YMAX+2., 0. )
      CALL JHSTRG(' Y')
      CALL J3MOVE(  0., YMAX+1.5, 0. )
      CALL J3DRAW(  0., YMIN-1.5, 0. )
      CALL J3MOVE(  .5, YMIN-1.5, 0. )
      CALL JBASE( 0., -1., 0. )
      CALL JPLANE( -1., 0., 0. )
      CALL JHSTRG('> ')
      CALL JBASE(1., 0., 0. )
      CALL JPLANE( 0., 1., 0. )
      CALL J3MOVE( -1., YMIN, 0. )
      CALL J3DRAW(  1., YMIN, 0. )      ! Drawing labels' mark
      CALL J3MOVE( -1.,PYMED, 0. )
      CALL J3DRAW(  1.,PYMED, 0. )
      CALL J3MOVE( -1.,NYMED, 0. )
      CALL J3DRAW(  1.,NYMED, 0. )
C Drawing Y-labels
C
      CALL J3MOVE( 2., YMIN-.5, 0. )
      CALL J3STRG(CYMIN)
      CALL J3MOVE( 2., NYMED-.5, 0. )
      CALL J3STRG(CNYMED)
      CALL J3MOVE( 2.,PYMED-.5, 0. )
      CALL J3STRG(CPYMED)
      CALL J3MOVE( 2., YMAX-.5, 0. )
      CALL J3STRG(CYMAX)
      CALL PXCOLN('CDC',1,0,0,KCOL,KINTEN,KFILL,KSTL)
      CALL JLSTYL(KSTL)
  999 RETURN
      END
