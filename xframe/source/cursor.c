/*******************************************************************************
*									       *
* misc.c -- Miscelaneous Motif convenience functions			       *
*									       *
* Copyright (c) 1991 Universities Research Association, Inc.		       *
* All rights reserved.							       *
* 									       *
* This material resulted from work developed under a Government Contract and   *
* is subject to the following license:  The Government retains a paid-up,      *
* nonexclusive, irrevocable worldwide license to reproduce, prepare derivative *
* works, perform publicly and display publicly by or for the Government,       *
* including the right to distribute to other Government contractors.  Neither  *
* the United States nor the United States Department of Energy, nor any of     *
* their employees, makes any warrenty, express or implied, or assumes any      *
* legal liability or responsibility for the accuracy, completeness, or         *
* usefulness of any information, apparatus, product, or process disclosed, or  *
* represents that its use would not infringe privately owned rights.           *
*                                        				       *
* Fermilab Nirvana GUI Library						       *
* July 28, 1992								       *
*									       *
* Written by Mark Edel							       *
*									       *
*******************************************************************************/
static char SCCSID[] = "@(#)misc.c	1.26	3/8/94";
#include <math.h>
#include <stdio.h>
#include "/d0library/scratch/test/xframe/source/d0x_c.h"

void RemapDeleteKey(Widget w);
void SetDeleteRemap(int state);
void SetWatchCursor(Widget topCursorWidget);
void SetDefaultCursor(Widget topCursorWidget);

/* flag used to disable delete key remapping */
static int RemapDeleteEnabled = True;

/* bitmap and mask for waiting (wrist-watch) cursor */
#define watch_x_hot 7
#define watch_y_hot 7
#define watch_width 16
#define watch_height 16
static unsigned char watch_bits[] = {
   0xe0, 0x07, 0xe0, 0x07, 0xe0, 0x07, 0xe0, 0x07, 0x10, 0x08, 0x08, 0x11,
   0x04, 0x21, 0x04, 0x21, 0xe4, 0x21, 0x04, 0x20, 0x08, 0x10, 0x10, 0x08,
   0xe0, 0x07, 0xe0, 0x07, 0xe0, 0x07, 0xe0, 0x07
};
#define watch_mask_width 16
#define watch_mask_height 16
static unsigned char watch_mask_bits[] = {
   0xf0, 0x0f, 0xf0, 0x0f, 0xf0, 0x0f, 0xf0, 0x0f, 0xf8, 0x1f, 0xfc, 0x3f,
   0xfe, 0x7f, 0xfe, 0x7f, 0xfe, 0x7f, 0xfe, 0x7f, 0xfc, 0x3f, 0xf8, 0x1f,
   0xf0, 0x0f, 0xf0, 0x0f, 0xf0, 0x0f, 0xf0, 0x0f
};

/*
** This routine kludges around the problem of backspace not being mapped
** correctly when Motif is used between a server with a delete key in
** the traditional typewriter backspace position and a client that
** expects a backspace key in that position.  Though there are three
** distinct levels of key re-mapping in effect when a user is running
** a Motif application, none of these is really appropriate or effective
** for eliminating the delete v.s. backspace problem.  Our solution is,
** sadly, to eliminate the forward delete functionality of the delete key
** in favor of backwards delete for both keys.  So as not to prevent the
** user or the application from applying other translation table re-mapping,
** we apply re-map the key as a post-processing step, applied after widget
** creation.  As a result, the re-mapping necessarily becomes embedded
** throughout an application (wherever text widgets are created), and
** within library routines, including the Nirvana utility library.  To
** make this remapping optional, the SetDeleteRemap function provides a
** way for an application to turn this functionality on and off.  It is
** recommended that applications that use this routine provide an
** application resource called remapDeleteKey so savvy users can get
** their forward delete functionality back.
*/
void RemapDeleteKey(Widget w)
{
    static XtTranslations table = NULL;
    static char *translations = "<Key>osfDelete: delete-previous-character()\n";

    if (RemapDeleteEnabled) {
    	if (table == NULL)
    	    table = XtParseTranslationTable(translations);
    	XtOverrideTranslations(w, table);
    }
}


void SetDeleteRemap(int state)
{
    RemapDeleteEnabled = state;
}
/*   
** BeginWait/EndWait
**
** Display/Remove a watch cursor over topCursorWidget and its descendents
*/
void SetWatchCursor(Widget topCursorWidget)
{
    Display *display = XtDisplay(topCursorWidget);
    Pixmap pixmap;
    Pixmap maskPixmap;
    XColor xcolors[2];
    int scr;
    static Cursor  waitCursor = 0;
    
    /* if the watch cursor hasn't been created yet, create it */
    if (!waitCursor) {
	pixmap = XCreateBitmapFromData(display, DefaultRootWindow(display),
		(char *)watch_bits, watch_width, watch_height);

	maskPixmap = XCreateBitmapFromData(display, DefaultRootWindow(display),
		(char *)watch_mask_bits, watch_width, watch_height);

	xcolors[0].pixel = BlackPixelOfScreen(DefaultScreenOfDisplay(display));
	xcolors[1].pixel = WhitePixelOfScreen(DefaultScreenOfDisplay(display));

	XQueryColors(display, DefaultColormapOfScreen(
		DefaultScreenOfDisplay(display)), xcolors, 2);
	waitCursor = XCreatePixmapCursor(display, pixmap, maskPixmap,
		&xcolors[0], &xcolors[1], watch_x_hot, watch_y_hot);
	XFreePixmap(display, pixmap);
	XFreePixmap(display, maskPixmap);
    }

    /* display the cursor and flush events */
    XDefineCursor(display, XtWindow(topCursorWidget), waitCursor);
    XFlush(display);
}

void SetDefaultCursor(Widget topCursorWidget)
{
    XUndefineCursor(XtDisplay(topCursorWidget), XtWindow(topCursorWidget));
}

