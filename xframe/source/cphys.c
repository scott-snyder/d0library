/*
        cphys.c
         Created           : 16-DEC-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */
#include <math.h>
#include "/d0lib/scratch/xframe/source/d0x_c.h"
#include <Xm/RowColumn.h>
#include <Xm/ToggleB.h>
#include <Xm/Label.h>
#include <Xm/Text.h>
#include <Xm/DrawingA.h>

#define IBMAIX 2
#define ELECTRON 0
#define PHOTON 1
#define MUON 2
#define JET 3
#define PNUT 4
#define COMPOSITE 5
#define CCRAD 84.455
#define CCZEND 130.3
#define PI 3.141592653
#define RADDEG 57.2957795
#define DEGRAD .017453292
#define OBJMAX 100
#define RADIANS 0
#define DEGREES 1
#define   CFMAX(a,b) ((a) > (b) ? (a) : (b))
#define   CFMIN(a,b) ((a) < (b) ? (a) : (b))
#define  EMLSIDE 10
XtIntervalId timer_id;
static void selectit(), variab();
Widget rowcol;
XFontStruct *font, *font1, *fsymb;
XmFontList  fontlist1, fontlist, symblist;
int nrows;
GC gcxy, gcrz, gcxz, gclego, gceml;
XGCValues gcvxy, gcvrz, gcvxz, gcvlego, gcveml;
XtGCMask gcmask;
Position xm, ym, xx, x2, yy, y2, x1RZ, y1RZ, xc, yc, rads, x1EML,y1EML;
Display *DisplayXY, *DisplayLEGO, *DisplayRZ, *DisplayEML;
Window WindowXY, WindowLEGO, WindowRZ, WindowEML;
Dimension widthRZ, heightRZ, widthXY, heightXY, heightEML, widthEML;
int radius;
double xRZx,yRZx,zRZx;
double xRZy,yRZy,zRZy;
double xRZz,yRZz,zRZz;
int timeout = 400;
int radordeg = RADIANS;
/*
  here is the arrays of info for the physics objects
*/
union {
	int ival;
	char cval[4];
	} uval;
int nobj, nvert, maxobj=OBJMAX, nrun, nev;
float et[OBJMAX],eta[OBJMAX],phi[OBJMAX],mass[OBJMAX],emf[OBJMAX];
float mt[OBJMAX],rjet[OBJMAX], zvert[OBJMAX], dzvert[OBJMAX];
float e[OBJMAX],px[OBJMAX],py[OBJMAX],pz[OBJMAX],theta[OBJMAX];
float pt[OBJMAX], sfac[OBJMAX];
XmString partid[OBJMAX], partidn[OBJMAX];
int otype[OBJMAX], osel[OBJMAX], baddr[OBJMAX];
int otypen[OBJMAX], oseln[OBJMAX], baddrn[OBJMAX];
int do_et=1, do_eta=1, do_phi=1, do_mass=1, do_mt=1, do_emf=0, do_rjet=0;
int do_e=1, do_px=1, do_py=1, do_pz=1, do_theta=0, do_pt = 0;
int do_sfac=1;
float threshold = 0.0;
float phi0 = 0.0, theta0 = 0.0, psi0 = 0.0;
float etn[OBJMAX],etan[OBJMAX],phin[OBJMAX],massn[OBJMAX],emfn[OBJMAX];
float mtn[OBJMAX],rjetn[OBJMAX], zvertn[OBJMAX], dzvertn[OBJMAX];
float en[OBJMAX],pxn[OBJMAX],pyn[OBJMAX],pzn[OBJMAX],thetan[OBJMAX];
float ptn[OBJMAX], sfacn[OBJMAX], mczvert[OBJMAX];
int jetid[OBJMAX],nmcobj, mcnvert;
int use_ecorr = 0;   /* 0=no, 1=mc, 2=data */
int dojnep = 0;        /* same as above */
char cstr[100];
int first=1;


cphys(w,tag,reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
	int n, i, j, k, select = *tag, indmax, length, dummy, status, deml;
	int iscale, ipnut, ilep, nquans;
	Arg wargs[20];
	char string[1000], iquans[1200], substr[100], *pstr;
	Widget toggle;
	double dm, dpt, dp, dpt2, dpx, dpy, dpz, de, det, deta, dphi, dtheta;
	double dalpha, dotp, alf2, a2, b2, c2;
#if D0FLAVOR==IBMAIX
	double sqrt;
#else
	double sqrt();
#endif
	double px1, px2, py1, py2, pz1, pz2, p1, p2, phi1, phi2, theta1, theta2;
	double ptry,pmax,fx1,fx2,fy1,fy2, coshdeta, etanu, thetanu;
	double cp0, sp0, ct0, st0, cs0, ss0, xe, ye, ze;
	float falpha, ftheta, fphi, etmax, xnorm, ynorm, depths[5], quans[100];
	float lep4vec[4], nu2vec[2], w4vec[4], wmass = 80., kz1, kz2;
	Dimension width, height, swid, shei;
	char filename[100];
	FILE *fpo, *fopen();

	if (first) {
		first = 0;
		for (i=0; i<OBJMAX; i++) sfac[i]=1.0;
	}
/*
  load up fonts
*/
	if ( font == 0 ) {
		font = XLoadQueryFont(XtDisplay(toplevel_widget),
/*			"-*-fixed-medium-r-*-*-*-120-*-*-*-*-*-*");*/
"-misc-fixed-medium-r-normal-*-13-120-75-75-c-70-iso8859-1");
		fontlist = XmFontListCreate(font,"charset");
 	}
	if ( font1 == 0 ) {
		font1 = XLoadQueryFont(XtDisplay(toplevel_widget),
			"-*-fixed-medium-r-*-*-*-100-*-*-*-*-*-*");
		fontlist1 = XmFontListCreate(font1,"charset1");
	}
	if ( fsymb == 0 ) {
		fsymb = XLoadQueryFont(XtDisplay(toplevel_widget),
			"-adobe-symbol-*-*-*-*-*-120-*-*-*-*-*-*");
		symblist = XmFontListCreate(fsymb,"symbset");
	}
/*
    do it
*/
	switch (select) {
		case 20:        /* angles in radians */
			radordeg = RADIANS;
			break;
		case 19:        /* angles in degrees */
			radordeg = DEGREES;
			break;
		case 18:        /* type out MC info if present */
#ifdef D0FLAVOR
			fgobjmc_(
#else
			fgobjmc(
#endif
			&nmcobj,&maxobj,jetid,otypen,etn,etan,phin,massn,en,
				pxn,pyn,pzn,ptn,thetan,&mcnvert,mczvert);
			if ( nmcobj > 0 ) {
				strcpy(string,"        'JET'   ET     ETA    PHI   MASS   ");
				strcat(string,"  E     PX     PY     PZ\n");
				for ( i=0; i<nmcobj; i++ ) {
					uval.ival = otypen[i];
					sprintf(substr,"%s",uval.cval);
					substr[4] = '\0';
					strcat(string,substr);

					sprintf(substr,"     %3d %6.1f %6.1f %6.1f %6.2f ",
						jetid[i],etn[i],etan[i],phin[i],massn[i]);
					strcat(string,substr);

				    sprintf(substr,"%6.1f %6.1f %6.1f %6.1f\n",
				    	en[i],pxn[i],pyn[i],pzn[i]);
					strcat(string,substr);
				}
				XmTextSetString(physhelp,string);
			}
			else {
				n = 0;
				XmTextSetString(physhelp,"No ISAJET Info Available");
			}
			n = 0;
			XtSetArg(wargs[n], XmNdialogTitle,
					XmStringCreateSimple("ISAJ/ISAQ Info"));n++;
			XtSetValues(physhelp_bull, wargs, n);
			break;
		case 17:        /* print physics list */
			printf("Name of output file to create: ");
			gets(filename);
/*
			xgetchar("Name of output file to create:","",
				filename,&status);
*/
			fpo = fopen(filename,"w");
			fprintf(fpo,"Run %d  Event %d\n",nrun,nev);
			if ( nvert < 1 ) {
				fprintf(fpo,"No primary vertices found\n");
			}
			else {
				fprintf(fpo,"Found %d primary vertices:\n",nvert);
				for ( i=0; i<nvert; i++ ) {
					fprintf(fpo,"%6.1f ",zvert[i]);
				}
				fprintf(fpo,"\n");
			}
			fprintf(fpo," Object      ET    ETA    PHI   MASS     EMF   ");
			fprintf(fpo,
				"RJET    MT      E     PX     PY     PZ     Pt  Scale\n");
			for (i=0; i<nobj; i++) {
				k = otype[i];
				switch (k) {
					case ELECTRON:          /* electron */
						fprintf(fpo,"ele(PELC) ");
						break;
					case PHOTON:          /* photon */
						fprintf(fpo,"gam(PPHO) ");
						break;
					case MUON:          /* muon */
						fprintf(fpo,"muo(PMUO) ");
						break;
					case JET:          /* jet */
						fprintf(fpo,"jet(JETS) ");
						break;
					case PNUT:          /* neutrino */
						fprintf(fpo,"nu (PNUT) ");
						break;
					case COMPOSITE:
					default:         /* composite */
						XmStringGetLtoR(partid[i],
							XmSTRING_DEFAULT_CHARSET,&pstr);
						strncpy(string,pstr,strlen(pstr));
						string[strlen(pstr)] = '\0';
						if ( strlen(pstr) > 10 ) string[10] = '\0';
						fprintf(fpo,"%s",string);
						break;
				}
				fprintf(fpo,"%6.1f %6.1f %6.1f %6.1f %6.2f ",
					et[i],eta[i],phi[i],mass[i],emf[i]);
				fprintf(fpo,
					"%6.2f %6.1f %6.1f %6.1f %6.1f %6.1f %6.1f %6.3f\n",
					rjet[i],mt[i],e[i],px[i],
					py[i],pz[i],pt[i],sfac[i]);
			}
			fclose(fpo);
			break;
		case 16:        /* pmuo list stuff */
/*
            valid selection?
*/
			for (n= -1, i=0; i<nobj; i++) {
				if ( osel[i] &&	( otype[i] == MUON ) ) n = i;
			}
			if ( n<0 ) break;
#ifdef D0FLAVOR
			muostuff_(&baddr[n],quans);
#else
			muostuff(&baddr[n],quans);
#endif
			sprintf(string,
"VERSION     %7.0f\nCHARGE      %7.0f\nDEDX FLAG   %7.0f\n\
METHOD CALC %7.0f\nTRKFLG      %7.0f\nNO. CD TKS  %7.0f\nQUAD        %7.0f\n\
METHOD FIT  %7.0f\nIFW4        %7.0f\nPX          %7.1f\nPY          %7.1f\n\
PZ          %7.1f\nP           %7.0f\nPt          %7.1f\nTHETA       %7.2f\n\
ETA         %7.2f\nPHI         %7.2f\n(sigP )**2  %7.1f\n(sigPt)**2  %7.1f\n\
CHI**2/DF   %7.2f\nT0_OFF      %7.1f\nXTRK        %7.1f\nYTRK        %7.1f\n\
ZTRK        %7.2f\nECAL(EXP)   %7.2f\nECAL(HIT)   %7.2f\nECAL(.4)    %7.2f\n\
ECAL(.6)    %7.2f\nMU-CD ANGLE %7.2f\nD_phi(deg)  %7.1f\nD_theta(deg)%7.1f\n\
CD-cone     %7.2f\nIMPACT(VTX) %7.2f\nIMPACT(FIT) %7.2f\nELOSS(MU)   %7.2f\n\
HITS     %10.0f\nHIST(FIT)%10.0f\nTRIG1       %7.0f\nTRIG2       %7.0f\n\
TRIG3       %7.0f\nTRIG4       %7.0f\nVTX NUMBER  %7.0f\n",
 				quans[0],quans[1],quans[2],quans[3],quans[4],quans[5],
 				quans[6],quans[7],quans[8],quans[9],quans[10],quans[11],
 				quans[12],quans[13],quans[14],quans[15],quans[16],quans[17],
 				quans[18],quans[19],quans[20],quans[21],quans[22],quans[23],
 				quans[24],quans[25],quans[26],quans[27],quans[28],quans[29],
 				quans[30],quans[31],quans[32],quans[33],quans[34],quans[35],
 				quans[36],quans[37],quans[38],quans[39],quans[40],quans[41]);
 			XmTextSetString(pmuolist,string);
			break;
		case 14:        /* reset psi0 */
			psi0 = 0.;
			SetLabel(psi0lab,"Z' Axis (by 0)");
			ResetPN();
			DispRZ();
			break;
		case 13:        /* reset phi0 */
			phi0 = 0.;
			SetLabel(phi0lab,"Z Axis (by 0)");
			ResetPN();
			DispRZ();
			dummy = 9;
			cphys(w,&dummy,reason);
			break;
		case 12:        /* reset theta0 */
			theta0 = 0.;
			SetLabel(theta0lab,"Y' Axis (by 0)");
			ResetPN();
			DispRZ();
			break;
		case 10:       /* constrain to W mass, produced 2 solutions */
/*
            make sure use selected a pnut and a lepton
*/
			for (ilep= -1,ipnut= -1,i=0; i<nobj; i++) {
				if (osel[i]) {
					if (otype[i]== PNUT) ipnut = i;
					else ilep = i;
				}
			}
			if (ipnut < 0 ) {
				warning("PNUT  selection MANDATORY - try again");
				break;
			}
			if (ilep < 0 ) {
				warning("non-PNUT selection MANDATORY - try again");
				break;
			}
/*
  			ok, got a pnut and non-put - now create two additional objects
  			which make a W mass (use 80 GeV)

  			use the Serban's routine
*/
			lep4vec[3] = sfac[ilep]*e[ilep];
			lep4vec[0] = sfac[ilep]*px[ilep];
			lep4vec[1] = sfac[ilep]*py[ilep];
			lep4vec[2] = sfac[ilep]*pz[ilep];
			nu2vec[0] = sfac[ipnut]*px[ipnut];
			nu2vec[1] = sfac[ipnut]*py[ipnut];
#ifdef D0FLAVOR
			find_wlnu_(&wmass,lep4vec,nu2vec,w4vec,&kz2,&dummy);
#else
			find_wlnu(&wmass,lep4vec,nu2vec,w4vec,&kz2,&dummy);
#endif
			if ( dummy != -1 ) {
				warning("No solution (MT>80 GeV)");
				break;
			}
/*
   		    unmange rowcolumn widget
*/
		    XtUnmanageChild(rowcol);
/*
            make first one
*/
			sprintf(substr,"%d+%d+",ilep+1,ipnut+1);
			osel[nobj] = 0; otype[nobj] = 5; emf[nobj] = 0.; rjet[nobj] = 0.;
			e[nobj] = w4vec[3];
			px[nobj] = w4vec[0];
			py[nobj] = w4vec[1];
			pz[nobj] = w4vec[2];
			pt[nobj] = sqrt(px[nobj]*px[nobj] + py[nobj]*py[nobj]);
			mass[nobj] = sqrt(e[nobj]*e[nobj] - 
			                  pt[nobj]*pt[nobj] - pz[nobj]*pz[nobj]);
			et[nobj] = sqrt(pt[nobj]*pt[nobj] + mass[nobj]*mass[nobj]);
			mt[nobj] = sqrt(et[nobj]*et[nobj] - 
			                  pt[nobj]*pt[nobj] );
			dp = sqrt( px[nobj]*px[nobj] + py[nobj]*py[nobj] +
					pz[nobj]*pz[nobj]);
			dpz = pz[nobj];
			dtheta = acos( dpz / dp);  theta[nobj] = dtheta;
			deta = -log( tan( .5*dtheta) );  eta[nobj] = deta;
			phi[nobj] = atan2( py[nobj], px[nobj]);
			if ( phi[nobj] < 0.0) phi[nobj] += 2*PI;
			NewRow(nobj,substr,0);  nobj += 1;
/*
            make second one
*/
			sprintf(substr,"%d+%d-",ilep+1,ipnut+1);
			osel[nobj] = 0; otype[nobj] = 5; emf[nobj] = 0.; rjet[nobj] = 0.;
			kz1 = kz2 - lep4vec[2];
			e[nobj] = lep4vec[3] + sqrt(nu2vec[0]*nu2vec[0] +
			                            nu2vec[1]*nu2vec[1] + kz1*kz1);
			px[nobj] = w4vec[0];
			py[nobj] = w4vec[1];
			pz[nobj] = kz2;
			pt[nobj] = sqrt(px[nobj]*px[nobj] + py[nobj]*py[nobj]);
			mass[nobj] = sqrt(e[nobj]*e[nobj] - 
			                  pt[nobj]*pt[nobj] - pz[nobj]*pz[nobj]);
			mt[nobj] = sqrt(e[nobj]*e[nobj] - 
			                  pt[nobj]*pt[nobj] );
			et[nobj] = sqrt(pt[nobj]*pt[nobj] + mass[nobj]*mass[nobj]);
			dp = sqrt( px[nobj]*px[nobj] + py[nobj]*py[nobj] +
					pz[nobj]*pz[nobj]);
			dpz = pz[nobj];
			dtheta = acos( dpz / dp);  theta[nobj] = dtheta;
			deta = -log( tan( .5*dtheta) );  eta[nobj] = deta;
			phi[nobj] = atan2( py[nobj], px[nobj]);
			if ( phi[nobj] < 0.0) phi[nobj] += 2*PI;
			NewRow(nobj,substr,0);  nobj += 1;
/*
            re-manage rowcol widget
*/
            XtManageChild(rowcol);
			break;
		case 9:        /* make xy physics event display */
			if ( nobj < 1 ) break;
		    if ( DisplayXY == 0 ) DisplayXY = XtDisplay(drawitxy);
		    if ( WindowXY == 0 ) WindowXY = XtWindow(drawitxy);
			XClearWindow(DisplayXY,WindowXY);
/*
            initialize graphics for xy widget
*/
			if ( gcxy == 0 ) {
				gcmask = GCForeground | GCBackground;
				gcvxy.foreground = BlackPixelOfScreen(XtScreen(drawitxy));
				gcvxy.background = WhitePixelOfScreen(XtScreen(drawitxy));
				gcxy = XCreateGC(DisplayXY,WindowXY,gcmask,&gcvxy);
/*
            	get height and width of draw widget
*/
				n = 0;
				XtSetArg(wargs[n], XmNwidth, &widthXY); n++;
				XtSetArg(wargs[n], XmNheight, &heightXY); n++;
				XtGetValues(drawitxy, wargs, n);
				radius = CFMIN(heightXY,widthXY)/2; de = radius;
				radius = .75*de;  de = radius;
				xc = widthXY/2; yc = heightXY/2;
			}
/*
            renormalize:  put 0,0 at height/2 and width/2 with positive y
  			              direction downward (widgets always start at 0,0
                          is upper left hand corner and positive y downward)
  					      reduce by 10% for the id
            get max p(xy) of all objects
*/
			pmax = 0.;
			for (i=0; i<nobj; i++) {
				px1 = px[i];
				py1 = py[i];
				ptry = sqrt( px1*px1 + py1*py1 );
				if ( ptry > pmax ) pmax = ptry;
			}
		    xnorm = .95*radius/pmax;
/*
            loop over all objects
*/
			for (i=0; i<nobj; i++) {
					dphi = atan2( py[i], px[i] );
					if ( dphi < 0 ) dphi += 2*PI;
					px1 = px[i];
					py1 = py[i];
					ptry = sqrt( px1*px1 + py1*py1 );
					fx2 = xc + xnorm*ptry*cos(dphi + phi0);
					fy2 = yc - xnorm*ptry*sin(dphi + phi0);
					x2 = fx2;
					y2 = fy2;
					XDrawLine(DisplayXY,WindowXY,gcxy,xc,yc,x2,y2);
/*
  					put label at outside of circle and draw it
*/
					sprintf(substr,"%d",i+1);
					length = strlen(substr);
					fx2 = xc + 1.2*radius*cos(dphi + phi0);
					fy2 = yc - 1.2*radius*sin(dphi + phi0);
					x2 = fx2;
					y2 = fy2;
					XDrawString(DisplayXY,WindowXY,gcxy,x2,y2,substr,length);
			}
/*
            draw circle around the whole thing
*/
			DrawCircle(drawitxy,gcxy,xc,yc,radius);
/*
            draw a little tick at -phi0
*/
  			fx1 = xc + radius*cos(phi0);
  			fy1 = yc - radius*sin(phi0);
  			fx2 = xc + 1.05*radius*cos(phi0);
  			fy2 = yc - 1.05*radius*sin(phi0);
  			xx = fx1; yy = fy1; x2 = fx2; y2 = fy2;
		    XDrawLine(DisplayXY,WindowXY,gcxy,xx,yy,x2,y2);
		    break;
		case 8:  			/* make lego plot */
			if ( nobj < 1 ) break;
			if ( DisplayLEGO == 0 ) DisplayLEGO = XtDisplay(drawitlego);
			if ( WindowLEGO == 0 ) WindowLEGO = XtWindow(drawitlego);
			XClearWindow(DisplayLEGO,WindowLEGO);
/*
            initialize graphics for lego widget
*/
			if ( gclego == 0 ) {
				gcmask = GCForeground | GCBackground;
				gcvlego.foreground = BlackPixelOfScreen(XtScreen(drawitlego));
				gcvlego.background = WhitePixelOfScreen(XtScreen(drawitlego));
				gclego = XCreateGC(DisplayLEGO,WindowLEGO,gcmask,&gcvlego);
			}
/*
            get height and width of draw widget
*/
			n = 0;
			XtSetArg(wargs[n], XmNwidth, &width); n++;
			XtSetArg(wargs[n], XmNheight, &height); n++;
			XtGetValues(drawitlego, wargs, n);
			shei = height; swid = width;
/*
            start rectangle at 20,20 with width and height reduced by 40
*/
			xx = 30; yy = 30;
			xm = 30; ym = 30;
	        XDrawRectangle(DisplayLEGO,WindowLEGO,
	        	gclego,xm,ym,width-2*xm,height-2*ym);
/*
            draw eta/phi labels
*/
/*
	        XmStringDraw(DisplayLEGO,WindowLEGO,
	        	symblist,XmStringCreate("j","symbset"),
	        	gclego,10,height/2,
	        	10,XmALIGNMENT_CENTER,XmSTRING_DIRECTION_L_TO_R,NULL);
	        XmStringDraw(DisplayLEGO,WindowLEGO,
	        	symblist,XmStringCreate("h","symbset"),
	        	gclego,width/2,height-10,
	        	10,XmALIGNMENT_CENTER,XmSTRING_DIRECTION_L_TO_R,NULL);
*/
	        XDrawString(DisplayLEGO,WindowLEGO,gclego,
	        	2,2*height/3,"phi",3);
	        XDrawString(DisplayLEGO,WindowLEGO,gclego,
	        	2*width/3,height-5,"eta",3);
/*
  			draw circles centered at eta/phi for all jets, label electrons,
  			photons and gammas with letters
*/
			xx = width/2; yy = height/2;
			width = width - 2*xm; height = height - 2*ym;
			xnorm = width/10; ynorm = height/(2*PI);
			for (i=0; i<nobj; i++) {
					dphi = phi[i]; deta = eta[i];
/*
                    normalize eta to +- 5 and phi to 2*PI
*/
					fx2 = xx + xnorm*deta;
					fy2 = yy - ynorm*(dphi-PI);
					x2 = fx2;
					y2 = fy2;
/*
                    draw the id of the object
*/
					sprintf(string,"%d",i+1);
					length = strlen(string);
					XDrawString(DisplayLEGO,WindowLEGO,
									gclego,x2,y2,string,length);
/*
                    for jets, draw a circle of radius "rjet" (min 2, and
                    normalize to rapidity of +- 5)
*/
					k = otype[i];
					if ( k == 3) {
						rads = rjet[i]*width/10;
						if ( rads < 2 ) rads = 2;
						DrawCircle(drawitlego,gclego,x2,y2,rads);
					}
			}
/*
            mark the primary vertices: eta(vertex) is calculated using
  			the triangle of z=0 r=0, zvertex r=0, and z=0 r=85cm which is
            the radius up to the calorimeter
*/
			for ( i=0, fy1=CCRAD; i<nvert; i++) {
				fx1 = zvert[i];
				dtheta = atan2( fy1, fx1 );
				if ( dtheta < 0.0 ) dtheta = -dtheta;
				deta = -log( tan( .5*dtheta ) );
				x2 = xx + xnorm*deta;
				XDrawString(DisplayLEGO,WindowLEGO,gclego,
					x2,ym+height+8,"*",1);
			}
/*
            draw axis ticks (every 1 eta, every 1 phi)
*/
			for (i = -4; i<6; i += 2) {
				fx2 = xx + xnorm*i;
				x2 = fx2;
				XDrawLine(DisplayLEGO,WindowLEGO,gclego,
					x2,ym,x2,ym-2);
				XDrawLine(DisplayLEGO,WindowLEGO,gclego,
					x2,ym+height,x2,ym+height+2);
				sprintf(string,"%d",i);
				length = strlen(string);
				XDrawString(DisplayLEGO,WindowLEGO,
					gclego,x2-5,ym-5,string,length);
			}
			for (i=0; i<7; i += 2) {
				fy2 = yy - ynorm*(i-PI);
				y2 = fy2;
				XDrawLine(DisplayLEGO,WindowLEGO,gclego,
					xm,y2,xm-2,y2);
				XDrawLine(DisplayLEGO,WindowLEGO,gclego,
					xm+width,y2,xm+width+2,y2);
				sprintf(string,"%d",i);
				length = strlen(string);
				XDrawString(DisplayLEGO,WindowLEGO,
					gclego,xm-10,y2,string,length);
			}
			break;
		case 7:   /* do rz plane */
			if ( nobj < 1 ) break;
			if ( DisplayRZ == 0 ) DisplayRZ = XtDisplay(drawityz);
			if ( WindowRZ == 0 ) WindowRZ = XtWindow(drawityz);
			if ( gcrz == 0 ) {
				gcmask = GCForeground | GCBackground;
				gcvrz.foreground = BlackPixelOfScreen(XtScreen(drawityz));
				gcvrz.background = WhitePixelOfScreen(XtScreen(drawityz));
				gcrz = XCreateGC(DisplayRZ,WindowRZ,gcmask,&gcvrz);
/*
  				get height and width of draw widget
*/
				n = 0;
				XtSetArg(wargs[n], XmNwidth, &widthRZ); n++;
				XtSetArg(wargs[n], XmNheight, &heightRZ); n++;
				XtGetValues(drawityz, wargs, n);
				x1RZ = widthRZ/2; y1RZ = heightRZ/2;
			}
/*
            display all objects, no rotations (initial display)
*/
			DispRZ();
			break;
		case 6:        /* printout pelc/ppho stuff */
/*
            make the longitudinal profile in drawemlong
*/
			if ( DisplayEML == 0 ) DisplayEML = XtDisplay(drawemlong);
			if ( WindowEML == 0 ) WindowEML = XtWindow(drawemlong);
			if ( gceml == 0 ) {
				gcmask = GCForeground | GCBackground;
				gcveml.foreground = BlackPixelOfScreen(XtScreen(drawemlong));
				gcveml.background = WhitePixelOfScreen(XtScreen(drawemlong));
				gceml = XCreateGC(DisplayEML,WindowEML,gcmask,&gcveml);
/*
  				get height and width of draw widget
*/
				n = 0;
				XtSetArg(wargs[n], XmNwidth, &widthEML); n++;
				XtSetArg(wargs[n], XmNheight, &heightEML); n++;
				XtGetValues(drawemlong, wargs, n);
				x1EML = widthEML/2; y1EML = heightEML/2;
			}
			XClearWindow(DisplayEML, WindowEML);
/*
            valid selection?
*/
			for (n= -1, i=0; i<nobj; i++) {
				if ( osel[i] &&
					( (otype[i] == ELECTRON) ||
				      (otype[i] == PHOTON) ) ) n = i;
			}
			if ( n<0 ) break;
/*
            now get all interesting stuff and put in emlist
*/
#ifdef D0FLAVOR
			fgemlist_
#else
			fgemlist
#endif
				(&baddr[n],quans,iquans,&nquans,depths);

/*
            check that there is something returned here, if so
  			then put it all into a char string
*/
			if (nquans > 0 ){
				sprintf(string," %s   %7.2f\n",
						iquans,quans[0]);
				for (i=1; i<nquans; i++) {
					sprintf(substr," %s   %7.2f\n",
					      iquans+12*i,quans[i]);
					strcat(string,substr);
				}
			}
			XmTextSetString(emlist,string);
			etmax = 0;
			for (i=0; i<5; i++) if (depths[i]>etmax) etmax = depths[i];
			if (etmax < 0.001) break;
			dm = widthEML - 2*EMLSIDE; dm = dm/5.;  /* leave 10 on each side */
			deml = dm;
			xm = EMLSIDE;                     /* start at 10 */
			ynorm = .8*heightEML/etmax;       /* 80% */
			yy = .1*heightEML;				  /* put value here */
			XSetFillStyle(DisplayEML, gceml, FillStippled);
			for (i=0; i<5; i++) {
				ym = ynorm*depths[i];
/*				XDrawRectangle(DisplayEML,WindowEML,gceml,
					xm,heightEML-ym,deml,ym);
*/
				XFillRectangle(DisplayEML,WindowEML,gceml,
					xm,heightEML-ym,deml,ym);
				sprintf(string,"%5.3f",depths[i]);
				length = strlen(string);
				XDrawString(DisplayEML,WindowEML,gceml,
					xm,yy,string,length);
				xm += deml;
			}
			break;
		case 5:        /* apply scale factor from scale widget */
			XmScaleGetValue(scalefac,&iscale);
			for (i=0; i<nobj; i++) {
				if (osel[i]) {
/*
                    take out old factor
*/
					et[i] = et[i]/sfac[i];
					e[i] = e[i]/sfac[i];
					px[i] = px[i]/sfac[i];
					py[i] = py[i]/sfac[i];
					pz[i] = pz[i]/sfac[i];
					pt[i] = pt[i]/sfac[i];
					mass[i] = mass[i]/sfac[i];
					mt[i] = mt[i]/sfac[i];
/*
                    put in new one
*/
					sfac[i] = .01*iscale;   /*.01 is set in widget resource */
					et[i] = et[i]*sfac[i];
					e[i] = e[i]*sfac[i];
					dpx = px[i]*(1.-sfac[i]);
					dpy = py[i]*(1.-sfac[i]);
					px[i] = px[i]*sfac[i];
					py[i] = py[i]*sfac[i];
					pz[i] = pz[i]*sfac[i];
					pt[i] = pt[i]*sfac[i];
					mass[i] = mass[i]*sfac[i];
					mt[i] = mt[i]*sfac[i];
					osel[i] = 0;
				}
			}
/*
  			now correct MET
*/
			for (i=0; i<nobj; i++) {
				if (otype[i] == PNUT) {
					px[i] = px[i] + dpx;
					py[i] = py[i] + dpy;
					pt[i] = sqrt( px[i]*px[i] + py[i]*py[i] );
					et[i] = pt[i];
					phi[i] = atan2( py[i], px[i]);
					if ( phi[i] < 0.0) phi[i] += 2*PI;
					e[i] = sqrt( pt[i]*pt[i] + pz[i]*pz[i] );
				}
			}
/*
            remake rowcol widget
*/
			MakeRCPhysics();
/*
  			fill the widget
*/
			FillRcPhysics(1);
/*
            manage rowcol widget
*/
			XtManageChild(rowcol);
			break;
		case 4:		   /* delete selected objects */
/*
            first move into "n" array 
*/
			for ( i=0; i<nobj; i++ ) 
				cp_to_n(i,i);
/*
            copy back only those not selected
*/
			for ( k=0, i=0; i<nobj; i++ ) {
				if ( osel[i] == 0 ) {
					cp_from_n(k,i);
					k++;
				}
			}
			nobj = k;
/*
            remake rowcol widget
*/
			MakeRCPhysics();
/*
  			fill the widget
*/
			FillRcPhysics(1);
/*
            manage rowcol widget
*/
			XtManageChild(rowcol);
			break;
		case 3:        /* set threshold on objects and reget */
			XmScaleGetValue(thresh,&iscale);
			threshold = iscale;
			break;
		case 2:        /* calculate angles (dalpha, dtheta, dphi) 
		                  using the first two selected ones */
		   for ( i=0, k=0; i<nobj; i++) {
		   	  if ( osel[i] ) {
		   	  	if ( k == 0 ) {
		   	  		px1 = px[i]; py1 = py[i]; pz1 = pz[i];
		   	  		phi1 = phi[i]; theta1 = theta[i];
		   	  		k += 1;
		   	  	}
		   	  	else if ( k == 1 ) {
		   	  		px2 = px[i]; py2 = py[i]; pz2 = pz[i];
		   	  		phi2 = phi[i]; theta2 = theta[i];
		   	  		k += 1;
		   	  	}
		   	  }
		   }
		   if ( k > 1 ) {
		   	 p1 = sqrt( px1*px1 + py1*py1 + pz1*pz1 );
		   	 p2 = sqrt( px2*px2 + py2*py2 + pz2*pz2 );
		   	 dotp = px1*px2 + py1*py2 + pz1*pz2;
		   	 dotp = dotp/(p1*p2);
		   	 dalpha = acos(dotp);
		   	 dphi = phi1 - phi2 ;
		   	 if ( dphi < 0 ) dphi = -dphi;
		   	 dtheta = theta1 - theta2;
		   	 if ( dtheta < 0 ) dtheta = -dtheta;
		   	 falpha = dalpha;
		   	 ftheta = dtheta;
		   	 fphi = dphi;
		   	 sprintf(string,"3-D Angle: %5.2f  Theta: %5.2f Phi: %5.2f",
		   	 	falpha,ftheta,fphi);
		   	 SetLabel(anglelab,string);
		   }
		   break;
		case 1:        /* 1 means add n-vectors of selected ones */
		   for (n=0, i=0; i<nobj; i++) {
		   	 if ( osel[i] ) n++;
		   }
		   if ( !n ) break;
		   e[nobj] = 0.; px[nobj] = 0.; py[nobj] = 0.; pz[nobj] = 0.;
		   et[nobj] = 0.;
		   for ( i=0, k=0; i<nobj; i++) {
		   	  if ( osel[i] ) {
		   	  	sprintf(substr,"%d+",i+1);
		   	  	e[nobj] += e[i];
		   	  	px[nobj] += px[i];
		   	  	py[nobj] += py[i];
		   	  	pz[nobj] += pz[i];
		   	  	et[nobj] += et[i];
		   	  	k += 1;
		   	  	switch (k) {    /* this only makes sense for 2 particles */
		   	  		case 1: 
		   	  			strcpy(cstr,substr);
		   	  			break;
		   	  		case 2:
		   	  			strcat(cstr,substr);
		   	  			break;
		   	  		default:
		   	  			strcat(cstr,substr);
		   	  			break;
		   	  	}
		   	  }
		   }
		   i = strlen(cstr);
		   cstr[i-1] = '\0';
/*
  	       calculate stuff
*/
		   de = e[nobj]; dpx = px[nobj]; dpy = py[nobj]; dpz = pz[nobj];
		   det = et[nobj]; dp = sqrt( dpx*dpx + dpy*dpy + dpz*dpz );
		   dpt2 = dpx*dpx + dpy*dpy;
/*
           transverse mass is DEFINED to be sum(et) - sum(\vec[pt])
*/
		   dm = det*det - dpx*dpx - dpy*dpy;
		   mt[nobj] = sqrt(dm);
/*
           relativistic length
*/
		   dm = de*de - dpx*dpx - dpy*dpy - dpz*dpz;
		   mass[nobj] = sqrt(dm);
/*
           theta is only due to momentum
*/
		   dtheta = acos( dpz/dp);
		   theta[nobj] = dtheta;
/*
           et is DEFINED to be E*sin(theta) - note the difference between
           this definition and the one used in the transverse mass!
*/
		   det = de*sin(dtheta);
		   et[nobj] = det;
/*
           etc.
*/
		   dpt = sqrt( dpt2 );  pt[nobj] = dpt;
		   dphi = atan2( dpy, dpx );
		   phi[nobj] = dphi;
		   if ( phi[nobj] < 0.0 ) phi[nobj] += 2*PI;
		   deta = -log( tan( .5*dtheta ) ); 
		   eta[nobj] = deta ;
		   osel[nobj] = 0;    /* not selected */
		   otype[nobj] = 5;   /* type 5 means composite */
		   rjet[nobj] = 0.;
		   emf[nobj] = 0.;
		   sfac[nobj] = 1.0;
/*
  	       create new row
*/
	       NewRow(nobj,cstr,0);
/*
           re-manage rowcol widget
*/
           XtManageChild(rowcol);
/*
           and finally, increment number of objects
*/
		   nobj += 1;
		   break;
		case 0:        /* create physics menu (the default) */
		default:
/*
  			make an unmanaged rowcolumn widget which will contain 1 row 
  			per physics object (note: for rc widgets, if you specify 
  			horizontal then numcolums is the number of rows!!!! how 
  			about that!!!!
*/
			MakeRCPhysics();
/*
  			gather all of the physics objects
*/
#ifdef D0FLAVOR
			fgobj_(
#else
			fgobj(
#endif
			&threshold,&use_ecorr,&dojnep,&nobj,&maxobj,baddr,otype,
				et,eta,phi,mass,e,px,py,pz,pt,
				theta,emf,rjet,mt,&nvert,zvert,dzvert,&nrun,&nev);
/*
  			initialize some arrays
*/
			for ( i=0; i<nobj; i++) {
				sfac[i] = 1.0;
				osel[i] = 0;
			}
			xRZx = 1.; xRZy = 0.; xRZz = 0.;
			yRZx = 0.; yRZy = 1.; yRZz = 0.;
			zRZx = 0.; zRZy = 0.; zRZz = 1.;
/*
            set dialog title
*/
			sprintf(string,"Run %d Event %d",nrun,nev);
			n = 0;
			XtSetArg(wargs[n], XmNdialogTitle,
				XmStringCreateSimple(string)); n++;
			XtSetValues(physics_bull, wargs, n);
/*
  			fill the widget
*/
			FillRcPhysics(0);
/*
            manage rowcol widget
*/
			XtManageChild(rowcol);
/*
            fill label of verticies
*/
			ReportVertices();
		break;
	}
}

ReportVertices()
{
	char string[100], substr[100];
	int i;
	
	if ( nvert < 1 ) 
		SetLabel(vertlab,"No Primary Vertices Found");
	else {		
		sprintf(string,"%d Primary Vertices: ",nvert);
		for (i=0; i<nvert; i++) {
			sprintf(substr,"%6.1f ",zvert[i]);
			strcat(string,substr);
		}
		SetLabel(vertlab,string);
	}
}

FillRcPhysics(mode)     /* mode=0, new labels, 1 means leave alone */
int mode;
{
	int i,k;
	char string[20];
/*
  make labels for rc widget
*/
	MakeRCLables();
/*
  loop over all objects, type out useful information into 
  rowcolumn widget (make a toggle button widget and fixed font 
  labels for each)
*/
	for (i=0; i<nobj; i++) NewRow(i,string,mode);
}

MakeRCPhysics()       /* makes the rowcol widget */
{
	int n;
	Arg wargs[10];

	n = 0;
	XtSetArg(wargs[n], XmNpacking, XmPACK_COLUMN); n++; 
	XtSetArg(wargs[n], XmNorientation, XmHORIZONTAL); n++;
/*	XtSetArg(wargs[n], XmNentryAlignment, XmALIGNMENT_END); n++;*/
	XtSetArg(wargs[n], XmNnumColumns, 1); n++; nrows=1;  /* 1 row */
	if ( rowcol ) XtDestroyWidget(rowcol);
	rowcol = XtCreateWidget("rowcolumn", xmRowColumnWidgetClass,
				physics_scroll, wargs, n);
}

static void variab(w,tag,reason)    /* which rowcol text field was changed */
Widget w;
int tag;
unsigned long reason;
{
	int row,col;
	char *string, sst[10];
	float val;
	Arg wargs[10];
/*
  row is tag/100, column is tag - 100*tag/100 and 
  look below for correspondence
*/
	row = tag/100;
	col = tag - 100*row;
	string = XmTextGetString(w);
	sscanf(string,"%f",&val);
	switch (col) {
		case 0:       /* et */
			et[row] = val;
			sprintf(sst,"%6.1f",val);
			break;
		case 1:       /* eta */
			eta[row] = val;
			sprintf(sst,"%6.2f",val);
			break;
		case 2:       /* phi */
			phi[row] = val;
			if (radordeg == RADIANS) sprintf(sst,"%6.2f",val);
			sprintf(sst,"%6.1f",val);
		case 3:       /* emf */
			emf[row] = val;
			sprintf(sst,"%6.2f",val);
			break;
		case 4:       /* cone */
			rjet[row] = val;
			sprintf(sst,"%6.2f",val);
			break;
		case 5:       /* m */
			mass[row] = val;
			sprintf(sst,"%6.1f",val);
			break;
		case 6:       /* mt */
			mt[row] = val;
			sprintf(sst,"%6.1f",val);
			break;
		case 7:       /* theta */
			theta[row] = val;
			if (radordeg == RADIANS) sprintf(sst,"%6.2f",val);
			sprintf(sst,"%6.1f",val);
			break;
		case 8:       /* e */
			e[row] = val;
			sprintf(sst,"%6.1f",val);
			break;
		case 9:       /* px */
			px[row] = val;
			sprintf(sst,"%6.1f",val);
			break;
		case 10:       /* py */
			py[row] = val;
			sprintf(sst,"%6.1f",val);
			break;
		case 11:       /* pz */
			pz[row] = val;
			sprintf(sst,"%6.1f",val);
			break;
		case 12:       /* pt */
			pt[row] = val;
			sprintf(sst,"%6.1f",val);
			break;
		case 13:       /* scale */ 
			sfac[row] = val;
			sprintf(sst,"%6.2f",val);
			break;
		default:
			printf(" Error VARIAB ***** Tell FNALD0::DREW \n");
			break;
	}
	XtFree(string);	
	XtSetArg(wargs[0], XmNvalue, sst);
	XtSetValues(w, wargs, 1);
}

static void selectit(w,tag,reason)	/* select object to manipulate */
Widget w;
int tag;
XmToggleButtonCallbackStruct *reason;
{
	Boolean state;
	state = XmToggleButtonGetState(w);
	
	if ( state )            /* state is true, turn on */
	
		osel[tag] = 1;
	else                    /* state is false, turn off */

		osel[tag] = 0;

}

selecor(w,tag,reason) /* toggle using et corrections */
Widget w;
int *tag;
XmToggleButtonCallbackStruct *reason;
{
	int select = *tag;	
	
	switch (select) {
		case 0:                 /* disable et corrections */
			use_ecorr = 0;
			break;
		case 1:					/* enable et corrections jet_et_mccorr */
			use_ecorr = 1;
			break;
		case 4:					/* enable et corrections jet_cal */
			use_ecorr = 2;
			break;
		case 2:
			dojnep = 0; 		/* disable jnep */
			break;
		case 3:
			dojnep = 1;			/* enable jnet */
			break;
		default:
			break;
	}
}

selectc(w,tag,reason)  /* select column to display */
Widget w;
int *tag;
XmToggleButtonCallbackStruct *reason;
{
	Boolean state;
	int which = *tag;	
	int toset;

	state = XmToggleButtonGetState(w);
	
	if ( state )            /* state is true, turn on */
	
		toset = 1;
	else                    /* state is false, turn off */

		toset = 0;

/*
  	which column?
*/

	switch (which) {
		case 0:  do_et = toset;   break;
		case 1:  do_eta = toset;   break;
		case 2:  do_phi = toset;   break;
		case 3:  do_emf = toset;   break;
		case 4:  do_rjet = toset;   break;
		case 5:  do_mass = toset;   break;
		case 6:  do_mt = toset;   break;
		case 7:  do_theta = toset;   break;
		case 8:  do_e = toset;   break;
		case 9:  do_px = toset;   break;
	    case 10:  do_py = toset;   break;
	    case 11:  do_pz = toset;   break;
	    case 12:  do_pt = toset;   break;
	    case 13:  do_sfac = toset;   break;
		default:  break;
	}
}

NewRow(ind,string,mode)  /* ind is index to physics object 
                            string is used if particle is a composite
                            mode = 1 means don't fill labels, use what's there
                         */
int ind,mode;
char *string;
{
	XmString xmstr;
	int n, k;
	Arg wargs[20];
	Widget toggle, w;
	char sst[20];

/*
  add another row, change rowcol rows
*/
	nrows += 1;
	n = 0;
	XtSetArg(wargs[n], XmNnumColumns, nrows); n++;
	XtSetValues(rowcol, wargs, n);
/*
  create the label index
*/
	n = 0;
	XtSetArg(wargs[n], XmNfontList, fontlist); n++;
/* id */
	sprintf(sst,"%2d",ind+1);
	XtSetArg(wargs[n], XmNlabelString,XmStringCreateSimple(sst));n++;
	XtCreateManagedWidget("label",xmLabelWidgetClass,rowcol,wargs,n);
/*
  create toggle button
*/
	n = 0;
	k = otype[ind];
	switch (k) {
		case 0:          /* electron */
			if (!mode) partid[ind] = XmStringCreate("e","charset");
			XtSetArg(wargs[n], XmNfontList, fontlist); n++;
			break;
		case 1:          /* photon */
			if (!mode) partid[ind] = XmStringCreate("g","symbset");
			XtSetArg(wargs[n], XmNfontList, symblist); n++;
			break;
		case 2:          /* muon */
			if (!mode) partid[ind] = XmStringCreate("m","symbset");
			XtSetArg(wargs[n], XmNfontList, symblist); n++;
			break;
		case 3:          /* jet */
			if (!mode) partid[ind] = XmStringCreate("j","charset");
			XtSetArg(wargs[n], XmNfontList, fontlist); n++;
			break;
		case 4:          /* neutrino */
			if (!mode) partid[ind] = XmStringCreate("n","symbset");
			XtSetArg(wargs[n], XmNfontList, symblist); n++;
			break;
		default:         /* composite */
			if (!mode) partid[ind] = XmStringCreateSimple(string);
			XtSetArg(wargs[n], XmNfontList, fontlist); n++;
			break;
	}
	XtSetArg(wargs[n], XmNlabelString, partid[ind]); n++;
	toggle = XtCreateManagedWidget("toggle",xmToggleButtonWidgetClass,
			rowcol, wargs, n);
	XtAddCallback(toggle, XmNvalueChangedCallback, selectit, ind);
/*
  now do each of the columns
*/
	n = 0;
	XtSetArg(wargs[n], XmNfontList, fontlist); n++;
	XtSetArg(wargs[n], XmNshadowThickness, 0); n++;
	XtSetArg(wargs[n], XmNcolumns, 6); n++;
	XtSetArg(wargs[n], XmNmarginWidth, 0); n++;
/* et */
	if ( do_et ) {
	sprintf(sst,"%6.1f",et[ind]);
	XtSetArg(wargs[n], XmNvalue,sst); n++;
	w = XtCreateManagedWidget("label",xmTextWidgetClass,rowcol,wargs,n);
	XtAddCallback(w, XmNactivateCallback, variab, 100*ind);
	}
/* eta */
	if ( do_eta ) {
	sprintf(sst,"%6.2f",eta[ind]);
	XtSetArg(wargs[n], XmNvalue,sst); n++;
	w = XtCreateManagedWidget("label",xmTextWidgetClass,rowcol,wargs,n);
	XtAddCallback(w, XmNactivateCallback, variab, 100*ind + 1);
	}
/* phi */
	if ( do_phi ) {
	if (radordeg == RADIANS) sprintf(sst,"%6.2f",phi[ind]);
	else sprintf(sst,"%6.1f",RADDEG*phi[ind]);
	XtSetArg(wargs[n], XmNvalue,sst); n++;
	w = XtCreateManagedWidget("label",xmTextWidgetClass,rowcol,wargs,n);
	XtAddCallback(w, XmNactivateCallback, variab, 100*ind + 2);
	}
/* emf */
	if ( do_emf ) {
	sprintf(sst,"%6.2f",emf[ind]);
	XtSetArg(wargs[n], XmNvalue,sst); n++;
	w = XtCreateManagedWidget("label",xmTextWidgetClass,rowcol,wargs,n);
	XtAddCallback(w, XmNactivateCallback, variab, 100*ind + 3);
	}
/* cone */
	if ( do_rjet ) {
	sprintf(sst,"%6.2f",rjet[ind]);
	XtSetArg(wargs[n], XmNvalue,sst); n++;
	w = XtCreateManagedWidget("label",xmTextWidgetClass,rowcol,wargs,n);
	XtAddCallback(w, XmNactivateCallback, variab, 100*ind + 4);
	}
/* mass */
	if ( do_mass ) {
	sprintf(sst,"%6.1f",mass[ind]);
	XtSetArg(wargs[n], XmNvalue,sst); n++;
	w = XtCreateManagedWidget("label",xmTextWidgetClass,rowcol,wargs,n);
	XtAddCallback(w, XmNactivateCallback, variab, 100*ind + 5);
	}
/* transverse mass */
	if ( do_mt ) {
	sprintf(sst,"%6.1f",mt[ind]);
	XtSetArg(wargs[n], XmNvalue,sst); n++;
	w = XtCreateManagedWidget("label",xmTextWidgetClass,rowcol,wargs,n);
	XtAddCallback(w, XmNactivateCallback, variab, 100*ind + 6);
	}
/* theta */
	if ( do_theta ) {
	if (radordeg == RADIANS) sprintf(sst,"%6.2f",theta[ind]);
	else sprintf(sst,"%6.1f",RADDEG*theta[ind]);
	XtSetArg(wargs[n], XmNvalue,sst); n++;
	w = XtCreateManagedWidget("label",xmTextWidgetClass,rowcol,wargs,n);
	XtAddCallback(w, XmNactivateCallback, variab, 100*ind + 7);
	}
/* e */
	if ( do_e ) {
	sprintf(sst,"%6.1f",e[ind]);
	XtSetArg(wargs[n], XmNvalue,sst); n++;
	w = XtCreateManagedWidget("label",xmTextWidgetClass,rowcol,wargs,n);
	XtAddCallback(w, XmNactivateCallback, variab, 100*ind + 8);
	}
/* px */
	if ( do_px ) {
	sprintf(sst,"%6.1f",px[ind]);
	XtSetArg(wargs[n], XmNvalue,sst); n++;
	w = XtCreateManagedWidget("label",xmTextWidgetClass,rowcol,wargs,n);
	XtAddCallback(w, XmNactivateCallback, variab, 100*ind + 9);
	}
/* py */
	if ( do_py ) {
	sprintf(sst,"%6.1f",py [ind]);
	XtSetArg(wargs[n], XmNvalue,sst); n++;
	w = XtCreateManagedWidget("label",xmTextWidgetClass,rowcol,wargs,n);
	XtAddCallback(w, XmNactivateCallback, variab, 100*ind + 10);
	}
/* pz */
	if ( do_pz ) {
	sprintf(sst,"%6.1f",pz[ind]);
	XtSetArg(wargs[n], XmNvalue,sst); n++;
	w = XtCreateManagedWidget("label",xmTextWidgetClass,rowcol,wargs,n);
	XtAddCallback(w, XmNactivateCallback, variab, 100*ind + 11);
	}
/* pt */
	if ( do_pt ) {
	sprintf(sst,"%6.1f",pt[ind]);
	XtSetArg(wargs[n], XmNvalue,sst); n++;
	w = XtCreateManagedWidget("label",xmTextWidgetClass,rowcol,wargs,n);
	XtAddCallback(w, XmNactivateCallback, variab, 100*ind + 12);
	}
/* scale */
	if ( do_sfac) {
	sprintf(sst,"%6.2f",sfac[ind]);
	XtSetArg(wargs[n], XmNvalue,sst); n++;
	w = XtCreateManagedWidget("label",xmTextWidgetClass,rowcol,wargs,n);
	XtAddCallback(w, XmNactivateCallback, variab, 100*ind + 13);
	}
		
}

MakeRCLables()
{
	int n;
	Arg wargs[20];
	Widget toggle;

/*
  create the labels for the columns
*/
	n = 2;
	XtSetArg(wargs[0], XmNfontList, fontlist);

/* blank */
	XtSetArg(wargs[1], XmNlabelString,XmStringCreateSimple(" ")); 
	XtCreateManagedWidget("label",xmLabelWidgetClass,rowcol, wargs, n);

/* blank */
	XtSetArg(wargs[1], XmNlabelString,XmStringCreateSimple(" ")); 
	XtCreateManagedWidget("label",xmLabelWidgetClass,rowcol, wargs, n);

/* et */
	if ( do_et ) {
	XtSetArg(wargs[1], XmNlabelString,XmStringCreateSimple("   Et "));
	XtCreateManagedWidget("label",xmLabelWidgetClass,rowcol, wargs, n);
	}
/* eta */
	if ( do_eta ) {
	XtSetArg(wargs[1], XmNlabelString,XmStringCreateSimple("   Eta"));
	XtCreateManagedWidget("label",xmLabelWidgetClass,rowcol, wargs, n);
	}
/* phi */
	if ( do_phi) {
	XtSetArg(wargs[1], XmNlabelString,XmStringCreateSimple("   Phi"));
	XtCreateManagedWidget("label",xmLabelWidgetClass,rowcol, wargs, n);
	}
/* emf */
	if ( do_emf) {
	XtSetArg(wargs[1], XmNlabelString,XmStringCreateSimple("   Emf"));
	XtCreateManagedWidget("label",xmLabelWidgetClass,rowcol, wargs, n);
	}
/* cone  */
	if ( do_rjet) {
	XtSetArg(wargs[1], XmNlabelString,XmStringCreateSimple("  Cone"));
	XtCreateManagedWidget("label",xmLabelWidgetClass,rowcol, wargs, n);
	}
/* mass */
	if ( do_mass) {
	XtSetArg(wargs[1], XmNlabelString,XmStringCreateSimple("  Mass"));
	XtCreateManagedWidget("label",xmLabelWidgetClass,rowcol, wargs, n);
	}
/* transverse mass */
	if ( do_mt) {
	XtSetArg(wargs[1], XmNlabelString,XmStringCreateSimple("    MT"));
	XtCreateManagedWidget("label",xmLabelWidgetClass,rowcol, wargs, n);
	}
/* theta */
	if ( do_theta) {
	XtSetArg(wargs[1], XmNlabelString,XmStringCreateSimple(" Theta"));
	XtCreateManagedWidget("label",xmLabelWidgetClass,rowcol, wargs, n);
	}
/* e */
	if ( do_e) {
	XtSetArg(wargs[1], XmNlabelString,XmStringCreateSimple("    E "));
	XtCreateManagedWidget("label",xmLabelWidgetClass,rowcol, wargs, n);
	}
/* px */
	if ( do_px) {
	XtSetArg(wargs[1], XmNlabelString,XmStringCreateSimple("   Px "));
	XtCreateManagedWidget("label",xmLabelWidgetClass,rowcol, wargs, n);
	}
/* py */
	if ( do_py) {
	XtSetArg(wargs[1], XmNlabelString,XmStringCreateSimple("   Py "));
	XtCreateManagedWidget("label",xmLabelWidgetClass,rowcol, wargs, n);
	}
/* pz */
	if ( do_pz) {
	XtSetArg(wargs[1], XmNlabelString,XmStringCreateSimple("   Pz "));
	XtCreateManagedWidget("label",xmLabelWidgetClass,rowcol, wargs, n);
	}
/* pt */
	if ( do_pt) {
	XtSetArg(wargs[1], XmNlabelString,XmStringCreateSimple("   Pt "));
	XtCreateManagedWidget("label",xmLabelWidgetClass,rowcol, wargs, n);
	}
/* scale */
	if ( do_sfac) {
	XtSetArg(wargs[1], XmNlabelString,XmStringCreateSimple(" Scale"));
	XtCreateManagedWidget("label",xmLabelWidgetClass,rowcol, wargs, n);
	}

}

GetMaxEt()
{
	int indmax,i;
	float maxet = 0.;
	
	for (i=0; i<nobj; i++) {
		if ( et[i] > maxet ) {
			maxet = et[i];
			indmax = i;
		}
	}
	return(indmax);	
}


DrawCircle(draw,the_gc,x,y,rads)    /* draws a circle in window draw, with
                                     	 context the_gc, with center at
                                         x,y and radius rads */
Widget draw;
GC the_gc;
int x,y,rads;
{
	
	int xc,yc;
	
	xc = x - rads;
	yc = y - rads;

	XDrawArc(XtDisplay(draw),XtWindow(draw),the_gc,xc,yc,2*rads,2*rads,
	  0, 23040);

}


/*  euler rotation - input are values x,y,z and cos/sin of theta,phi,psi
    output is via address to xe,ye,ze */
Euler(x,y,z,cosphi,sinphi,costheta,sintheta,cospsi,sinpsi,xe,ye,ze)
double *xe, *ye, *ze;
double x,y,z,costheta,sintheta,cosphi,sinphi,cospsi,sinpsi;
{
	*xe = x*(cospsi*cosphi-costheta*sinphi*sinpsi) +
	      y*(cospsi*sinphi+costheta*cosphi*sinpsi) +
	      z*(sintheta*sinpsi);
	*ye = -x*(sinpsi*cosphi+costheta*sinphi*cospsi) -
	       y*(sinpsi*sinphi-costheta*cosphi*cospsi) +
	       z*(sintheta*cospsi);
	*ze = x*(sintheta*sinphi) - y*(sintheta*cosphi) + z*costheta;
}

/*  euler rotation about PHI (1st rotation, see Goldstein pg 108/109 */
Euler1(x,y,z,cosphi,sinphi,xe,ye,ze)
double *xe, *ye, *ze;
double x,y,z,cosphi,sinphi;
{
	*xe = x*cosphi + y*sinphi;
	*ye = -x*sinphi + y*cosphi;
	*ze = z;
}

/*  euler rotation about THETA (2nd rotation, see Goldstein pg 108/109 */
Euler2(x,y,z,costheta,sintheta,xe,ye,ze)
double *xe, *ye, *ze;
double x,y,z,costheta,sintheta;
{
	*xe = x;
	*ye = y*costheta + z*sintheta;
	*ze = -y*sintheta + z*costheta;
}

/*  inverse euler rotation - input are values x,y,z and cos/sin of theta,phi,psi
    output is via address to xe,ye,ze */
invEuler(x,y,z,cosphi,sinphi,costheta,sintheta,cospsi,sinpsi,xe,ye,ze)
double *xe, *ye, *ze;
double x,y,z,costheta,sintheta,cosphi,sinphi,cospsi,sinpsi;
{
	*xe = x*(cospsi*cosphi-costheta*sinphi*sinpsi) -
	      y*(sinpsi*cosphi+costheta*sinphi*cospsi) +
	      z*(sintheta*sinphi);
	*ye = x*(cospsi*sinphi+costheta*cosphi*sinpsi) -
	      y*(sinpsi*sinphi-costheta*cosphi*cospsi) -
	      z*(sintheta*cosphi);
	*ze = x*(sintheta*sinpsi) + y*(sintheta*cospsi) + z*costheta;
}

DispRZ()          /* do "RZ" (or 3d really) display */
{

	double pmax, dp, ptry, fx1, fx2, fy1, fy2;
	double cp0, sp0, ct0, st0, cs0, ss0, xe, ye, ze, zoff;
	double dpx, dpy, dpz;
	float xnorm,ynorm;
	int i, length;
	Position xx,yy,x2,y2;
	char substr[100];

	XClearWindow(DisplayRZ,WindowRZ);
/*
  renormalize:  put 0,0 at height/2 and width/2 with positive y
  				direction downward (widgets always start at 0,0
  				is upper left hand corner and positive y downward)
  				reduce by 10% for the id

  get max p(rz) of all objects
*/
	pmax = 0.;
	for (i=0; i<nobj; i++) {
		dp = sqrt( pt[i]*pt[i] + pz[i]*pz[i] );
		ptry = dp;
		if ( ptry > pmax ) pmax = ptry;
	}
	xnorm = .4*heightRZ/pmax;
/*
  use cosines and sines to make things faster
*/
	cp0 = cos(phi0); sp0 = sin(phi0);
	ct0 = cos(theta0); st0 = sin(theta0);
	cs0 = cos(psi0);  ss0 = sin(psi0);
/*
  set lines to be solid
*/
	XSetLineAttributes(DisplayRZ,gcrz,0,LineSolid,CapButt,JoinMiter);
	for (i=0; i<nobj; i++) {
/*
  		do Euler rotation 
*/
		Euler(px[i],py[i],pz[i],cp0,sp0,ct0,st0,cs0,ss0,&xe,&ye,&ze);
		pxn[i] = xe; pyn[i] = ye; pzn[i] = ze;
/*
        project onto display xy plane
*/
		fx2 = x1RZ + xnorm*pzn[i];
		fy2 = y1RZ - xnorm*pxn[i];
		x2 = fx2;
		y2 = fy2;
		XDrawLine(DisplayRZ,WindowRZ,gcrz,x1RZ,y1RZ,x2,y2);
/*
        draw label (for muons, extend to max with dashed line first)
*/
		if ( otype[i] == 2 ) {
			dp = sqrt( pxn[i]*pxn[i] + pyn[i]*pyn[i] + pzn[i]*pzn[i] );
			fx2 = x1RZ + 2.*xnorm*pmax*pzn[i]/dp;
			fy2 = y1RZ - 2.*xnorm*pmax*pxn[i]/dp;
			x2 = fx2;
			y2 = fy2;
			XSetLineAttributes(DisplayRZ,gcrz,0,LineOnOffDash,
						CapButt,JoinMiter);
			XDrawLine(DisplayRZ,WindowRZ,gcrz,x1RZ,y1RZ,x2,y2);
			XSetLineAttributes(DisplayRZ,gcrz,0,LineSolid,
						CapButt,JoinMiter);
			sprintf(substr,"%d",i+1);
			length = strlen(substr);
			fx2 = x1RZ + 2.05*xnorm*pmax*pzn[i]/dp;
			fy2 = y1RZ - 2.05*xnorm*pmax*pxn[i]/dp;
			x2 = fx2;
			y2 = fy2;
			XDrawString(DisplayRZ,WindowRZ,gcrz,x2,y2,substr,length);
		}
		else {
			sprintf(substr,"%d",i+1);
			length = strlen(substr);
			fx2 = x1RZ + 1.05*xnorm*pzn[i];
			fy2 = y1RZ - 1.05*xnorm*pxn[i];
			x2 = fx2;
			y2 = fy2;
			XDrawString(DisplayRZ,WindowRZ,gcrz,x2,y2,substr,length);
		}
	}


/*
  	draw x and y and z axes (dashed lines)

	XSetLineAttributes(DisplayRZ,gcrz,0,LineOnOffDash,CapButt,JoinMiter);
	dp = xnorm*pmax;
/*
  x axis

	Euler(xRZx,xRZy,xRZz,cp0,sp0,ct0,st0,cs0,ss0,&xe,&ye,&ze);
	fx2 = x1RZ + dp*ze; fy2 = y1RZ - dp*xe;
	x2 = fx2; y2 = fy2;
	XDrawLine(DisplayRZ,WindowRZ,gcrz,x1RZ,y1RZ,x2,y2);
	fx2 = x1RZ + 1.05*dp*ze; fy2 = y1RZ - 1.05*dp*xe; 
	x2 = fx2; y2 = fy2;
	strcpy(substr,"x");
	XDrawString(DisplayRZ,WindowRZ,gcrz,x2,y2,substr,1);
/*
  y axis

	Euler(yRZx,yRZy,yRZz,cp0,sp0,ct0,st0,cs0,ss0,&xe,&ye,&ze);
	fx2 = x1RZ + dp*ze; fy2 = y1RZ - dp*xe;
	x2 = fx2; y2 = fy2;
  	XDrawLine(DisplayRZ,WindowRZ,gcrz,x1RZ,y1RZ,x2,y2);
	fx2 = x1RZ + 1.05*dp*ze; fy2 = y1RZ - 1.05*dp*xe; 
	x2 = fx2; y2 = fy2;
	strcpy(substr,"y");
	XDrawString(DisplayRZ,WindowRZ,gcrz,x2,y2,substr,1);
/*
  z axis

	Euler(zRZx,zRZy,zRZz,cp0,sp0,ct0,st0,cs0,ss0,&xe,&ye,&ze);
	fx2 = x1RZ + dp*ze; fy2 = y1RZ - dp*xe;
	x2 = fx2; y2 = fy2;
	XDrawLine(DisplayRZ,WindowRZ,gcrz,x1RZ,y1RZ,x2,y2);
	fx2 = x1RZ + 1.05*dp*ze; fy2 = y1RZ - 1.05*dp*xe; 
	x2 = fx2; y2 = fy2;
	strcpy(substr,"z");
	XDrawString(DisplayRZ,WindowRZ,gcrz,x2,y2,substr,1);
*/
/*
  draw the cc - use +-CCZEND cm in z and CCRAD cm radius
*/
    xnorm = (widthRZ/4.)/CCZEND; ynorm = (heightRZ/4.)/CCRAD;
    if ( nvert < 1 ) zoff = 0.;
    else zoff = -zvert[0];
    for ( i=0; i<36; i++ ) {
  		dp = 10*i*DEGRAD;
  		dpx = CCRAD*ynorm*cos(dp);
  		dpy = CCRAD*ynorm*sin(dp);
  		dpz = (CCZEND+zoff)*xnorm;
  		Euler(dpx,dpy,dpz,cp0,sp0,ct0,st0,cs0,ss0,&xe,&ye,&ze);
  		xx = x1RZ + ze;
  		yy = y1RZ - xe;
  		XDrawPoint(DisplayRZ,WindowRZ,gcrz,xx,yy);
    }
    for ( i=0; i<36; i++ ) {
  		dp = (10*i+5)*DEGRAD;
  		dpx = CCRAD*ynorm*cos(dp);
  		dpy = CCRAD*ynorm*sin(dp);
  		dpz = (-CCZEND+zoff)*xnorm;
  		Euler(dpx,dpy,dpz,cp0,sp0,ct0,st0,cs0,ss0,&xe,&ye,&ze);
  		xx = x1RZ + ze;
  		yy = y1RZ - xe;
  		XDrawPoint(DisplayRZ,WindowRZ,gcrz,xx,yy);
    }
/*
  put a * at the primary vertices
*/
	if ( nvert < 1 ) {
		dpx = 0; dpy = 0; dpz = 0.;
  		Euler(dpx,dpy,dpz,cp0,sp0,ct0,st0,cs0,ss0,&xe,&ye,&ze);
  		xx = x1RZ + ze;
  		yy = y1RZ - xe;
  		XDrawString(DisplayRZ,WindowRZ,gcrz,xx,yy,"*",1);
	}
	else {
		for ( i=0; i<nvert; i++ ) {
			dpx = 0; dpy = 0; dpz = -zvert[i];
			Euler(dpx,dpy,dpz,cp0,sp0,ct0,st0,cs0,ss0,&xe,&ye,&ze);
			xx = x1RZ + ze;
			yy = y1RZ - xe;
			XDrawString(DisplayRZ,WindowRZ,gcrz,xx,yy,"*",1);
		}
	}
/*
  connect the two ends together at 0, 90, 180, and 270 degrees by
  dashed lines 
*/
	XSetLineAttributes(DisplayRZ,gcrz,0,LineOnOffDash,CapButt,JoinMiter);
/*                            0 degrees */
	dpz = (-CCZEND+zoff)*xnorm;
	dpy = 0.;
	dpx = CCRAD*ynorm;
	Euler(dpx,dpy,dpz,cp0,sp0,ct0,st0,cs0,ss0,&xe,&ye,&ze);
	xx = x1RZ + ze;
	yy = y1RZ - xe;
	dpz = (CCZEND+zoff)*xnorm;
	dpy = 0.;
	dpx = CCRAD*ynorm;
	Euler(dpx,dpy,dpz,cp0,sp0,ct0,st0,cs0,ss0,&xe,&ye,&ze);
	x2 = x1RZ + ze;
	y2 = y1RZ - xe;
	XDrawLine(DisplayRZ,WindowRZ,gcrz,xx,yy,x2,y2);
/*                            90 degrees */
	dpz = (-CCZEND+zoff)*xnorm;
	dpx = 0.;
	dpy = CCRAD*ynorm;
	Euler(dpx,dpy,dpz,cp0,sp0,ct0,st0,cs0,ss0,&xe,&ye,&ze);
	xx = x1RZ + ze;
	yy = y1RZ - xe;
	dpz = (CCZEND+zoff)*xnorm;
	dpx = 0.;
	dpy = CCRAD*ynorm;
	Euler(dpx,dpy,dpz,cp0,sp0,ct0,st0,cs0,ss0,&xe,&ye,&ze);
	x2 = x1RZ + ze;
	y2 = y1RZ - xe;
	XDrawLine(DisplayRZ,WindowRZ,gcrz,xx,yy,x2,y2);
/*                            180 degrees */
	dpz = (-CCZEND+zoff)*xnorm;
	dpy = 0.;
	dpx = -CCRAD*ynorm;
	Euler(dpx,dpy,dpz,cp0,sp0,ct0,st0,cs0,ss0,&xe,&ye,&ze);
	xx = x1RZ + ze;
	yy = y1RZ - xe;
	dpz = (CCZEND+zoff)*xnorm;
	dpy = 0.;
	dpx = -CCRAD*ynorm;
	Euler(dpx,dpy,dpz,cp0,sp0,ct0,st0,cs0,ss0,&xe,&ye,&ze);
	x2 = x1RZ + ze;
	y2 = y1RZ - xe;
	XDrawLine(DisplayRZ,WindowRZ,gcrz,xx,yy,x2,y2);
/*                            270 degrees */
	dpz = (-CCZEND+zoff)*xnorm;
	dpx = 0.;
	dpy = -CCRAD*ynorm;
	Euler(dpx,dpy,dpz,cp0,sp0,ct0,st0,cs0,ss0,&xe,&ye,&ze);
	xx = x1RZ + ze;
	yy = y1RZ - xe;
	dpz = (CCZEND+zoff)*xnorm;
	dpx = 0.;
	dpy = -CCRAD*ynorm;
	Euler(dpx,dpy,dpz,cp0,sp0,ct0,st0,cs0,ss0,&xe,&ye,&ze);
	x2 = x1RZ + ze;
	y2 = y1RZ - xe;
	XDrawLine(DisplayRZ,WindowRZ,gcrz,xx,yy,x2,y2);
/*
  draw boxes for the muon system
*/
/*
  type a few things out
*/
	strcpy(substr,"-- Inner CC Edge");
	length = strlen(substr);
	XDrawString(DisplayRZ,WindowRZ,gcrz,10,10,substr,length);
	strcpy(substr," *  z=0");
	length = strlen(substr);
	XDrawString(DisplayRZ,WindowRZ,gcrz,10,22,substr,length);
/*
  reset line attributes
*/
	XSetLineAttributes(DisplayRZ,gcrz,0,LineSolid,CapButt,JoinMiter);
/*
  reset axes
*/
	xRZx = 1.; xRZy = 0.; xRZz = 0.;
	yRZx = 0.; yRZy = 1.; yRZz = 0.;
	zRZx = 0.; zRZy = 0.; zRZz = 1.;
}

ResetPN()
{
	int i;
	
	for ( i=0; i<nobj; i++ ) {
		pxn[i] = px[i];
		pyn[i] = py[i];
		pzn[i] = pz[i];
	}

	xRZx = 1.; xRZy = 0.; xRZz = 0.;
	yRZx = 0.; yRZy = 1.; yRZz = 0.;
	zRZx = 0.; zRZy = 0.; zRZz = 1.;
	
}


cp_to_n(ind,jnd)        /* copy from jnd to ind */
int ind, jnd;
{
			partidn[ind] = partid[jnd];
			baddrn[ind] = baddr[jnd];
			etn[ind] = et[jnd];
			etan[ind] = eta[jnd];
			phin[ind] = phi[jnd];
			massn[ind] = mass[jnd];
			en[ind] = e[jnd];
			pxn[ind] = px[jnd];
			pyn[ind] = py[jnd];
			pzn[ind] = pz[jnd];
			thetan[ind] = theta[jnd];
			otypen[ind] = otype[jnd];
			oseln[ind] = osel[jnd];
			emfn[ind] = emf[jnd];
			rjetn[ind] = rjet[jnd];
			mtn[ind] = mt[jnd];
			ptn[ind] = pt[jnd];
			sfacn[ind] = sfac[jnd];
}

cp_from_n(ind,jnd)        /* copy from jnd to ind */
int ind, jnd;
{
			partid[ind] = partidn[jnd];
			baddr[ind] = baddrn[jnd];
			et[ind] = etn[jnd];
			eta[ind] = etan[jnd];
			phi[ind] = phin[jnd];
			mass[ind] = massn[jnd];
			e[ind] = en[jnd];
			px[ind] = pxn[jnd];
			py[ind] = pyn[jnd];
			pz[ind] = pzn[jnd];
			theta[ind] = thetan[jnd];
			otype[ind] = otypen[jnd];
			osel[ind] = oseln[jnd];
			emf[ind] = emfn[jnd];
			rjet[ind] = rjetn[jnd];
			mt[ind] = mtn[jnd];
			pt[ind] = ptn[jnd];
			sfac[ind] = sfacn[jnd];
}

phirot(w, more, cbs)
Widget w;
int *more;
XmArrowButtonCallbackStruct *cbs;
{
	void change_phi();
	int incr = *more;	

	if ( cbs->reason == XmCR_ARM) {
		change_phi(incr);
	}
	else if (cbs->reason == XmCR_DISARM) {
		XtRemoveTimeOut(timer_id);
	}
}

void
change_phi(incr)
int incr;
{
	char string[20];
	double d;
	int i, dummy;
	
	phi0 += incr*DEGRAD;
	if ( phi0 > 2.*PI ) phi0 = 2.*PI - phi0;
	if ( phi0 < -2.*PI ) phi0 = phi0 + 2.*PI;
	if ( phi0 < 0. ) {
		d = phi0*RADDEG - .5;  i = d;
	}
	else {
		d = phi0*RADDEG + .5;  i = d;
	}
	sprintf(string,"Z Axis (by %d)",i);
	SetLabel(phi0lab,string);
	dummy = 9;
	cphys(dummy,&dummy,dummy);
	DispRZ();
/*
  	call myself after timeout ms
*/
	timer_id = XtAppAddTimeOut(appcontext, 
		timeout, change_phi, incr);
}

thetarot(w, more, cbs)
Widget w;
int *more;
XmArrowButtonCallbackStruct *cbs;
{
	void change_theta();
	int incr = *more;	

	if ( cbs->reason == XmCR_ARM) {
		change_theta(incr);
	}
	else if (cbs->reason == XmCR_DISARM) {
		XtRemoveTimeOut(timer_id);
	}
}

void
change_theta(incr)
int incr;
{
	char string[20];
	double d;
	int i, dummy;
	
	theta0 += incr*DEGRAD;
	if ( theta0 > 2.*PI ) theta0 = 2.*PI - theta0;
	if ( theta0 < -2.*PI ) theta0 = theta0 + 2.*PI;
	if ( theta0 < 0. ) {
		d = theta0*RADDEG - .5;  i = d;
	}
	else {
		d = theta0*RADDEG + .5;  i = d;
	}
	sprintf(string,"Z Axis (by %d)",i);
	SetLabel(theta0lab,string);
	DispRZ();
/*
  	call myself after timeout ms
*/
	timer_id = XtAppAddTimeOut(appcontext, 
		timeout, change_theta, incr);
}

psirot(w, more, cbs)
Widget w;
int *more;
XmArrowButtonCallbackStruct *cbs;
{
	void change_psi();
	int incr = *more;	

	if ( cbs->reason == XmCR_ARM) {
		change_psi(incr);
	}
	else if (cbs->reason == XmCR_DISARM) {
		XtRemoveTimeOut(timer_id);
	}
}

void
change_psi(incr)
int incr;
{
	char string[20];
	double d;
	int i, dummy;
	
	psi0 += incr*DEGRAD;
	if ( psi0 > 2.*PI ) psi0 = 2.*PI - psi0;
	if ( psi0 < -2.*PI ) psi0 = psi0 + 2.*PI;
	if ( psi0 < 0. ) {
		d = psi0*RADDEG - .5;  i = d;
	}
	else {
		d = psi0*RADDEG + .5;  i = d;
	}
	sprintf(string,"Z Axis (by %d)",i);
	SetLabel(psi0lab,string);
	DispRZ();
/*
  	call myself after timeout ms
*/
	timer_id = XtAppAddTimeOut(appcontext, 
		timeout, change_psi, incr);
}

