/* 
        d0tracks.c
         Created           : 18-OCT-1994 by Drew Baden
   fills in texts (in Physics/Tracks window) for tracking banks

*/
 
#include <stdio.h>                   /* I/O definitions                       */
#include "/d0library/scratch/test/xframe/source/d0x_c.h"
#define MXTRKS 100 
#define CD 0
#define VTX 1
#define FDC 2
int which_tracks = CD;

void d0tracks(w,tag,reason)
Widget        w;
int        *tag;
unsigned long    *reason;
{
    int select = *tag;
    int cdbvers,cdvers,ncdtracks,nseg0,nseg1,nseg2,nseg3,ncdsta;
    int ncdhitscon,ncdrawlength,cdfull;
    int maxtracks = MXTRKS,idum;
    int *nvers,*tau,*e,*mu,*vee,*vid,*edge,*nztrks,*seg0,*seg1;
    int *seg2,*seg3,*nwires,*jet,*status,*xybit,*rzbit,*nz,*ndof,*nhits;
    int *fdcbits,*nptsfit,*bankno,*vtxfit;
    float *phi,*x0,*y0,*theta,*r0,*z0,*chixy2,*dxdz,*dydz,*chisq;
    float *chirz2,*errphi,*errxy,*errtheta,*errz0,*ion,*errion,*thetaz0cov;
    float *thetafit,*errthetafit,*phifit,*errphifit,*chisqfit;
    /* vtx variables */
    int vtxvers,nvtxtracks,ptcorr,hvcorr,nhitsvtx,nsta,cdd1size;
    int *nzhits;
    float *xg,*yg,*vzgtheta,*zg,*dzdr,*zvtx,*errrz,*sintheta;
    /* fd variables */
    int ftvers,nfdtracks,nfd0,nfd1,nfdwires,cdd3size,alignlevel,fdfull;
    float fdz0,fdz1;

    int okok,i;
    char *string,temp[400],*row0;
    char *row1,*row2,*row3,*row4,*row5,*row6;
    char *row7,*row8,*row9,*row10,*row11,*row12;
    char *row13,*row14,*row15,*row16,*row17;
    char *row18,*row19,*row20,*row21,*row22;
    char *row23,*row24,*row25,*row26,*row27;
    char *row28,*row29,*row30,*row31,*row32;
    
    if (select != 10) {
        which_tracks = select;
        return;
    }
    SetWatchCursor(track_bull);
    switch (which_tracks) {
        case CD:    /* get cd info */
#ifdef D0FLAVOR
            numcdtks_(&okok,&ncdtracks);
#else
            numcdtks(&okok,&ncdtracks);
#endif
            if (ncdtracks>0) idum = ncdtracks;
            else idum = 1;
            bankno = (int *) XtMalloc(sizeof(int)*idum);    
            nvers = (int *) XtMalloc(sizeof(int)*idum);    
            tau = (int *) XtMalloc(sizeof(int)*idum);    
            e = (int *) XtMalloc(sizeof(int)*idum);    
            mu = (int *) XtMalloc(sizeof(int)*idum);    
            vee = (int *) XtMalloc(sizeof(int)*idum);    
            vid = (int *) XtMalloc(sizeof(int)*idum);    
            edge = (int *) XtMalloc(sizeof(int)*idum);    
            nztrks = (int *) XtMalloc(sizeof(int)*idum);    
            seg0 = (int *) XtMalloc(sizeof(int)*idum);    
            seg1 = (int *) XtMalloc(sizeof(int)*idum);    
            seg2 = (int *) XtMalloc(sizeof(int)*idum);    
            seg3 = (int *) XtMalloc(sizeof(int)*idum);    
            nwires = (int *) XtMalloc(sizeof(int)*idum);    
            xybit = (int *) XtMalloc(sizeof(int)*idum);    
            rzbit = (int *) XtMalloc(sizeof(int)*idum);    
            nz = (int *) XtMalloc(sizeof(int)*idum);    
            ndof = (int *) XtMalloc(sizeof(int)*idum);    
            phi = (float *) XtMalloc(sizeof(float)*idum);    
            x0 = (float *) XtMalloc(sizeof(float)*idum);    
            y0 = (float *) XtMalloc(sizeof(float)*idum);    
            theta = (float *) XtMalloc(sizeof(float)*idum);    
            r0 = (float *) XtMalloc(sizeof(float)*idum);    
            z0 = (float *) XtMalloc(sizeof(float)*idum);    
            chixy2 = (float *) XtMalloc(sizeof(float)*idum);    
            chirz2 = (float *) XtMalloc(sizeof(float)*idum);    
            errphi = (float *) XtMalloc(sizeof(float)*idum);    
            errxy = (float *) XtMalloc(sizeof(float)*idum);    
            errtheta = (float *) XtMalloc(sizeof(float)*idum);    
            errz0 = (float *) XtMalloc(sizeof(float)*idum);    
            ion = (float *) XtMalloc(sizeof(float)*idum);    
            errion = (float *) XtMalloc(sizeof(float)*idum);    
            thetaz0cov = (float *) XtMalloc(sizeof(float)*idum);    
#ifdef D0FLAVOR
            cdstuff_(&okok,&maxtracks,
             &cdbvers,&cdvers,&ncdtracks,&nseg0,&nseg1,&nseg2,&nseg3,&ncdsta,
            &ncdhitscon,&ncdrawlength,&cdfull,bankno,
            nvers,tau,e,mu,vee,vid,edge,nztrks,seg0,seg1,seg2,
            seg3,nwires,xybit,rzbit,nz,ndof,phi,x0,y0,theta,r0,z0,
            chixy2,chirz2,errphi,errxy,errtheta,errz0,ion,errion,thetaz0cov);
#else
            cdstuff(&okok,&maxtracks,
            &cdbvers,&cdvers,&ncdtracks,&nseg0,&nseg1,&nseg2,&nseg3,&ncdsta,
            &ncdhitscon,&ncdrawlength,&cdfull,bankno,
            nvers,tau,e,mu,vee,vid,edge,nztrks,seg0,seg1,seg2,
            seg3,nwires,xybit,rzbit,nz,ndof,phi,x0,y0,theta,r0,z0,
            chixy2,chirz2,errphi,errxy,errtheta,errz0,ion,errion,thetaz0cov);
#endif
            if (!okok) {
                strcpy(string,"DTRH bank not found");
                XmTextSetString(trktext,string);
                SetDefaultCursor(track_bull);
                return;
            }
            /* size = #columns*#rows+about 240 */
            string = (char *) XtMalloc(sizeof(char)*(ncdtracks+1)*8*33 + 300);
            if (cdfull) sprintf(string,
"VERSION %2d  BNK VER %2d  FULL TRKING YES  No. Tracks Kept %3d\n\
No. Seg: Layer 0 %3d  Layer 1 %3d  Layer 2 %3d  Layer 3 %3d\n\
No. CDC built %3d  Hits constructed %5d  Length CD rawdat %5d\n",
                cdbvers,cdvers,ncdtracks,nseg0,nseg1,nseg2,nseg3,ncdsta,
                ncdhitscon,ncdrawlength);
            else sprintf(string,
"VERSION %2d  BNK VER %2d  FULL TRKING NO  No. Tracks Kept %3d\n\
No. Seg: Layer 0 %3d  Layer 1 %3d  Layer 2 %3d  Layer 3 %3d\n\
No. CDC built %3d  Hits constructed %5d  Length CD rawdat %5d\n",
                cdbvers,cdvers,ncdtracks,nseg0,nseg1,nseg2,nseg3,ncdsta,
                ncdhitscon,ncdrawlength);
            /* now do every track */
            if (ncdtracks>0) {
                idum = sizeof(char)*ncdtracks*8;
                row0 = (char *) XtMalloc(idum); row0[0]=0;
                row1 = (char *) XtMalloc(idum); row1[0]=0;
                row2 = (char *) XtMalloc(idum); row2[0]=0;
                row3 = (char *) XtMalloc(idum); row3[0]=0;
                row4 = (char *) XtMalloc(idum); row4[0]=0;
                row5 = (char *) XtMalloc(idum); row5[0]=0;
                row6 = (char *) XtMalloc(idum); row6[0]=0;
                row7 = (char *) XtMalloc(idum); row7[0]=0;
                row8 = (char *) XtMalloc(idum); row8[0]=0;
                row9 = (char *) XtMalloc(idum); row9[0]=0;
                row10 = (char *) XtMalloc(idum); row10[0]=0;
                row11 = (char *) XtMalloc(idum); row11[0]=0;
                row12 = (char *) XtMalloc(idum); row12[0]=0;
                row13 = (char *) XtMalloc(idum); row13[0]=0;
                row14 = (char *) XtMalloc(idum); row14[0]=0;
                row15 = (char *) XtMalloc(idum); row15[0]=0;
                row16 = (char *) XtMalloc(idum); row16[0]=0;
                row17 = (char *) XtMalloc(idum); row17[0]=0;
                row18 = (char *) XtMalloc(idum); row18[0]=0;
                row19 = (char *) XtMalloc(idum); row19[0]=0;
                row20 = (char *) XtMalloc(idum); row20[0]=0;
                row21 = (char *) XtMalloc(idum); row21[0]=0;
                row22 = (char *) XtMalloc(idum); row22[0]=0;
                row23 = (char *) XtMalloc(idum); row23[0]=0;
                row24 = (char *) XtMalloc(idum); row24[0]=0;
                row25 = (char *) XtMalloc(idum); row25[0]=0;
                row26 = (char *) XtMalloc(idum); row26[0]=0;
                row27 = (char *) XtMalloc(idum); row27[0]=0;
                row28 = (char *) XtMalloc(idum); row28[0]=0;
                row29 = (char *) XtMalloc(idum); row29[0]=0;
                row30 = (char *) XtMalloc(idum); row30[0]=0;
                row31 = (char *) XtMalloc(idum); row31[0]=0;
                row32 = (char *) XtMalloc(idum); row32[0]=0;
                sprintf(temp,"\n\n\n\nBANK   \nVERSION\nTAU   ?\nE     ?\n\
MU    ?\nVEE   ?\nVID    \nEDGE  ?\nZTRKS  \nSeg/L0 \nSeg/L1 \n\
Seg/L2 \nSeg/L3 \nWires  \nXYbits \nRZbits \nNum Zs \nPhi    \n\
X0     \nY0     \nTheta  \nR0     \nZ0     \nChi2xy \nChi2rz \n\
NumDOF \nErrPHI \nErrXY  \nErrTHE \nErrZ0  \ndEdX   \n error \neZ0,Th ");
                XmTextSetString(trackvariabs,temp);
                strcat(string,"       ");
                for (i=0; i<ncdtracks; i++) {
                    sprintf(temp,"     %3d",bankno[i]); 
                    strcat(row0,temp);
                    sprintf(temp,"      %2d",nvers[i]); 
                    strcat(row1,temp);
                    sprintf(temp,"       %1d",tau[i]); strcat(row2,temp);
                    sprintf(temp,"       %1d",e[i]); strcat(row3,temp);
                    sprintf(temp,"       %1d",mu[i]); strcat(row4,temp);
                    sprintf(temp,"       %1d",vee[i]); strcat(row5,temp);
                    sprintf(temp,"      %2d",vid[i]); strcat(row6,temp);
                    sprintf(temp,"       %1d",edge[i]); strcat(row7,temp);
                    sprintf(temp,"     %3d",nztrks[i]); strcat(row8,temp);
                    sprintf(temp,"     %3d",seg0[i]); strcat(row9,temp);
                    sprintf(temp,"     %3d",seg1[i]); strcat(row10,temp);
                    sprintf(temp,"     %3d",seg2[i]); strcat(row11,temp);
                    sprintf(temp,"     %3d",seg3[i]); strcat(row12,temp);
                    sprintf(temp,"     %3d",nwires[i]); strcat(row13,temp);
                    sprintf(temp,"%8X",xybit[i]); strcat(row14,temp);
                    sprintf(temp,"%8X",rzbit[i]); strcat(row15,temp);
                    sprintf(temp,"     %3d",nz[i]); strcat(row16,temp);
                    sprintf(temp,"   %5.2f",phi[i]); strcat(row17,temp);
                    sprintf(temp,"  %6.1f",x0[i]); strcat(row18,temp);
                    sprintf(temp,"  %6.1f",y0[i]); strcat(row19,temp);
                    sprintf(temp,"   %5.2f",theta[i]); strcat(row20,temp);
                    sprintf(temp,"  %6.1f",r0[i]); strcat(row21,temp);
                    sprintf(temp,"  %6.1f",z0[i]); strcat(row22,temp);
                    sprintf(temp,"  %6.1f",chixy2[i]); strcat(row23,temp);
                    sprintf(temp,"  %6.1f",chirz2[i]); strcat(row24,temp);
                    sprintf(temp,"     %3d",ndof[i]); strcat(row25,temp);
                    sprintf(temp," %7.4f",errphi[i]); strcat(row26,temp);
                    sprintf(temp," %7.4f",errxy[i]); strcat(row27,temp);
                    sprintf(temp," %7.4f",errtheta[i]); strcat(row28,temp);
                    sprintf(temp," %7.4f",errz0[i]); strcat(row29,temp);
                    sprintf(temp," %7.2f",ion[i]); strcat(row30,temp);
                    sprintf(temp," %7.3f",errion[i]); strcat(row31,temp);
                    sprintf(temp," %7.4f",thetaz0cov[i]); strcat(row32,temp);
                }
                strcat(string,"\n");
                strcat(string,row0); strcat(string,"\n");
                strcat(string,row1); strcat(string,"\n");
                strcat(string,row2); strcat(string,"\n");
                strcat(string,row3); strcat(string,"\n");
                strcat(string,row4); strcat(string,"\n");
                strcat(string,row5); strcat(string,"\n");
                strcat(string,row6); strcat(string,"\n");
                strcat(string,row7); strcat(string,"\n");
                strcat(string,row8); strcat(string,"\n");
                strcat(string,row9); strcat(string,"\n");
                strcat(string,row10); strcat(string,"\n");
                strcat(string,row11); strcat(string,"\n");
                strcat(string,row12); strcat(string,"\n");
                strcat(string,row13); strcat(string,"\n");
                strcat(string,row14); strcat(string,"\n");
                strcat(string,row15); strcat(string,"\n");
                strcat(string,row16); strcat(string,"\n");
                strcat(string,row17); strcat(string,"\n");
                strcat(string,row18); strcat(string,"\n");
                strcat(string,row19); strcat(string,"\n");
                strcat(string,row20); strcat(string,"\n");
                strcat(string,row21); strcat(string,"\n");
                strcat(string,row22); strcat(string,"\n");
                strcat(string,row23); strcat(string,"\n");
                strcat(string,row24); strcat(string,"\n");
                strcat(string,row25); strcat(string,"\n");
                strcat(string,row26); strcat(string,"\n");
                strcat(string,row27); strcat(string,"\n");
                strcat(string,row28); strcat(string,"\n");
                strcat(string,row29); strcat(string,"\n");
                strcat(string,row30); strcat(string,"\n");
                strcat(string,row31); strcat(string,"\n");
                strcat(string,row32); strcat(string,"\n");
            }
            XmTextSetString(trktext,string);
            XtFree(string);
            if (ncdtracks>0) {
                XtFree(row0); XtFree(row1); XtFree(row2); XtFree(row3);
                XtFree(row4); XtFree(row5); XtFree(row6); XtFree(row7);
                XtFree(row8); XtFree(row9); XtFree(row10); XtFree(row11);
                XtFree(row12); XtFree(row13); XtFree(row14); XtFree(row15);
                XtFree(row16); XtFree(row17); XtFree(row18); XtFree(row19);
                XtFree(row20); XtFree(row21); XtFree(row22); XtFree(row23);
                XtFree(row24); XtFree(row25); XtFree(row26); XtFree(row27);
                XtFree(row28); XtFree(row29); XtFree(row30); XtFree(row31);
                XtFree(row32); 
            }
            XtFree((char*)bankno); XtFree((char*)nvers); XtFree((char*)tau);
            XtFree((char*)e); XtFree((char*)mu); XtFree((char*)vee); 
            XtFree((char*)vid); XtFree((char*)edge);
            XtFree((char*)nztrks); XtFree((char*)seg0); XtFree((char*)seg1); 
            XtFree((char*)seg2); 
            XtFree((char*)seg3); XtFree((char*)nwires); XtFree((char*)xybit); 
            XtFree((char*)rzbit);
            XtFree((char*)nz); XtFree((char*)ndof); XtFree((char*)phi); 
            XtFree((char*)x0); XtFree((char*)y0);
            XtFree((char*)theta); XtFree((char*)r0); XtFree((char*)z0); 
            XtFree((char*)chixy2); 
            XtFree((char*)chirz2); XtFree((char*)errphi); XtFree((char*)errxy); 
            XtFree((char*)errtheta);
            XtFree((char*)errz0); XtFree((char*)ion); XtFree((char*)errion); 
            XtFree((char*)thetaz0cov);
        break;
    case VTX:   /* get vtx info */
#ifdef D0FLAVOR
            numvtxtks_(&okok,&nvtxtracks);
#else
            numvtxtks(&okok,&nvtxtracks);
#endif
            if (nvtxtracks>0) idum = nvtxtracks;
            else idum = 1;
            bankno = (int *) XtMalloc(sizeof(int)*idum);    
            vee = (int *) XtMalloc(sizeof(int)*idum);    
            mu = (int *) XtMalloc(sizeof(int)*idum);    
            e = (int *) XtMalloc(sizeof(int)*idum);    
            tau = (int *) XtMalloc(sizeof(int)*idum);    
            status = (int *) XtMalloc(sizeof(int)*idum);    
            nwires = (int *) XtMalloc(sizeof(int)*idum);    
            xybit = (int *) XtMalloc(sizeof(int)*idum);    
            rzbit = (int *) XtMalloc(sizeof(int)*idum);    
            nzhits = (int *) XtMalloc(sizeof(int)*idum);    
            phi = (float *) XtMalloc(sizeof(float)*idum);    
            xg = (float *) XtMalloc(sizeof(float)*idum);    
            yg = (float *) XtMalloc(sizeof(float)*idum);    
            theta = (float *) XtMalloc(sizeof(float)*idum);    
            vzgtheta  = (float *) XtMalloc(sizeof(float)*idum);    
            zg = (float *) XtMalloc(sizeof(float)*idum);    
            chixy2 = (float *) XtMalloc(sizeof(float)*idum);    
            chirz2 = (float *) XtMalloc(sizeof(float)*idum);    
            dzdr = (float *) XtMalloc(sizeof(float)*idum);    
            zvtx = (float *) XtMalloc(sizeof(float)*idum);    
            errphi = (float *) XtMalloc(sizeof(float)*idum);    
            errxy = (float *) XtMalloc(sizeof(float)*idum);    
            errtheta = (float *) XtMalloc(sizeof(float)*idum);    
            errrz = (float *) XtMalloc(sizeof(float)*idum);    
            ion = (float *) XtMalloc(sizeof(float)*idum);    
            sintheta = (float *) XtMalloc(sizeof(float)*idum);    
#ifdef D0FLAVOR
            vtxstuff_(&okok,&maxtracks,&vtxvers,&nvtxtracks,
                &ptcorr,&hvcorr,&nhitsvtx,&nsta,&cdd1size,bankno,
                vee,mu,e,tau,status,nwires,xybit,rzbit,nzhits,phi,xg,yg,
                theta,vzgtheta,zg,chixy2,chirz2,dzdr,zvtx,errphi,errxy,
                errtheta,errrz,ion,sintheta);
#else
            vtxstuff(&okok,&maxtracks,&vtxvers,&nvtxtracks,
                &ptcorr,&hvcorr,&nhitsvtx,&nsta,&cdd1size,bankno,
                vee,mu,e,tau,status,nwires,xybit,rzbit,nzhits,phi,xg,yg,
                theta,vzgtheta,zg,chixy2,chirz2,dzdr,zvtx,errphi,errxy,
                errtheta,errrz,ion,sintheta);
#endif
            if (!okok) {
                strcpy(string,"VTRH bank not found");
                XmTextSetString(trktext,string);
                SetDefaultCursor(track_bull);
                return;
            }
            /* size = #columns*#rows+about 240 */
            string = (char *) malloc(sizeof(char)*(nvtxtracks+1)*8*33 + 200);
            sprintf(string,
"VERSION %2d  No. Tracks %3d  PTcorr?  %2d  HVcorr?  %2d\n\
No. VTX Hits %4d  No. STA Tracks %4d    CDD1 Size  %4d\n",
            vtxvers,nvtxtracks,ptcorr,hvcorr,nhitsvtx,nsta,cdd1size);
            /* now do every track */
            if (nvtxtracks>0) {
                idum = sizeof(char)*nvtxtracks*8;
                row0 = (char *) XtMalloc(idum); row0[0]=0;
                row1 = (char *) XtMalloc(idum); row1[0]=0;
                row2 = (char *) XtMalloc(idum); row2[0]=0;
                row3 = (char *) XtMalloc(idum); row3[0]=0;
                row4 = (char *) XtMalloc(idum); row4[0]=0;
                row5 = (char *) XtMalloc(idum); row5[0]=0;
                row6 = (char *) XtMalloc(idum); row6[0]=0;
                row7 = (char *) XtMalloc(idum); row7[0]=0;
                row8 = (char *) XtMalloc(idum); row8[0]=0;
                row9 = (char *) XtMalloc(idum); row9[0]=0;
                row10 = (char *) XtMalloc(idum); row10[0]=0;
                row11 = (char *) XtMalloc(idum); row11[0]=0;
                row12 = (char *) XtMalloc(idum); row12[0]=0;
                row13 = (char *) XtMalloc(idum); row13[0]=0;
                row14 = (char *) XtMalloc(idum); row14[0]=0;
                row15 = (char *) XtMalloc(idum); row15[0]=0;
                row16 = (char *) XtMalloc(idum); row16[0]=0;
                row17 = (char *) XtMalloc(idum); row17[0]=0;
                row18 = (char *) XtMalloc(idum); row18[0]=0;
                row19 = (char *) XtMalloc(idum); row19[0]=0;
                row20 = (char *) XtMalloc(idum); row20[0]=0;
                row21 = (char *) XtMalloc(idum); row21[0]=0;
                row22 = (char *) XtMalloc(idum); row22[0]=0;
                row23 = (char *) XtMalloc(idum); row23[0]=0;
                row24 = (char *) XtMalloc(idum); row24[0]=0;
                row25 = (char *) XtMalloc(idum); row25[0]=0;
                sprintf(temp,"\n\n\n\nBANK   \nVEE   ?\nMU    ?\nE     ?\n\
TAU   ?\nSTATUS \nWIRES  \nXYBITS \nRZBITS \nNZHITS \nPHI    \n\
XG     \nYG     \nTHETA  \nVZGTHET\nZG     \nChiXY2 \nChiRZ2 \n\
dZdR   \nZvtx   \nerrPHI \nerrXY  \nerrTHET\nerrRZ  \ndEdX   \nsinTHET");
                XmTextSetString(trackvariabs,temp);
                strcat(string,"       ");
                for (i=0; i<nvtxtracks; i++) {
                    sprintf(temp,"     %3d",bankno[i]); strcat(row0,temp);
                    sprintf(temp,"       %1d",vee[i]); strcat(row1,temp);
                    sprintf(temp,"       %1d",mu[i]); strcat(row2,temp);
                    sprintf(temp,"       %1d",e[i]); strcat(row3,temp);
                    sprintf(temp,"       %1d",tau[i]); strcat(row4,temp);
                    sprintf(temp,"%8X",status[i]); strcat(row5,temp);
                    sprintf(temp,"    %4d",nwires[i]); strcat(row6,temp);
                    sprintf(temp,"%8X",xybit[i]); strcat(row7,temp);
                    sprintf(temp,"%8X",rzbit[i]); strcat(row8,temp);
                    sprintf(temp,"    %4d",nzhits[i]); strcat(row9,temp);
                    sprintf(temp,"  %6.1f",phi[i]); strcat(row10,temp);
                    sprintf(temp,"  %6.1f",xg[i]); strcat(row11,temp);
                    sprintf(temp,"  %6.1f",yg[i]); strcat(row12,temp);
                    sprintf(temp," %7.3f",theta[i]); strcat(row13,temp);
                    sprintf(temp," %7.3f",vzgtheta[i]); strcat(row14,temp);
                    sprintf(temp," %7.3f",zg[i]); strcat(row15,temp);
                    sprintf(temp," %7.2f",chixy2[i]); strcat(row16,temp);
                    sprintf(temp," %7.2f",chirz2[i]); strcat(row17,temp);
                    sprintf(temp," %7.3f",dzdr[i]); strcat(row18,temp);
                    sprintf(temp," %7.2f",zvtx[i]); strcat(row19,temp);
                    sprintf(temp," %7.4f",errphi[i]); strcat(row20,temp);
                    sprintf(temp," %7.4f",errxy[i]); strcat(row21,temp);
                    sprintf(temp," %7.2f",errtheta[i]); strcat(row22,temp);
                    sprintf(temp," %7.2f",errrz[i]); strcat(row23,temp);
                    sprintf(temp," %7.2f",ion[i]); strcat(row24,temp);
                    sprintf(temp," %7.2f",sintheta[i]); strcat(row25,temp);
                }
                strcat(string,"\n");
                strcat(string,row0); strcat(string,"\n");
                strcat(string,row1); strcat(string,"\n");
                strcat(string,row2); strcat(string,"\n");
                strcat(string,row3); strcat(string,"\n");
                strcat(string,row4); strcat(string,"\n");
                strcat(string,row5); strcat(string,"\n");
                strcat(string,row6); strcat(string,"\n");
                strcat(string,row7); strcat(string,"\n");
                strcat(string,row8); strcat(string,"\n");
                strcat(string,row9); strcat(string,"\n");
                strcat(string,row10); strcat(string,"\n");
                strcat(string,row11); strcat(string,"\n");
                strcat(string,row12); strcat(string,"\n");
                strcat(string,row13); strcat(string,"\n");
                strcat(string,row14); strcat(string,"\n");
                strcat(string,row15); strcat(string,"\n");
                strcat(string,row16); strcat(string,"\n");
                strcat(string,row17); strcat(string,"\n");
                strcat(string,row18); strcat(string,"\n");
                strcat(string,row19); strcat(string,"\n");
                strcat(string,row20); strcat(string,"\n");
                strcat(string,row21); strcat(string,"\n");
                strcat(string,row22); strcat(string,"\n");
                strcat(string,row23); strcat(string,"\n");
                strcat(string,row24); strcat(string,"\n");
                strcat(string,row25); strcat(string,"\n");
            }
            XmTextSetString(trktext,string);
            XtFree(string);
            if (nvtxtracks>0) {
                XtFree(row0); XtFree(row1); XtFree(row2); XtFree(row3);
                XtFree(row4); XtFree(row5); XtFree(row6); XtFree(row7);
                XtFree(row8); XtFree(row9); XtFree(row10); XtFree(row11);
                XtFree(row12); XtFree(row13); XtFree(row14); XtFree(row15);
                XtFree(row16); XtFree(row17); XtFree(row18); XtFree(row19);
                XtFree(row20); XtFree(row21); XtFree(row22); XtFree(row23);
                XtFree(row24); XtFree(row25); 
            }
            XtFree((char*)vee); XtFree((char*)mu); XtFree((char*)e); 
            XtFree((char*)tau); XtFree((char*)bankno);
            XtFree((char*)status); XtFree((char*)nwires); XtFree((char*)xybit); 
            XtFree((char*)rzbit);
            XtFree((char*)nzhits); XtFree((char*)phi); XtFree((char*)xg); 
            XtFree((char*)yg); XtFree((char*)theta);
            XtFree((char*)vzgtheta); XtFree((char*)zg); 
            XtFree((char*)chixy2); XtFree((char*)chirz2);
            XtFree((char*)dzdr); XtFree((char*)zvtx); XtFree((char*)errphi); 
            XtFree((char*)errxy);
            XtFree((char*)errtheta); XtFree((char*)errrz); 
            XtFree((char*)ion); XtFree((char*)sintheta);
        break;
    case FDC:      /* get fdc info */
#ifdef D0FLAVOR
            numfdtks_(&okok,&nfdtracks);
#else
            numfdtks(&okok,&nfdtracks);
#endif
            if (nfdtracks>0) idum = nfdtracks;
            else idum = 1;
            bankno = (int *) XtMalloc(sizeof(int)*idum);    
            vee = (int *) XtMalloc(sizeof(int)*idum);    
            mu = (int *) XtMalloc(sizeof(int)*idum);    
            e = (int *) XtMalloc(sizeof(int)*idum);    
            jet = (int *) XtMalloc(sizeof(int)*idum);    
            status = (int *) XtMalloc(sizeof(int)*idum);    
            nhits  = (int *) XtMalloc(sizeof(int)*idum);    
            fdcbits = (int *) XtMalloc(sizeof(int)*idum);    
            x0 = (float *) XtMalloc(sizeof(float)*idum);    
            y0 = (float *) XtMalloc(sizeof(float)*idum);    
            phi = (float *) XtMalloc(sizeof(float)*idum);    
            dxdz  = (float *) XtMalloc(sizeof(float)*idum);    
            dydz  = (float *) XtMalloc(sizeof(float)*idum);    
            chisq = (float *) XtMalloc(sizeof(float)*idum);    
            ion = (float *) XtMalloc(sizeof(float)*idum);    
            errion = (float *) XtMalloc(sizeof(float)*idum);    
            theta = (float *) XtMalloc(sizeof(float)*idum);    
            errphi = (float *) XtMalloc(sizeof(float)*idum);    
            errtheta = (float *) XtMalloc(sizeof(float)*idum);    
            nptsfit = (int *) XtMalloc(sizeof(int)*idum);    
            thetafit = (float *) XtMalloc(sizeof(float)*idum);    
            errthetafit = (float *) XtMalloc(sizeof(float)*idum);    
            phifit = (float *) XtMalloc(sizeof(float)*idum);    
            errphifit = (float *) XtMalloc(sizeof(float)*idum);    
            chisqfit = (float *) XtMalloc(sizeof(float)*idum);    
            vtxfit = (int *) XtMalloc(sizeof(int)*idum);    
#ifdef D0FLAVOR
            fdcstuff_(&okok,&maxtracks,&ftvers,&nfdtracks,&nfd0,&nfd1,
                &nfdwires,&cdd3size,&alignlevel,&fdfull,&fdz0,&fdz1,bankno,
                vee,mu,e,jet,status,nhits,fdcbits,x0,y0,phi,dxdz,dydz,
                chisq,ion,errion,theta,errphi,errtheta,nptsfit,
                thetafit,errthetafit,phifit,errphifit,chisqfit,vtxfit);
#else
            fdcstuff(&okok,&maxtracks,&ftvers,&nfdtracks,&nfd0,&nfd1,
                &nfdwires,&cdd3size,&alignlevel,&fdfull,&fdz0,&fdz1,bankno,
                vee,mu,e,jet,status,nhits,fdcbits,x0,y0,phi,dxdz,dydz,
                chisq,ion,errion,theta,errphi,errtheta,nptsfit,
                thetafit,errthetafit,phifit,errphifit,chisqfit,vtxfit);
#endif
            if (!okok) {
                strcpy(string,"FTRH bank not found");
                XmTextSetString(trktext,string);
                SetDefaultCursor(track_bull);
                return;
            }
            /* size = #columns*#rows+about 240 */
            string = (char *) malloc(sizeof(char)*(nfdtracks+1)*8*33 + 200);
            if (fdfull) sprintf(string,
"VERSION %2d  BNK VER %2d  FULL TRKING YES  No. Tracks %3d\n\
No. FD Wires Half 0  %4d  No. FD Wires Half 1  %4d    CDD3 Size  %4d\n\
ALIGN_LEVEL  %4d   Z Point Half 0   %6.1f     Z Point Half 1    %6.1f\n",
                ftvers,nfdtracks,nfd0,nfd1,nfdwires,cdd3size,alignlevel,
                fdz0,fdz1);
            else sprintf(string,
"VERSION %2d  BNK VER %2d  FULL TRKING NO  No. Tracks %3d\n\
No. FD Wires Half 0  %4d  No. FD Wires Half 1  %4d    CDD3 Size  %4d\n\
ALIGN_LEVEL  %4d   Z Point Half 0   %6.1f     Z Point Half 1    %6.1f\n",
                ftvers,nfdtracks,nfd0,nfd1,nfdwires,cdd3size,alignlevel,
                fdz0,fdz1);
            /* now do every track */
            if (nfdtracks>0) {
                idum = sizeof(char)*nfdtracks*8;
                row0 = (char *) XtMalloc(idum); row0[0]=0;
                row1 = (char *) XtMalloc(idum); row1[0]=0;
                row2 = (char *) XtMalloc(idum); row2[0]=0;
                row3 = (char *) XtMalloc(idum); row3[0]=0;
                row4 = (char *) XtMalloc(idum); row4[0]=0;
                row5 = (char *) XtMalloc(idum); row5[0]=0;
                row6 = (char *) XtMalloc(idum); row6[0]=0;
                row7 = (char *) XtMalloc(idum); row7[0]=0;
                row8 = (char *) XtMalloc(idum); row8[0]=0;
                row9 = (char *) XtMalloc(idum); row9[0]=0;
                row10 = (char *) XtMalloc(idum); row10[0]=0;
                row11 = (char *) XtMalloc(idum); row11[0]=0;
                row12 = (char *) XtMalloc(idum); row12[0]=0;
                row13 = (char *) XtMalloc(idum); row13[0]=0;
                row14 = (char *) XtMalloc(idum); row14[0]=0;
                row15 = (char *) XtMalloc(idum); row15[0]=0;
                row16 = (char *) XtMalloc(idum); row16[0]=0;
                row17 = (char *) XtMalloc(idum); row17[0]=0;
                row18 = (char *) XtMalloc(idum); row18[0]=0;
                row19 = (char *) XtMalloc(idum); row19[0]=0;
                row20 = (char *) XtMalloc(idum); row20[0]=0;
                row21 = (char *) XtMalloc(idum); row21[0]=0;
                row22 = (char *) XtMalloc(idum); row22[0]=0;
                row23 = (char *) XtMalloc(idum); row23[0]=0;
                row24 = (char *) XtMalloc(idum); row24[0]=0;
                sprintf(temp,"\n\n\n\nBANK   \nE     ?\nMU    ?\nJET   ?\n\
VEE   ?\nSTATUS \nWIRES  \nX0     \nY0     \nPHI    \ndxdz   \n\
dydz   \nCHISQ  \ndEdX   \n error \nTHETA  \nerrPHI \nerrTHE \n\
NptsFIT\nTheFIT \n error \nPhiFIT \n error \nChi2FIT\nVTXfit ");
                XmTextSetString(trackvariabs,temp);
                strcat(string,"       ");
                for (i=0; i<nfdtracks; i++) {
                    sprintf(temp,"     %3d",bankno[i]); strcat(row0,temp);
                    sprintf(temp,"       %1d",e[i]); strcat(row1,temp);
                    sprintf(temp,"       %1d",mu[i]); strcat(row2,temp);
                    sprintf(temp,"       %1d",jet[i]); strcat(row3,temp);
                    sprintf(temp,"       %1d",vee[i]); strcat(row4,temp);
                    sprintf(temp,"%8X",status[i]); strcat(row5,temp);
                    sprintf(temp,"%8X",fdcbits[i]); strcat(row6,temp);
                    sprintf(temp,"  %6.1f",x0[i]); strcat(row7,temp);
                    sprintf(temp,"  %6.1f",y0[i]); strcat(row8,temp);
                    sprintf(temp,"  %6.1f",phi[i]); strcat(row9,temp);
                    sprintf(temp,"  %6.1f",dxdz[i]); strcat(row10,temp);
                    sprintf(temp,"  %6.1f",dydz[i]); strcat(row11,temp);
                    sprintf(temp,"  %6.1f",chisq[i]); strcat(row12,temp);
                    sprintf(temp," %7.2f",ion[i]); strcat(row13,temp);
                    sprintf(temp," %7.2f",errion[i]); strcat(row14,temp);
                    sprintf(temp," %7.2f",theta[i]); strcat(row15,temp);
                    sprintf(temp," %7.3f",errphi[i]); strcat(row16,temp);
                    sprintf(temp," %7.3f",errtheta[i]); strcat(row17,temp);
                    sprintf(temp,"   %5d",nptsfit[i]); strcat(row18,temp);
                    sprintf(temp," %7.2f",thetafit[i]); strcat(row19,temp);
                    sprintf(temp," %7.3f",errthetafit[i]); strcat(row20,temp);
                    sprintf(temp," %7.2f",phifit[i]); strcat(row21,temp);
                    sprintf(temp," %7.3f",errphifit[i]); strcat(row22,temp);
                    sprintf(temp," %7.2f",chisqfit[i]); strcat(row23,temp);
                    sprintf(temp,"   %5d",vtxfit[i]); strcat(row24,temp);
                }
                strcat(string,"\n");
                strcat(string,row0); strcat(string,"\n");
                strcat(string,row1); strcat(string,"\n");
                strcat(string,row2); strcat(string,"\n");
                strcat(string,row3); strcat(string,"\n");
                strcat(string,row4); strcat(string,"\n");
                strcat(string,row5); strcat(string,"\n");
                strcat(string,row6); strcat(string,"\n");
                strcat(string,row7); strcat(string,"\n");
                strcat(string,row8); strcat(string,"\n");
                strcat(string,row9); strcat(string,"\n");
                strcat(string,row10); strcat(string,"\n");
                strcat(string,row11); strcat(string,"\n");
                strcat(string,row12); strcat(string,"\n");
                strcat(string,row13); strcat(string,"\n");
                strcat(string,row14); strcat(string,"\n");
                strcat(string,row15); strcat(string,"\n");
                strcat(string,row16); strcat(string,"\n");
                strcat(string,row17); strcat(string,"\n");
                strcat(string,row18); strcat(string,"\n");
                strcat(string,row19); strcat(string,"\n");
                strcat(string,row20); strcat(string,"\n");
                strcat(string,row21); strcat(string,"\n");
                strcat(string,row22); strcat(string,"\n");
                strcat(string,row23); strcat(string,"\n");
                strcat(string,row24); strcat(string,"\n");
            }
            XmTextSetString(trktext,string);
            XtFree(string);
            if (nfdtracks>0) {
                XtFree(row0); XtFree(row1); XtFree(row2); XtFree(row3);
                XtFree(row4); XtFree(row5); XtFree(row6); XtFree(row7);
                XtFree(row8); XtFree(row9); XtFree(row10); XtFree(row11);
                XtFree(row12); XtFree(row13); XtFree(row14); XtFree(row15);
                XtFree(row16); XtFree(row17); XtFree(row18); XtFree(row19);
                XtFree(row20); XtFree(row21); XtFree(row22); XtFree(row23);
                XtFree(row24); 
            }
            XtFree((char*)bankno); XtFree((char*)vee); XtFree((char*)mu); 
            XtFree((char*)e); XtFree((char*)jet);
            XtFree((char*)status); XtFree((char*)nhits); 
            XtFree((char*)fdcbits); XtFree((char*)x0);
            XtFree((char*)y0); XtFree((char*)phi); XtFree((char*)dxdz ); 
            XtFree((char*)dydz );
            XtFree((char*)chisq); XtFree((char*)ion); 
            XtFree((char*)errion); XtFree((char*)theta);
            XtFree((char*)errphi); XtFree((char*)errtheta); 
            XtFree((char*)nptsfit);
            XtFree((char*)thetafit); XtFree((char*)errthetafit); 
            XtFree((char*)phifit);
            XtFree((char*)errphifit); XtFree((char*)chisqfit); 
            XtFree((char*)vtxfit);
    default:
        break;
    }
    SetDefaultCursor(track_bull);
}
