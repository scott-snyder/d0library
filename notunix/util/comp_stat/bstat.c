/*
	Purpose: Program to return info on batch daemon configuration 

			 Usage:
					$ batchstat  characteristic  [output file]

	Inputs:	characteristic - a comma separated list of queue characteristics
							 for which one would like configuration info. This
							 may be any valid characteristic acceptable to the
							 batch daemon or the special values:

								all_classes
								all_nodes
								sys$batch

			outfile 	   - an optional file to which the output may be 
							 redirected.  If no file is speciifed, output is
						     to standard output.

	Outputs: For project type characteristics, one gets a list of nodes
			 belonging to that project as well as a summary of the number
			 of job slots available and CPU power.
			 For node type characteristics, one gets a list of the projects
			 to which that node belongs as well as the node job slots and
			 CPU power.
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define TMAX 10
#define TMAXL 25
#define BLEN 256

struct Clslnk {
	struct Class *pclass;
	struct Clslnk *pfwd;
};

struct Node  {
	char	name[13];
	char	cput[13];
	int		ndsk;
	int		ntpe;
	float	memy;
	float	tgbt;
	float	fgbt;
	float	cpu;
	int		eslots;
	struct	Clslnk pclist;
};

struct Rsvlnk {
	struct	Node *pnode;
	int		rslots;
	struct	Rsvlnk *pfwd;
};

struct Class {
	char	bchar[20];
	int		nodes;
	int		priority;
	int		joblim;
	int		jslots;
	float	tcpu;
	struct Rsvlnk pnlist;
};

struct Class prjs[20];
struct Node nodes[200];

int decode(char *,char **);
int istype(char **, char *);
int isclass(char *, int);
int isnode(char *, int);
int clsinfo(char **,struct Class *);
int ndeinfo(char **,struct Node *);
int rsvinfo(char **,int,int);
struct Node *fndnode(char *,int);
void prntcls(FILE *,int,int);
void prntnde(FILE *,int);
char *stoupper(char *);
void hardware(char *,int *,FILE *);

int ign = 0;

main(int argc,char *argv[])
{

#if VMS
	char batchfile[] = "batch_daemon$root:[data]sys$batch.parameters";
	char equipfile[] = "batch_daemon$root:[data]hardware.parameters";
#else
	char batchfile[] = "/d0sgi9/usr0/diesburg/nodedata/utilities/sys.batch";
	char equipfile[] = "/d0sgi9/usr0/diesburg/nodedata/utilities/memry.list";
#endif

	char ibuf[BLEN];
	char *tokens[TMAX];
	char *chctrs[TMAX];
	int  inode = 0, iclass = 0;
	int  i,j,icl,ind,ntks,nchrs;
	FILE *ofile = stdout;
	FILE *ifile = stdin;
	FILE *efile;

	if ((ifile=fopen(batchfile,"r")) == NULL ) {
		fprintf(stderr,"Could not open batch daemon configuration file\n");
		perror("BATCHSTAT");
		exit;
	}

	if ((efile=fopen(equipfile,"r")) == NULL ) {
		fprintf(stderr,"Could not open hardware configuration file\n");
		perror("BATCHSTAT");
		exit;
	}

	if ( argc > 1 ) {
		nchrs = decode(argv[1],chctrs);
	}

	if ( argc == 3 ) {
		if ((ofile=fopen(argv[2],"w")) == NULL ) {
			fprintf(stderr,"Could not open output file %s\n",argv[2]);
			perror("BATCHSTAT");
			exit;
		}
	}

	while ( fgets(ibuf,BLEN,ifile) != NULL ) {
		if ( ibuf[0] != '!' ) {
			ntks = decode(ibuf,tokens);
			if ( istype(tokens,"DEFINE")||istype(tokens,"GENERIC") ) {
				clsinfo(tokens,&prjs[iclass++]);
			}
			if ( istype(tokens,"EXECUTOR_QUEUE") ) {
				ndeinfo(tokens,&nodes[inode++]);
			}
			if ( istype(tokens,"RESERVE_SLOTS") ) {
				rsvinfo(tokens,iclass,inode);
			}			
		}
	}

	while ( fgets(ibuf,BLEN,efile) != NULL ) {
		hardware(ibuf,&inode,ofile);
	}

	for ( i = 0; i < nchrs; i++ ) {
		if ( (icl = isclass(chctrs[i],iclass)) != 999 ) {
			if ( icl < 0 ) {
				for ( j = 0; j < iclass; j++ ) {
					prntcls(ofile,j,icl);
				}
			}
			else {
				prntcls(ofile,icl,icl);
			}
		}
		if ( (ind = isnode(chctrs[i],inode)) != 999 ) {
			if ( ind < 0 ) {
				for ( j = 0; j < inode; j++ ) {
					prntnde(ofile,j);
				}
			}
			else {
				prntnde(ofile,ind);
			}
		}
	}
}
  

void hardware(char *ibuf,int *inode,FILE *ofile)
{
	char *tok;
	int ind;
	struct Clslnk *pcls;

	tok = ibuf;
	tok=strtok(tok," 	");
		if ((ind = isnode(tok,*inode)) == 999 ) {
			ind = (*inode)++;
			strncpy(nodes[ind].name,tok,6);
			nodes[ind].eslots = 0;
			nodes[ind].pclist.pfwd = NULL;
			nodes[ind].pclist.pclass = NULL;
		}
		if ((ind = isnode(tok,*inode)) != 999 && (ind >= 0)) {
			tok = strtok(NULL," 	");
			sscanf(tok,"%f",&nodes[ind].memy);
			tok = strtok(NULL," 	");
			sscanf(tok,"%f",&nodes[ind].tgbt);
			tok = strtok(NULL," 	");
			sscanf(tok,"%d",&nodes[ind].ndsk);
			tok = strtok(NULL," 	");
			sscanf(tok,"%d",&nodes[ind].ntpe);
			tok = strtok(NULL," 	");
			sscanf(tok,"%f",&nodes[ind].fgbt);
			tok = strtok(NULL," 	");
			sscanf(tok,"%s",&nodes[ind].cput);
			tok = NULL;
		}
}

void prntnde(FILE *ofile,int i)
{
	struct Clslnk *pcllst;

	fprintf(ofile,"  Node   : %6s",nodes[i].name);
	fprintf(ofile,"  VUPS : %4.1f",nodes[i].cpu);
	fprintf(ofile,"  Execution Slots : %2d",nodes[i].eslots);
	pcllst = &nodes[i].pclist;
	if ( pcllst->pfwd != NULL ) {
		while ( pcllst != NULL ) {
			fprintf(ofile,"  %s",pcllst->pclass->bchar);
			pcllst = pcllst->pfwd;
			if ( pcllst->pfwd == NULL ) break;
		}
	}
	fprintf(ofile,"\n");
	fprintf(ofile,"  CPU/Mem: %-13s %3.0fMb\n",nodes[i].cput,nodes[i].memy);
	fprintf(ofile,"  Disks  : %4.2f of %4.2f Gbytes free on %d disks\n",nodes[i].fgbt,nodes[i].tgbt,nodes[i].ndsk);
	fprintf(ofile,"  Tapes  : %d\n\n",nodes[i].ntpe);
}

void prntcls(FILE *ofile,int i,int ifl)
{
	struct Rsvlnk *pndlst;
	int tdsks = 0,ttpes = 0;
	float fgbts = 0., tgbts = 0.;

	fprintf(ofile,"\nClass         : %s\n",prjs[i].bchar);
	if ( i != ign ) {
		fprintf(ofile,"Base Priority : %3d\n",prjs[i].priority);
	}
	fprintf(ofile,"User Job Limit: %3d\n",prjs[i].joblim);
	fprintf(ofile,"Total nodes   : %3d\t",prjs[i].nodes);
	fprintf(ofile,"Total CPU (VUPS): %5.1f\n",prjs[i].tcpu);
	fprintf(ofile,"Max Execution Slots: %3d\n",prjs[i].jslots);
	pndlst = &prjs[i].pnlist;
	if ( pndlst->pfwd != NULL ) {
		fprintf(ofile,"                    Memory   Job     Disk    Free");
		fprintf(ofile,"\n Node    CPU Type    (Mb)   Slots   Blocks  Blocks  Disks  Tapes");
		fprintf(ofile,"\n------------------------------------------------------------------\n");
		while ( pndlst != NULL ) {
			fprintf(ofile,"%6s",pndlst->pnode->name);
			fprintf(ofile,"%13s  ",pndlst->pnode->cput);
			fprintf(ofile,"%5.2f   ",pndlst->pnode->memy);
			fprintf(ofile,"%2d     ",pndlst->rslots);
			fprintf(ofile,"%5.2f   ",pndlst->pnode->tgbt);
			fprintf(ofile,"%5.2f   ",pndlst->pnode->fgbt);
			fprintf(ofile,"%3d    ",pndlst->pnode->ndsk);
			fprintf(ofile,"%3d\n",pndlst->pnode->ntpe);
			tdsks += pndlst->pnode->ndsk;
			ttpes += pndlst->pnode->ntpe;
			tgbts += pndlst->pnode->tgbt;
			fgbts += pndlst->pnode->fgbt;
			pndlst = pndlst->pfwd;
			if ( pndlst->pfwd == NULL ) break;
		}	
	}
	if ( i != ign ) {
		fprintf(ofile,"\n Disks  : %4.2f of %4.2f",fgbts,tgbts);
		fprintf(ofile," Gbytes free on %d disks\n",tdsks);
		fprintf(ofile," Tapes  : %d\n",ttpes);
		fprintf(ofile,"\n");
	}
}

int rsvinfo(char **tks,int classes,int maxnode)
{
	char **tmp;
	char cltype[20];
	int ind,icl,slots;
	struct Rsvlnk *prsv;
	struct Rsvlnk *ptmp;
	struct Clslnk *pcls;
	struct Clslnk *psav;

	tmp = tks;
	while ( *tmp != NULL ) {
		if (istype(tmp,"CLASS")) {
			sscanf(*(tmp+1),"%s",cltype);
		}
		if (istype(tmp,"JOB")) {
			sscanf(*(tmp+1),"%d",&slots);
		}
	tmp++;
	}
	for ( icl = 0; icl < classes; icl++) {
		if ( strcmp(prjs[icl].bchar,cltype) == 0 ) {
			break;
		}
	}
	prjs[icl].nodes += 1;
	prsv = prjs[icl].pnlist.pfwd;
	ptmp = &prjs[icl].pnlist;
	while ( prsv != NULL ) {
		ptmp = prsv;
		prsv = prsv->pfwd;
	}
	prsv = malloc(sizeof(struct Rsvlnk));
	if ( prsv == NULL ) {
		fprintf(stderr,"Couldn't allocate memory for node list\n");
		exit (1);
	}
	ptmp->pfwd = prsv;
	ptmp->rslots = slots;
	ptmp->pnode = fndnode(strtok(tks[1],"_"),maxnode);
	prjs[icl].tcpu += ptmp->pnode->cpu;
	prjs[icl].jslots += slots;
	prsv->pfwd = NULL;

	for ( ind = 0; ind < maxnode; ind++) {
		if ( strcmp(nodes[ind].name,strtok(tks[1],"_")) == 0 ) {
			break;
		}
	}
	pcls = nodes[ind].pclist.pfwd;
	psav = &nodes[ind].pclist;
	while ( pcls != NULL ) {
		psav = pcls;
		pcls = pcls->pfwd;
	}
	pcls = malloc(sizeof(struct Clslnk));
	if ( pcls == NULL ) {
		fprintf(stderr,"Couldn't allocate memory for class list\n");
		exit (1);
	}
	psav->pfwd = pcls;
	psav->pclass = &prjs[icl];
	pcls->pfwd = NULL;
}

struct Node *fndnode(char *node,int maxnode)
{
	int i;
	
	for ( i = 0; i < maxnode; i++ ) {
		if ( strcmp(nodes[i].name,node) == 0 ) {
			return &nodes[i];
		}
	}
	return NULL;
}

int ndeinfo(char **tks,struct Node *nde)
{
	strcpy(nde->name,strtok(tks[1],"_"));
	while ( *tks != NULL ) {
		if ( strncmp(*tks,"TOT",3) == 0 ) {
			sscanf(*(tks+1),"%d",&nde->eslots);
		}
		if ( strncmp(*tks,"REL",3) == 0 ) {
			sscanf(*(tks+1),"%f",&nde->cpu);
		}
	tks++;
	}
	prjs[ign].nodes += 1;
	prjs[ign].tcpu += nde->cpu;
	prjs[ign].jslots += nde->eslots;
	nde->pclist.pfwd = NULL;
}

int clsinfo(char **tks,struct Class *cls)
{
	strcpy(cls->bchar,tks[1]);
	while ( *tks != NULL ) {
		if ( strncmp(*tks,"PRI",3) == 0 ) {
			sscanf(*(tks+1),"%d",&cls->priority);
		}
		if ( strncmp(*tks,"JOB",3) == 0 ) {
			sscanf(*(tks+1),"%d",&cls->joblim);
		}
	tks++;
	}
	cls->pnlist.pfwd = NULL;
}

int istype(char **tks, char *str)
{
	if ( strncmp(tks[0],str,strlen(str)) == 0 ) return 1;
	return 0;
}

int isclass(char *tks, int maxclass)
{
	int i;

	for (i = 0; i < maxclass; i++ ) {
		if ( strncmp(tks,prjs[i].bchar,strlen(tks)) == 0 ) return i;
	}
	if ( strncmp(tks,"ALL_C",5) == 0 ) return -1;
	if ( strncmp(tks,"FULL_C",6) == 0 ) return -2;
	return 999;
}

int isnode(char *tks, int maxnode)
{
	int i;

	for (i = 0; i < maxnode; i++ ) {
		if ( strcmp(tks,nodes[i].name) == 0 ) return i;
	}
	if ( strncmp(tks,"ALL_N",5) == 0 ) return -maxnode;
	return 999;
}

int decode(char *tok, char **tks)
{
	int ntks,i = 0;

	tks[i] = NULL;
	while ((tok=strtok(tok," /	=,")) != NULL) {
		if ((tks[i]=malloc(strlen(tok)+1)) == NULL) {
			fprintf(stderr,"Couldn't allocate memory for decode\n");
			exit;
		}
		strcpy(tks[i],tok);
		stoupper(tks[i++]);
		tok = NULL;
	}
	tks[i] = NULL;
	return i;
}

char *stoupper(char *str)
{
	while ((*str = toupper(*str)) != '\0' ) {
		str++;
	}
	return (str);
}
