
#include <stdio.h>                      /* For printf and so on. */
#include "xframe/source/d0x_c.h"
/*#include <X11/X.h>*/

/* The vuit generated application include file.				    */
/* If the user does not specify a directory for the file		    */
/* then the vaxc$include logical needs to be defined to point to the	    */
/* directory of the include file.					    */
#include "xframe/source/d0x.h"

/*
 * Global data
 */
static MrmType class_id;		/* Place to keep class ID*/
static MrmType *dummy_class;            /* and class variable. */
static char *db_filename_vec[] =        /* Mrm.hierachy file list. */
  {
"d0xuid"
  };
static int db_filename_num =
                (sizeof db_filename_vec / sizeof db_filename_vec [0]);
char *vuit_dummy_ident_value = "VUIT dummy identifier value";
int i;
#define hash_table_limit 500
struct HASH_TABLE_STRUCT
    {
    char	*widget_name;
    Widget	id;
    } hash_table[hash_table_limit + 1];

/*
 * Forward declarations
 */
void quitting();
void help_proc();
void main_init();
void save_text_tag();
void specify_file_type();
void file_open();
void control_proc();
void zebra_proc();
void d0util_proc();
void mode_proc();
void search_type();
void interrupt();
void store_type();
void select_proc();
void format_proc();
void zeblst_proc();
void navigate_proc();
void again_proc();
void set_curtop();
void man_raw();
void unman_raw();
void decodeit();
void rawcall();
void out_event();
void printit();
void man_tree();
void unman_tree();
void unman_ztree();
void um_navc();
void um_navz();
void um_navd();
void um_tree();
void man_navc();
void man_navd();
void man_navz();
void man_navt();
void allout();
void manphys();
void unmanphys();
void cphysics();
void jetsel();
void mandisp();
void unmandisp();
void selectcol();
void phioff();
void allinfo();
void manphyshelp();
void umphyshelp();
void manfiles();
void umfiles();
void eselcor();
void manout();
void umout();
void umlist();
void manlist();
void rotatephi();
void rotatetheta();
void rotatepsi();
void manjetb();
void umjetb();
void manpb();
void umpb();
void manem();
void umem();
void treesize();
void manpmuo();
void umpmuo();
void mancol();
void umcol();
void manq();
void umq();
void doquery();
void squery();
void manzq();
void umzq();
void manqh();
void umqh();
void manhist();
void umhist();
void d0xhist();
void manhform();
void umhform();
void chdraw();
void mankunz();
void umkunzb();
void mantup();
void umtup();
void manpmc();
void umpmc();
void mandec();
void umdec();
void mand0x();
void umd0x();
void manqsub();
void umqsub();
void manpack();
void umpack();
void mand0dad();
void umd0dad();
void manpt();
void umpt();
void manen();
void umen();
void mantrk();
void umtrk();
void d0xtracks();
void mancal();
void umcal();
void d0xcaeh();

/*
 * Names and addresses of callback routines to register with Mrm
 */
static MrmRegisterArg reglist [] = {
{"quitting", (caddr_t)quitting},
{"help_proc", (caddr_t)help_proc},
{"main_init", (caddr_t)main_init},
{"save_text_tag", (caddr_t)save_text_tag},
{"specify_file_type", (caddr_t)specify_file_type},
{"file_open", (caddr_t)file_open},
{"control_proc", (caddr_t)control_proc},
{"zebra_proc", (caddr_t)zebra_proc},
{"d0util_proc", (caddr_t)d0util_proc},
{"mode_proc", (caddr_t)mode_proc},
{"search_type", (caddr_t)search_type},
{"interrupt", (caddr_t)interrupt},
{"store_type", (caddr_t)store_type},
{"select_proc", (caddr_t)select_proc},
{"format_proc", (caddr_t)format_proc},
{"zeblst_proc", (caddr_t)zeblst_proc},
{"navigate_proc", (caddr_t)navigate_proc},
{"again_proc", (caddr_t)again_proc},
{"set_curtop", (caddr_t)set_curtop},
{"man_raw", (caddr_t)man_raw},
{"unman_raw", (caddr_t)unman_raw},
{"decodeit", (caddr_t)decodeit},
{"rawcall", (caddr_t)rawcall},
{"out_event", (caddr_t)out_event},
{"printit", (caddr_t)printit},
{"man_tree", (caddr_t)man_tree},
{"unman_tree", (caddr_t)unman_tree},
{"unman_ztree", (caddr_t)unman_ztree},
{"um_navc", (caddr_t)um_navc},
{"um_navz", (caddr_t)um_navz},
{"um_navd", (caddr_t)um_navd},
{"um_tree", (caddr_t)um_tree},
{"man_navc", (caddr_t)man_navc},
{"man_navd", (caddr_t)man_navd},
{"man_navz", (caddr_t)man_navz},
{"man_navt", (caddr_t)man_navt},
{"allout", (caddr_t)allout},
{"manphys", (caddr_t)manphys},
{"unmanphys", (caddr_t)unmanphys},
{"cphysics", (caddr_t)cphysics},
{"jetsel", (caddr_t)jetsel},
{"mandisp", (caddr_t)mandisp},
{"unmandisp", (caddr_t)unmandisp},
{"selectcol", (caddr_t)selectcol},
{"phioff", (caddr_t)phioff},
{"allinfo", (caddr_t)allinfo},
{"manphyshelp", (caddr_t)manphyshelp},
{"umphyshelp", (caddr_t)umphyshelp},
{"manfiles", (caddr_t)manfiles},
{"umfiles", (caddr_t)umfiles},
{"eselcor", (caddr_t)eselcor},
{"manout", (caddr_t)manout},
{"umout", (caddr_t)umout},
{"umlist", (caddr_t)umlist},
{"manlist", (caddr_t)manlist},
{"rotatephi", (caddr_t)rotatephi},
{"rotatetheta", (caddr_t)rotatetheta},
{"rotatepsi", (caddr_t)rotatepsi},
{"manjetb", (caddr_t)manjetb},
{"umjetb", (caddr_t)umjetb},
{"manpb", (caddr_t)manpb},
{"umpb", (caddr_t)umpb},
{"manem", (caddr_t)manem},
{"umem", (caddr_t)umem},
{"treesize", (caddr_t)treesize},
{"manpmuo", (caddr_t)manpmuo},
{"umpmuo", (caddr_t)umpmuo},
{"mancol", (caddr_t)mancol},
{"umcol", (caddr_t)umcol},
{"manq", (caddr_t)manq},
{"umq", (caddr_t)umq},
{"doquery", (caddr_t)doquery},
{"squery", (caddr_t)squery},
{"manzq", (caddr_t)manzq},
{"umzq", (caddr_t)umzq},
{"manqh", (caddr_t)manqh},
{"umqh", (caddr_t)umqh},
{"manhist", (caddr_t)manhist},
{"umhist", (caddr_t)umhist},
{"d0xhist", (caddr_t)d0xhist},
{"manhform", (caddr_t)manhform},
{"umhform", (caddr_t)umhform},
{"chdraw", (caddr_t)chdraw},
{"mankunz", (caddr_t)mankunz},
{"umkunzb", (caddr_t)umkunzb},
{"mantup", (caddr_t)mantup},
{"umtup", (caddr_t)umtup},
{"manpmc", (caddr_t)manpmc},
{"umpmc", (caddr_t)umpmc},
{"mandec", (caddr_t)mandec},
{"umdec", (caddr_t)umdec},
{"mand0x", (caddr_t)mand0x},
{"umd0x", (caddr_t)umd0x},
{"manqsub", (caddr_t)manqsub},
{"umqsub", (caddr_t)umqsub},
{"manpack", (caddr_t)manpack},
{"umpack", (caddr_t)umpack},
{"mand0dad", (caddr_t)mand0dad},
{"umd0dad", (caddr_t)umd0dad},
{"manpt", (caddr_t)manpt},
{"umpt", (caddr_t)umpt},
{"manen", (caddr_t)manen},
{"umen", (caddr_t)umen},
{"mantrk", (caddr_t)mantrk},
{"umtrk", (caddr_t)umtrk},
{"d0xtracks", (caddr_t)d0xtracks},
{"mancal", (caddr_t)mancal},
{"umcal", (caddr_t)umcal},
{"d0xcaeh", (caddr_t)d0xcaeh}};

static int reglist_num = (sizeof reglist / sizeof reglist[0]);

/*
 * Names and addresses of uil identifiers (if any) to register with Mrm.
 * These identifiers are registered with a dummy value to allow the generated 
 * code to run without error.
 * You can avoid the registration of these identifiers by simplying editing
 * this template file (vuit_main_template_c) and removing the following
 * special format comments:
 *	***VUIT ident registration***
 *	***VUIT identlist size***
 *	***VUIT register identifiers***
 * You can provide your own registration of identifiers by calling your own
 * routine in response to a callback (such as the MrmNcreateCallback for your
 * application's main window), or by modifying this template to call your
 * own registration routine.
 */


/*
 * OS transfer point.  The main routine does all the one-time setup and
 * then calls XtAppMainLoop.
 */
int main(argc, argv)
    int argc;                  		    /* Command line argument count. */
    char *argv[];                       /* Pointers to command line args. */
{
    Arg arglist[2];
    int n;

    MrmInitialize();			/* Initialize MRM before initializing */
                                        /* the X Toolkit. */
    DXmInitialize();			/* Initialize additional DEC widgets */

    /* 
     * If we had user-defined widgets, we would register them with Mrm.here. 
     */

    /* 
     * Initialize the X Toolkit. We get back a top level shell widget.
     */
    XtToolkitInitialize();

    app_context = XtCreateApplicationContext();
    display = XtOpenDisplay(app_context, NULL, "d0x", "D0X",
                            NULL, 0, &argc, argv);
    if (display == NULL) 
	{
        fprintf(stderr, "%s:  Can't open display\n", argv[0]);
        exit(1);
	}

    n = 0;
    XtSetArg(arglist[n], XmNallowShellResize, True);  n++;
    toplevel_widget = XtAppCreateShell("d0x", "D0X", applicationShellWidgetClass,
                              display, arglist, n);

    /* 
     * Open the UID files (the output of the UIL compiler) in the hierarchy
     */
    if (MrmOpenHierarchy(db_filename_num, /* Number of files. */
      db_filename_vec,                    /* Array of file names.  */
      NULL,                               /* Default OS extenstion. */
      &s_MrmHierarchy)                    /* Pointer to returned MRM ID */
      !=MrmSUCCESS)
        s_error("can't open hierarchy");

MrmRegisterNames (reglist, reglist_num);


VUIT_Manage("d0x_main");

    /* 
     * Realize the top level widget.  All managed children now become visible
     */
    XtRealizeWidget(toplevel_widget);

    /* 
     * Sit around forever waiting to process X-events.  We never leave
     * XtAppMainLoop. From here on, we only execute our callback routines. 
     */
    XtAppMainLoop(app_context);
}

/*
 * All errors are fatal.
 */
void s_error(problem_string)
    char *problem_string;
{
    printf("%s\n", problem_string);
    exit(0);
}
void VUIT_Manage(widget_name)
    char	*widget_name;
{
    Widget		id;
    Window		pop_window;
    XWindowChanges	values;

    if (HashLookup(widget_name, &id))
	if (XtIsManaged(id))
	    {
	    pop_window = XtWindow(XtParent(id));
	    values.x = values.y = values.width = values.height =
		values.border_width = values.sibling = 0;
	    values.stack_mode = Above;
	    XConfigureWindow(display, pop_window, CWStackMode, &values);
	    }
	else
	    XtManageChild(id);
    else
	{
	id = NULL;
	MrmFetchWidget(s_MrmHierarchy, widget_name, toplevel_widget, &id, 
	    &class_id);
	XtManageChild(id);
	HashRegister(widget_name, id);
	}
}
void VUIT_Unmanage(widget_name)
    char	*widget_name;
{
    Widget	id;

    if (HashLookup(widget_name, &id))
	XtUnmanageChild(id);
}
int HashRegister (widget_name, id)
    char		*widget_name;
    Widget		id;    
{
    int			ndx;

    for (ndx = HashFunction(widget_name, hash_table_limit);
	((hash_table[ndx].widget_name != NULL) &&
	    (ndx < hash_table_limit));
	ndx++);
    if (hash_table[ndx].widget_name != NULL)
	for (ndx = 0;
	    hash_table[ndx].widget_name != NULL;
	    ndx++);
    if (ndx > hash_table_limit)
	return (FALSE);
    else
	{
	hash_table[ndx].widget_name = XtCalloc(1, strlen(widget_name) + 1);
	strcpy(hash_table[ndx].widget_name, widget_name);
	hash_table[ndx].id = id;
	return (TRUE);
	}
}

int HashLookup (name, id)
    char		*name;
    Widget		*id;
{
    int			ndx;

    for (ndx = HashFunction(name, hash_table_limit);
	((hash_table[ndx].widget_name != NULL) &&
	    (ndx <= hash_table_limit));
	ndx++)
	if (strcmp(name, hash_table[ndx].widget_name) == 0)
	    {
	    *id = hash_table[ndx].id;
	    return (TRUE);
	    }

    if (ndx > hash_table_limit)
	for (ndx = 0;
	    ((hash_table[ndx].widget_name != NULL) &&
		(ndx <= hash_table_limit));
	    ndx++)
	    {
	    if (strcmp(name, hash_table[ndx].widget_name) == 0)
		{
	 	*id = hash_table[ndx].id;
		return (TRUE);
		}
	    }

    return (FALSE);
}
int HashFunction (name, max)
    char		*name;
    int			max;

{
#define HashVecSize		20	/* plenty for 31 character names */
typedef union
    {
    short int		intname[HashVecSize];	 /* name as vector of ints */
    char		charname[2*HashVecSize]; /* name as vector of chars */
    } HashName;

    HashName		locname;	/* aligned name */
    int			namelen;	/* length of name */
    int			namelim;	/* length limit (fullword size) */
    int			namextra;	/* limit factor remainder */
    int			code = 0;	/* hash code value */
    int			ndx;		/* loop index */


    /*
     * Copy the name into the local aligned union.
     * Process the name as a vector of integers, with some remaining characters.
     * The string is copied into a local union in order to force correct
     * alignment for alignment-sensitive processors.
     */
    strcpy (locname.charname, name);
    namelen = strlen (locname.charname);
    namelim = namelen >> 1;		/* divide by 2 */
    namextra = namelen & 1;		/* remainder */

    /*
     * XOR each integer part of the name together, followed by the trailing
     * 0/1 character
     */
    for ( ndx=0 ; ndx<namelim ; ndx++ )
        code = code ^ ((locname.intname[ndx])<<ndx);
    if ( namextra > 0 )
        code = code ^ ((locname.intname[ndx])&0x00FF);

    return (code&0x7FFF) % max;
}

void quitting (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
myquit(w, tag, reason);
}
void help_proc (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
help_d0x(w, tag, reason);
}
void main_init (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
d0_init(w, tag, reason);
}
void save_text_tag (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
save_text(w, tag, reason);
}
void specify_file_type (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
cfiletype(w, tag, reason);
}
void file_open (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
fileopen(w, tag, reason);
}
void control_proc (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
control(w, tag, reason);
}
void zebra_proc (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
czebra(w, tag, reason);
}
void d0util_proc (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
cd0util(w, tag, reason);
}
void mode_proc (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
csetmode(w, tag, reason);
}
void search_type (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
csearch(w, tag, reason);
}
void interrupt (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
cinterrupt(w, tag, reason);
}
void store_type (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
cset_store(w, tag, reason);
}
void select_proc (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
cselect(w, tag, reason);
}
void format_proc (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
cformat(w, tag, reason);
}
void zeblst_proc (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
czeblst(w, tag, reason);
}
void navigate_proc (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
cnavigate(w, tag, reason);
}
void again_proc (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
cagain(w, tag, reason);
}
void set_curtop (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
csetcurtop(w, tag, reason);
}
void man_raw (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("raw_form");
}
void unman_raw (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("raw_form");
}
void decodeit (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
xdecode(w, tag, reason);
}
void rawcall (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
crawdata(w, tag, reason);
}
void out_event (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
evout(w, tag, reason);
}
void printit (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
cprintit(w, tag, reason);
}
void man_tree (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("tree_form");
}
void unman_tree (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("tree_form");
}
void unman_ztree (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
undotree(w, tag, reason);
}
void um_navc (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("nav_control");
}
void um_navz (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("nav_zeb");
}
void um_navd (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("nav_data");
}
void um_tree (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("xdbank_shell");
}
void man_navc (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("nav_control");
}
void man_navd (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("nav_data");
}
void man_navz (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("nav_zeb");
}
void man_navt (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("xdbank_shell");
}
void allout (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
csetout(w, tag, reason);
}
void manphys (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("physics_bull");
}
void unmanphys (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("physics_bull");
}
void cphysics (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
cphys(w, tag, reason);
}
void jetsel (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
cseljet(w, tag, reason);
}
void mandisp (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("disp_bull");
}
void unmandisp (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("disp_bull");
}
void selectcol (w, tag, reason)
Widget		w;
int		*tag;
XmToggleButtonCallbackStruct	*reason;
{
selectc(w, tag, reason);
}
void phioff (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
}
void allinfo (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
cinfo(w, tag, reason);
}
void manphyshelp (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("physicshelp");
}
void umphyshelp (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("physicshelp");
}
void manfiles (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("maincontrol");
}
void umfiles (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("maincontrol");
}
void eselcor (w, tag, reason)
Widget		w;
int		*tag;
XmToggleButtonCallbackStruct	*reason;
{
selecor(w, tag, reason);
}
void manout (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("output_bull");
}
void umout (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("output_bull");
}
void umlist (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("list_bull");
}
void manlist (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("list_bull");
}
void rotatephi (w, tag, reason)
Widget		w;
int		*tag;
XmArrowButtonCallbackStruct	*reason;
{
phirot(w, tag, reason);
}
void rotatetheta (w, tag, reason)
Widget		w;
int		*tag;
XmArrowButtonCallbackStruct	*reason;
{
thetarot(w, tag, reason);
}
void rotatepsi (w, tag, reason)
Widget		w;
int		*tag;
XmArrowButtonCallbackStruct	*reason;
{
psirot(w, tag, reason);
}
void manjetb (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("jet_bull");
}
void umjetb (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("jet_bull");
}
void manpb (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("pnut_bull");
}
void umpb (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("pnut_bull");
}
void manem (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("em_bull");
}
void umem (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("em_bull");
}
void treesize (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
retree(w, tag, reason);
}
void manpmuo (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("pmuo_bull");
}
void umpmuo (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("pmuo_bull");
}
void mancol (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("colbull");
}
void umcol (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("colbull");
}
void manq (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("querybull");
}
void umq (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("querybull");
}
void doquery (w, tag, reason)
Widget		w;
int		*tag;
XmListCallbackStruct	*reason;
{
d0xquery(w, tag, reason);
}
void squery (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
setquery(w, tag, reason);
}
void manzq (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("reqzeb");
}
void umzq (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("reqzeb");
}
void manqh (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("qhelp");
}
void umqh (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("qhelp");
}
void manhist (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("bullhist");
}
void umhist (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("bullhist");
}
void d0xhist (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
dxhisto(w, tag, reason);
}
void manhform (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("histform");
}
void umhform (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("histform");
}
void chdraw (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
d0xhdraw(w, tag, reason);
}
void mankunz (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("kunzb");
}
void umkunzb (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("kunzb");
}
void mantup (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("tupbull");
}
void umtup (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("tupbull");
}
void manpmc (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("physmc");
}
void umpmc (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("physmc");
}
void mandec (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("decodebull");
}
void umdec (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("decodebull");
}
void mand0x (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("d0xuserbull");
}
void umd0x (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("d0xuserbull");
}
void manqsub (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("querysubbull");
}
void umqsub (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("querysubbull");
}
void manpack (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("d0packages");
}
void umpack (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("d0packages");
}
void mand0dad (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("d0dadform");
}
void umd0dad (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("d0dadform");
}
void manpt (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("ptau_bull");
}
void umpt (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("ptau_bull");
}
void manen (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("enable_bull");
}
void umen (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("enable_bull");
}

void mantrk (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("track_bull");
}

void umtrk (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("track_bull");
}

void d0xtracks (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
d0tracks(w, tag, reason);
}

void mancal (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("cal_bull");
}

void umcal (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("cal_bull");
}

void d0xcaeh (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
d0caeh(w, tag, reason);
}
