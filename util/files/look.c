/*
 *  This is VAX/VMS fast file viewer. Author - S. Kovalyov, INP.
 *
 *  Filetype-sensitive! (distinguishes text/binary files).
 *  Wildcard is allowed!
 */

#include <iodef.h>
#include <rmsdef.h>
#include <smgdef.h>
#include <ssdef.h>
#include <fab.h>
#include <nam.h>
#include <rab.h>
#include <xab.h>
#include <smgmsg.h>
#include <smg$routines.h>
#include <lib$routines.h>
#include <descrip.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>


#define UNPRINTABLE '.'

#define VBLOCKLEN   512
#define BINRECLEN   16
#define BINTXTLEN   64
#define LONGWORDLEN 4
#define FNAMLEN	    NAM$C_MAXRSS

#define UP	    SMG$K_TRM_UP
#define DOWN	    SMG$K_TRM_DOWN
#define RIGHT	    SMG$K_TRM_RIGHT
#define LEFT	    SMG$K_TRM_LEFT

#define TAB	    SMG$K_TRM_CTRLI
#define PREV	    SMG$K_TRM_E5
#define NEXT	    SMG$K_TRM_E6

#define eNTER	    SMG$K_TRM_ENTER
#define ENTER	    SMG$K_TRM_CTRLM
#define enTER	    SMG$K_TRM_CTRLJ
#define DEL	    127
#define hELP	    SMG$K_TRM_F15
#define EXIT	    SMG$K_TRM_F10
#define BREAK	    SMG$K_TRM_CTRLZ
#define FIND	    SMG$K_TRM_FIND
#define REFRESH	    SMG$K_TRM_CTRLW
#define REREAD	    SMG$K_TRM_CTRLR

#define HLPFLLNM	"LOOK$HELP"
#define EDTFLLNM	"LOOK$EDIT"
#define HELPCOMMAND	"look view$view_hlp.view$hlp"
#define EDITCOMMAND	"ed/tpu %s"
#define DEFAULT_OUTPUT	"sys$output"
#define DEFAULT_INPUT	"sys$input"
#define HELPLINE	"helP eXit Quit Home End scrRight scrLeft Find findNxt eD Go Spawn chWid Txt/hex "
#define INVITE_FIND	"Find : "
#define INVITE_LINE	"Line number or percentage : "
#define NOTHING_FOUND	"Nothing found.  PRESS ANY KEY  "
#define WORKING_FORMAT	" %3d%% processing... (Press \'X\' to cancel) "
#define PLEASE_WAIT	" Wait please... "
#define HEADLINE_FORMAT	"File: %s  Row: %4d  Col: %3d  (%3d%%)"

#define $_ABNORMAL	-1
#define EXIT_NORMAL	2
#define NO_CLOSE	3
#define RELOOK		4

#define SCREENVER	20
#define SCREENMAX	132
#define SCREENMIN	80
#define SCREENCHNG	13
#define BORDERFIELD	0
#define LASTLINE	24
#define MAXLINE		256
#define BUFDEPTH	20
#define BUFQ		2
#define TOFIND		0
#define TOLINE		1
#define	WORKING_STEP	100
#define READKEY_STEP	10

#define CHECKRESULT(x)	{ if ((x) != SS$_NORMAL) return $_ABNORMAL; }
#define	FBEG(y)		{ if (sh_seek(vf_inf.home[y]) == -1) return $_ABNORMAL; }
#define BEEP		CHECKRESULT(smg$ring_bell(&vdisp.id))


struct 
{
    int bintxt;
    unsigned int *home;
    unsigned int current, end, all;
    struct
    {
	unsigned int pos;
	int beg;
	unsigned int rend;
	char sline[SCREENMAX + 1];
    } head, inv;
} vf_inf =
{   0,
    NULL,
    0, 0, 0,
    { SMG$K_TOP, 1, SMG$M_REVERSE, 0 },
    { SMG$K_BOTTOM, 1, SMG$M_REVERSE, 0 }
};

struct
{
    unsigned long id;
    long          firstrow, firstcol, len, wid;
    struct {long  firstcol, firstrow, len, curcol;} vport;
    struct {long  row, col;} cursor;
    char *f_spec;
} vdisp =
{
    0, 2, 1, MAXLINE, SCREENVER,
    { 1, 1, 0, 1 }, { 1, 1 },
    { '\0' }
};

struct
{
    unsigned long id;
    long firstrow, firstcol, len, wid, rend;
} k_bar =
{
    0, LASTLINE, 1, SCREENMAX, 1, SMG$M_REVERSE
};

char	line[MAXLINE + 1], no_line[MAXLINE + 1];
$DESCRIPTOR(outstr, INVITE_FIND);
$DESCRIPTOR(hlnm, HLPFLLNM);
$DESCRIPTOR(elnm, EDTFLLNM);
int wn = 0;
unsigned int	rpt = 0;
unsigned int	lnmlen = 0;
unsigned int	p_id = 0, k_id = 0;
unsigned int	screensize = 0;

struct	{ char fline[SCREENMAX + 1]; int flen; } linebuf[BUFQ][BUFDEPTH + 1];
int	fbc[BUFQ] = { 1, 1 };

unsigned int timeout	= 0;
unsigned int rend_find	= SMG$M_BOLD;
unsigned int c_off	= SMG$M_CURSOR_OFF;
unsigned int c_on	= SMG$M_CURSOR_ON;
unsigned int pscroll	= SMG$M_DOWN;
unsigned int scrright	= SMG$M_RIGHT;
unsigned int scrleft	= SMG$M_LEFT;


    /* RMS file I/O routines */

struct FAB fab_of_name, fab_of_file;
struct NAM nam_file;
struct RAB several_bytes;
struct XABFHC file_header;

int inner_ptr = VBLOCKLEN;
char v_block[VBLOCKLEN];

char res_name[FNAMLEN];
char rnm[FNAMLEN];
char outname[FNAMLEN];

int asy_stop = 0;

int v_rec_len	= 0;
int align_byte	= 0;

	/* Asynchronious operation completion indication */

void
ast_stop(void)
{
    asy_stop = 1;
}

	/* Expand file name */

char *
sh_fname(char *name_of_file)
{
    if (name_of_file != NULL)
    {
	nam_file.nam$b_bid = NAM$C_BID;
	nam_file.nam$b_bln = NAM$C_BLN;
	nam_file.nam$b_ess = FNAMLEN;
	nam_file.nam$b_esl = FNAMLEN;
	nam_file.nam$l_esa = res_name;
	nam_file.nam$b_nop = 0;
	nam_file.nam$b_rss = FNAMLEN;
	nam_file.nam$b_rsl = FNAMLEN;
	nam_file.nam$l_rsa = rnm;

	fab_of_name.fab$b_bid = FAB$C_BID; 
	fab_of_name.fab$b_bln = FAB$C_BLN; 
	fab_of_name.fab$l_nam = &nam_file;
	fab_of_name.fab$l_fna = name_of_file;
	fab_of_name.fab$b_fns = strlen(name_of_file);
	fab_of_name.fab$l_fop = FAB$M_OFP;

	fab_of_name.fab$l_sts = 0;
	asy_stop = 0;
	sys$parse(&fab_of_name, ast_stop, ast_stop);
	while(asy_stop == 0);
	if (fab_of_name.fab$l_sts != RMS$_NORMAL) return NULL;
    }

    fab_of_name.fab$w_ifi = 0;
    fab_of_name.fab$l_sts = 0;
    asy_stop = 0;
    sys$search(&fab_of_name, ast_stop, ast_stop);
    while(asy_stop == 0);
    if (fab_of_name.fab$l_sts != RMS$_NORMAL) return NULL;
    strncpy(outname, rnm, nam_file.nam$b_rsl);
    outname[nam_file.nam$b_rsl] = '\0';
    return outname;
}

	/* File name only */

char *
sh_onlyname(void)
{
    return &outname[(int)(nam_file.nam$l_name - nam_file.nam$l_rsa)];
}

	/* Open file */

int
sh_open(char *name_of_file)
{
    if (name_of_file == NULL) return -1;

    inner_ptr = VBLOCKLEN;

    fab_of_file.fab$b_bid = FAB$C_BID;
    fab_of_file.fab$b_bln = FAB$C_BLN;
    fab_of_file.fab$w_ifi = 0;
    fab_of_file.fab$b_fac = FAB$M_GET | FAB$M_BIO;
    fab_of_file.fab$l_fna = name_of_file;
    fab_of_file.fab$b_fns = strlen(name_of_file);
    fab_of_file.fab$l_fop = FAB$M_SQO | FAB$M_CIF;
    fab_of_file.fab$b_shr = FAB$M_SHRGET | FAB$M_SHRPUT | FAB$M_UPI;
    fab_of_file.fab$b_rtv = 0;
    fab_of_file.fab$l_xab = &file_header;

    file_header.xab$b_cod = XAB$C_FHC;
    file_header.xab$b_bln = XAB$C_FHCLEN;
    file_header.xab$l_nxt = 0;

    asy_stop = 0;
    fab_of_file.fab$l_sts = 0;
    sys$open(&fab_of_file, ast_stop, ast_stop);
    while(asy_stop == 0);
    if (fab_of_file.fab$l_sts != RMS$_NORMAL) return -1;

    several_bytes.rab$b_bid = RAB$C_BID;
    several_bytes.rab$b_bln = RAB$C_BLN;
    several_bytes.rab$w_isi = 0;
    several_bytes.rab$l_rop = RAB$M_RAH | RAB$M_BIO;
    several_bytes.rab$l_fab = &fab_of_file;
    several_bytes.rab$l_xab = 0;

    if (sys$connect(&several_bytes) != RMS$_NORMAL) return -1;

    several_bytes.rab$l_bkt = 0;
    several_bytes.rab$l_rop = 0;
    several_bytes.rab$w_usz = (unsigned short)VBLOCKLEN;
    several_bytes.rab$l_ubf = v_block;

    v_rec_len	= 0;
    return 0;
}

	/* Read some bytes */

int
sh_read(char *buf, int buflen)
{
    int rest_blk = VBLOCKLEN - inner_ptr;
    int actual_rest = buflen;

    while (actual_rest > rest_blk)
    {
	if (rest_blk > 0)
	    memcpy(buf + (buflen - actual_rest),
		several_bytes.rab$l_ubf + inner_ptr, (size_t)rest_blk);

	several_bytes.rab$l_bkt++;	/* Next virtual block */

	if (sys$read(&several_bytes) != RMS$_NORMAL)
	    return buflen - actual_rest;

	actual_rest -= rest_blk;
	rest_blk = VBLOCKLEN;
	inner_ptr = 0;
    }
    if (actual_rest == 0) return buflen;
    memcpy(buf + (buflen - actual_rest),
	several_bytes.rab$l_ubf + inner_ptr, (size_t)actual_rest);
    inner_ptr += actual_rest;
    return buflen;
}

	/* Calculate byte offset */

unsigned int
sh_tell(void)
{
    return (unsigned int)(several_bytes.rab$l_bkt * VBLOCKLEN
	+ inner_ptr - VBLOCKLEN);
}

	/* End of file offset */

unsigned int
sh_eof(void)
{
    return VBLOCKLEN * (file_header.xab$l_ebk - 1) + file_header.xab$w_ffb;
}

	/* Find position under offset */

int
sh_seek(unsigned int offset)
{
    if (several_bytes.rab$l_bkt != offset / VBLOCKLEN + 1)
    {
	several_bytes.rab$l_bkt = offset / VBLOCKLEN + 1;
	if (sys$read(&several_bytes) != RMS$_NORMAL) return -1;
    }
    inner_ptr = offset % VBLOCKLEN;
    return 0;
}

	/* Retrieve binary line with given length */
int
bin_fgets(char *str, int charnum, int mode)
{
    int i = 0;
    int txt_offset = charnum * 2 + LONGWORDLEN;
    int str_len = txt_offset + LONGWORDLEN + charnum + 1;
    char ch = '\0';
    char hex_ch[3];
    char *hex_ptr, *txt_ptr;

    if (mode != SS$_NORMAL)
    {
	if  (sh_read(str, charnum) != charnum) return -1;
	return 0;
    }
    memset(str, ' ', (size_t)str_len);
    hex_ptr = str + txt_offset;
    txt_ptr = hex_ptr + LONGWORDLEN;
    for (i = 0; i < BINRECLEN; i++)
    {
	if  (sh_read(&ch, 1) != 1) return -1;
	sprintf(hex_ch, "%02X", (unsigned char)ch);
	if ((i % LONGWORDLEN) == 0) hex_ptr--;
	*hex_ptr = hex_ch[1];
	hex_ptr--;
	*hex_ptr = hex_ch[0];
	hex_ptr--;
	*txt_ptr = (isprint(ch) == 0) ? '.' : ch;
	txt_ptr++;
    }
    *txt_ptr = '\0';
    return 0;
}

	/* Retrieve text line with given length */
int
txt_fgets(char *str, int charnum, int mode)
{
    int p = 0;

    if (sh_read(str, charnum) != charnum) return -1;
    if (mode == SS$_NORMAL)
    {
	for (p = 0; p < charnum; p++)
	    if (isprint(str[p]) == 0) str[p] = '.';
	str[p] = '\0';	    /* Terminator */
    }
    return 0;
}

	/* Retrieve a line considering RMS file organization */

int
sh_fgets(char *str, int maxchar, int mode)
{
    int i = 0;
    char ctrl_byte = '\0';
    char *rest_rec = NULL;

    if (maxchar <= 0) return -1;
    if (sh_tell() >= sh_eof()) return -1;
    if (vf_inf.bintxt == $_ABNORMAL) return bin_fgets(str, BINRECLEN, mode);
    switch(fab_of_file.fab$b_rfm)
    {
    case FAB$C_VAR:		    /* Usual VMS text files */
    case FAB$C_VFC:		    /* Printer format files */

	if (vf_inf.bintxt == 0) vf_inf.bintxt = SS$_NORMAL;

	if (v_rec_len == 0)
	{
	    /* Retrieve record length */

	    if (sh_read(&ctrl_byte, 1) != 1) return -1;
	    v_rec_len = (int)((unsigned char)ctrl_byte);
	    if (sh_read(&ctrl_byte, 1) != 1)
	    {
		v_rec_len = 0;
		return -1;
	    }
	    v_rec_len += ((int)((unsigned char)ctrl_byte) << 8);
	    if (v_rec_len == 0)
	    {
		*str = '\0';
		return 0;
	    }
	    if (v_rec_len == 0xFFFF)
	    {
		v_rec_len = 0;
		return sh_fgets(str, maxchar, mode);
	    }

	/* Retrieve record */

	    align_byte = v_rec_len % 2;

	    switch(fab_of_file.fab$b_rat)
	    {
	    case FAB$M_PRN:	    /* Printer file */
		if (sh_read(&ctrl_byte, 1) != 1)
		{
		    v_rec_len = 0;
		    return -1;
		}
		v_rec_len--;
		if (sh_read(&ctrl_byte, 1) != 1)
		{
		    v_rec_len = 0;
		    return -1;
		}
		v_rec_len--;
	    case FAB$M_FTN:	    /* Fortran file */
	    case FAB$M_CR:	    /* Text file */
	    default:		    /* Abstract variable-length file */
		break;
	    }
	}
	
	if (maxchar >= v_rec_len)
	{
	    if (sh_read(str, v_rec_len) != v_rec_len)
	    {
	        v_rec_len = 0;
	        return -1;
	    }
	    if (align_byte == 1)
	    {
	        if (sh_read(&ctrl_byte, 1) != 1)
		{
		    v_rec_len = 0;
		    return -1;
		}
	    }
	    if (mode == SS$_NORMAL)
	    {
	        for (i = 0; i < v_rec_len; i++)
		    if ((isprint(str[i]) == 0) && (str[i] != '\t'))
		        str[i] = UNPRINTABLE;
		str[i] = '\0';
	    }
	    v_rec_len = 0;
	    return 0;
	}
	else
	{
	    if (sh_read(str, maxchar) != maxchar)
	    {
		v_rec_len = 0;
		return -1;
	    }
	    v_rec_len -= maxchar;
	    if (mode == SS$_NORMAL)
	    {
	        for (i = 0; i < maxchar; i++)
		    if ((isprint(str[i]) == 0) && (str[i] != '\t'))
		        str[i] = UNPRINTABLE;
		str[i] = '\0';
	    }
	    return 1;
	}
    case FAB$C_STMLF:			/* Files created by C language */
        switch(fab_of_file.fab$b_rat)
        {
	case FAB$M_PRN:
	    if (sh_read(&ctrl_byte, 1) != 1) return -1;
	    if (sh_read(&ctrl_byte, 1) != 1) return -1;
	case FAB$M_FTN:
	case FAB$M_CR:
	    if (vf_inf.bintxt == 0) vf_inf.bintxt = SS$_NORMAL;
	    for (v_rec_len = 0; v_rec_len < maxchar; v_rec_len++)
	    {
		if (sh_tell() >= sh_eof()) break;
		if (sh_read(&ctrl_byte, 1) != 1) return -1;
		if (ctrl_byte == '\n')
		{
		    if (mode == SS$_NORMAL) str[v_rec_len] = '\0';
		    return 0;
		}
		if (mode == SS$_NORMAL)
		    str[v_rec_len] = ((isprint(ctrl_byte) == 0) &&
			(ctrl_byte != '\t')) ? UNPRINTABLE : ctrl_byte;
	    }
	    if (mode == SS$_NORMAL) str[v_rec_len] = '\0';
	    return 0;
	default:
	    if (vf_inf.bintxt == 0)
	    {
		vf_inf.bintxt = $_ABNORMAL;
		return bin_fgets(str, BINRECLEN, mode);
	    }
	    return txt_fgets(str, BINTXTLEN, mode);
        }
    default:			    /* Any other files are binary */
	if (vf_inf.bintxt == 0)
	{
	    vf_inf.bintxt = $_ABNORMAL;
	    return bin_fgets(str, BINRECLEN, mode);
	}
	return txt_fgets(str, BINTXTLEN, mode);
    }
}

	/* Close a file */

int
sh_close(void)
{
    asy_stop = 0;
    fab_of_file.fab$l_sts = 0;
    sys$close(&fab_of_file, ast_stop, ast_stop);
    while(asy_stop == 0);
    return (fab_of_file.fab$l_sts == RMS$_NORMAL) ? 0 : -1;
}


    /* Looker routines */

	/* Fill in a line descriptor */

struct dsc$descriptor_s *
outline(char *str)
{
    outstr.dsc$w_length	    = strlen(str);
    outstr.dsc$a_pointer    = str;
    return &outstr;
}

	/* Initialize a screen */

int
init_screen()
{
    int j = 0;

    vdisp.vport.firstcol = vdisp.vport.firstrow = vdisp.vport.curcol =
	vdisp.cursor.row = vdisp.cursor.col = 1;

    CHECKRESULT(smg$create_virtual_keyboard(&k_id));      

    CHECKRESULT(smg$create_pasteboard(&p_id, outline(DEFAULT_OUTPUT),
	&k_bar.firstrow, &screensize));
    if ((screensize < 10) || (k_bar.firstrow < 5)) return $_ABNORMAL;
    if (screensize > SCREENMAX) screensize = SCREENMAX;
    vdisp.vport.len = screensize - BORDERFIELD;
    vdisp.wid = k_bar.firstrow - 4;

    CHECKRESULT(smg$create_virtual_display(&k_bar.wid,
	&k_bar.len, &k_bar.id));
    CHECKRESULT(smg$put_line(&k_bar.id, outline(HELPLINE), &j, &k_bar.rend));

    CHECKRESULT(smg$create_virtual_display(&vdisp.wid,
	&vdisp.len, &vdisp.id));
    CHECKRESULT(smg$create_viewport(&vdisp.id,
	&vdisp.vport.firstrow, &vdisp.vport.firstcol,
	&vdisp.wid, &vdisp.vport.len));
    CHECKRESULT(smg$home_cursor(&vdisp.id));

    return SS$_NORMAL;
}

	/* Free a screen resources */

int
free_screen()
{
    CHECKRESULT(smg$delete_virtual_keyboard(&k_id));
    CHECKRESULT(smg$delete_viewport(&vdisp.id));
    CHECKRESULT(smg$delete_virtual_display(&vdisp.id));
    CHECKRESULT(smg$delete_virtual_display(&k_bar.id));
    CHECKRESULT(smg$delete_pasteboard(&p_id));
    free(vf_inf.home);
    if (sh_close() == -1) return NO_CLOSE;
    return SS$_NORMAL;
}

	/* Fill in a screen page */

int
new_page()
{
    int k = 0;

    FBEG(vf_inf.current);
    smg$begin_display_update(&vdisp.id);
    CHECKRESULT(smg$erase_display(&vdisp.id));
    CHECKRESULT(smg$home_cursor(&vdisp.id));
    for (k = 0; (k < vdisp.wid) && (sh_fgets(line, MAXLINE, SS$_NORMAL) != -1);
	k++)
        CHECKRESULT(smg$put_line(&vdisp.id, outline(line)));
    smg$end_display_update(&vdisp.id);
    return SS$_NORMAL;
}

	/* Put a work indicator */

int
working()
{
    unsigned short stop_key = 0;

    if (wn == 0) CHECKRESULT(smg$set_cursor_mode(&p_id, &c_off));
    if ((wn >= WORKING_STEP) || (wn == 0))
    {
	wn = 0;
	sprintf(vf_inf.inv.sline, WORKING_FORMAT,
	    (sh_eof() != 0) ? ((sh_tell() * 100)/sh_eof()) : 100);
	CHECKRESULT(smg$label_border(&vdisp.id, outline(vf_inf.inv.sline),
	    &vf_inf.inv.pos, &vf_inf.inv.beg, &vf_inf.inv.rend));
    }
    else if ((wn % READKEY_STEP == 0) &&
	(smg$read_keystroke(&k_id, &stop_key, NULL, &timeout) == SS$_NORMAL) &&
	((stop_key == EXIT) || (stop_key == BREAK) ||
	    (stop_key == 'X') || (stop_key == 'x')))
    {
	rpt = 0;
	CHECKRESULT(smg$set_cursor_mode(&p_id, &c_on));
	return 0;
    }
    wn++;
    return SS$_NORMAL;
}


	/* Headline a screen page & set screen cursor */

int
headline()
{
    int n = vf_inf.current + vdisp.cursor.row;

    CHECKRESULT(smg$set_cursor_abs(&vdisp.id,
	&vdisp.cursor.row, &vdisp.cursor.col));
    sprintf(vf_inf.head.sline, HEADLINE_FORMAT,
	sh_onlyname(), n, vdisp.cursor.col,
	(sh_eof() == 0) ? 100 :
	(n >= vf_inf.all-1) ? 100 : ((vf_inf.home[n-1] * 100)/sh_eof()));
    CHECKRESULT(smg$label_border(&vdisp.id, outline(vf_inf.head.sline),
	&vf_inf.head.pos, &vf_inf.head.beg, &vf_inf.head.rend));
    return SS$_NORMAL;
}

	/* Accept a line to buffer. Edition allowed */

int
get_line(int bufnum)
{
    char *invite[] = 
    {
	INVITE_FIND, INVITE_LINE
    };
    unsigned short sch = 0;
    int p = 0, q = 0, r = 0;
    int p_rows = vdisp.wid + 2;
    int p_cols = vf_inf.inv.pos + strlen(invite[bufnum]);


    if (linebuf[bufnum][fbc[bufnum]].flen != 0)
    {
	if (fbc[bufnum] == BUFDEPTH)
	{
	    for (q = 1; q < BUFDEPTH; q++)
	    {
		strcpy(linebuf[bufnum][q].fline, linebuf[bufnum][q + 1].fline);
		linebuf[bufnum][q].flen = linebuf[bufnum][q + 1].flen;
	    }
	    memset(linebuf[bufnum][fbc[bufnum]].fline, '\0', (size_t)SCREENMAX);
	    linebuf[bufnum][fbc[bufnum]].flen = 0;
	}
	else fbc[bufnum]++;
    }
    r = fbc[bufnum];
    CHECKRESULT(smg$label_border(&vdisp.id, outline(invite[bufnum]),
	&vf_inf.inv.pos, &vf_inf.inv.beg, &vf_inf.inv.rend));
    CHECKRESULT(smg$set_physical_cursor(&p_id, &p_rows, &p_cols));
    while ((sch != ENTER) && (sch != eNTER) && (p_cols < vdisp.vport.len))
    if (smg$read_keystroke(&k_id, &sch) == SS$_NORMAL)
    {
	if ((sch < (unsigned short)256) && (isprint(sch) != 0))
	{
	    p_cols++;
	    linebuf[bufnum][fbc[bufnum]].flen++;
	    for (q = linebuf[bufnum][fbc[bufnum]].flen; q > p; q--)
		linebuf[bufnum][fbc[bufnum]].fline[q] =
		    linebuf[bufnum][fbc[bufnum]].fline[q-1];
	    linebuf[bufnum][fbc[bufnum]].fline[p] = (char)sch;
	    p++;
	}
	else
	{
	    switch(sch)
	    {
	    case RIGHT:
		if (p < linebuf[bufnum][fbc[bufnum]].flen)
		{
		    p_cols++;
		    p++;
		}
		break;
	    case LEFT:
		if (p > 0)
		{
		    p_cols--;
		    p--;
		}
		break;
	    case SMG$K_TRM_CTRLH:
		p_cols = vf_inf.inv.pos + strlen(invite[bufnum]);
		p = 0;
		break;
	    case SMG$K_TRM_CTRLE:
		p_cols = vf_inf.inv.pos + strlen(vf_inf.inv.sline);
		p = linebuf[bufnum][fbc[bufnum]].flen;
		break;
	    case UP:
		if (r >= 0)
		{
		    if (r > 0) r--;
		    strcpy(linebuf[bufnum][fbc[bufnum]].fline,
			linebuf[bufnum][r].fline);
		    linebuf[bufnum][fbc[bufnum]].flen = linebuf[bufnum][r].flen;
		    p_cols -= p;
		    p = 0;
		}
		break;
	    case DOWN:
		if (r < fbc[bufnum])
		{
		    r++;
		    if (r == fbc[bufnum]) r = 0;
		    strcpy(linebuf[bufnum][fbc[bufnum]].fline,
			linebuf[bufnum][r].fline);
		    linebuf[bufnum][fbc[bufnum]].flen = linebuf[bufnum][r].flen;
		    p_cols -= p;
		    p = 0;
		    if (r == 0) r = fbc[bufnum];
		}
		break;
	    case (unsigned short)DEL:
		if (p > 0)
		{
		    for (q = p; q < linebuf[bufnum][fbc[bufnum]].flen; q++)
			linebuf[bufnum][fbc[bufnum]].fline[q-1] =
			    linebuf[bufnum][fbc[bufnum]].fline[q];
		    linebuf[bufnum][fbc[bufnum]].fline
			[--linebuf[bufnum][fbc[bufnum]].flen] =	'\0';
		    p_cols--;
		    p--;
		}
		break;
	    case SMG$K_TRM_F13:
		if (p == 0) break;
		sch = (unsigned short)linebuf[bufnum][fbc[bufnum]].fline[p-1];
		for (q = p; q < linebuf[bufnum][fbc[bufnum]].flen; q++)
		    linebuf[bufnum][fbc[bufnum]].fline[q-1] =
			linebuf[bufnum][fbc[bufnum]].fline[q];
		linebuf[bufnum][fbc[bufnum]].fline
		    [--linebuf[bufnum][fbc[bufnum]].flen] = '\0';
		p_cols--;
		p--;
		if (isalpha((int)sch) != 0)
		    while((p > 0) &&
			(isalpha((int)linebuf[bufnum][fbc[bufnum]].fline[p-1]) != 0))
		    {
			for (q = p; q < linebuf[bufnum][fbc[bufnum]].flen; q++)
			    linebuf[bufnum][fbc[bufnum]].fline[q-1] =
				linebuf[bufnum][fbc[bufnum]].fline[q];
			linebuf[bufnum][fbc[bufnum]].fline
			    [--linebuf[bufnum][fbc[bufnum]].flen] = '\0';
			p_cols--;
			p--;
		    }
		break;
	    case EXIT:
	    case BREAK:
		memset(linebuf[bufnum][fbc[bufnum]].fline, '\0',
		    (size_t)SCREENMAX);
		linebuf[bufnum][fbc[bufnum]].flen = 0;
		return SS$_NORMAL;
	    }
	}    
	sprintf(vf_inf.inv.sline, "%s%s", invite[bufnum],
	    linebuf[bufnum][fbc[bufnum]].fline);
	CHECKRESULT(smg$label_border(&vdisp.id, outline(vf_inf.inv.sline), 
	    &vf_inf.inv.pos, &vf_inf.inv.beg, &vf_inf.inv.rend));
	CHECKRESULT(smg$set_physical_cursor(&p_id, &p_rows, &p_cols));
    }
    return SS$_NORMAL;
}

	/* Find a template in line */

int
find_in_line()
{
    int s = 0, k = 0, lnlin = 0;

    no_line[MAXLINE] = '\0';
    for (lnlin = strlen(no_line) - linebuf[TOFIND][fbc[TOFIND]].flen;
	s <= lnlin; s++)
    {
	for (k = 0;
	    (tolower(no_line[s + k]) ==
		tolower(linebuf[TOFIND][fbc[TOFIND]].fline[k])) && 
	    (k < linebuf[TOFIND][fbc[TOFIND]].flen); k++);
	if (k < linebuf[TOFIND][fbc[TOFIND]].flen) s += k;
	else return s;
    }
    return $_ABNORMAL;
}

	/* Find once again */

int
nextfind()
{
    int findbeg = 0, found = 0;
    unsigned int cur_find = 0;
    unsigned short any_key = 0;

    if (rpt == 0)
    {
	if ((vf_inf.end == 0) && (vdisp.cursor.row >= vf_inf.all))
	{
	    BEEP;
	    return SS$_NORMAL;
	}
	vdisp.cursor.col++;
	if (vdisp.cursor.col < MAXLINE)
	{
	    memset(no_line, 'A', (size_t)MAXLINE);
	    no_line[MAXLINE] = '\0';
	    CHECKRESULT(smg$set_cursor_abs(&vdisp.id, 
		&vdisp.cursor.row, &vdisp.cursor.col));
	    CHECKRESULT(smg$read_from_display(&vdisp.id, outline(no_line)));
	    if ((found = find_in_line()) != $_ABNORMAL)
	    {
		CHECKRESULT(smg$begin_display_update(&vdisp.id));
		any_key = (unsigned short)no_line[found];
		no_line[found] = '\0';
		CHECKRESULT(new_page());
		CHECKRESULT(smg$put_chars(&vdisp.id, outline(no_line),
		    &vdisp.cursor.row, &vdisp.cursor.col));
		CHECKRESULT(smg$return_cursor_pos(&vdisp.id,
		    &vdisp.cursor.row, &vdisp.cursor.col));
		for (findbeg = 1; findbeg <= vdisp.cursor.col;
		    findbeg += (vdisp.vport.len - 1))
		CHECKRESULT(smg$change_viewport(&vdisp.id,
		    &vdisp.vport.firstrow, &findbeg));
		vdisp.vport.curcol = vdisp.cursor.col -findbeg +vdisp.vport.len;
		findbeg = 0;
		no_line[found] = (char)any_key;
		no_line[found + linebuf[TOFIND][fbc[TOFIND]].flen] = '\0';
		CHECKRESULT(smg$put_chars(&vdisp.id, outline(&no_line[found]),
		    &vdisp.cursor.row, &vdisp.cursor.col, &findbeg, &rend_find));
		CHECKRESULT(smg$end_display_update(&vdisp.id));
		return SS$_NORMAL;
	    }
	}
	cur_find = vf_inf.current + vdisp.cursor.row;
	if (((vf_inf.end != $_ABNORMAL) &&
	    ((int)(cur_find - vf_inf.end) >= vdisp.wid)) ||
	    ((vf_inf.end == 0) &&
	    (vdisp.cursor.row == vf_inf.all - 1)))
	{
	    vdisp.cursor.col--;
	    BEEP;
	    return SS$_NORMAL;
	}
	wn = 0;
    }
    else cur_find = rpt;

    while (cur_find < vf_inf.all - 1)
    {
	if ((vf_inf.end == $_ABNORMAL) && (cur_find + vdisp.wid >= vf_inf.all))
	{
	    rpt = cur_find;
	    return SS$_NORMAL;
	}
	FBEG(cur_find);
	switch(working())
	{
	case 0:
	    vdisp.cursor.col--;
	    return SS$_NORMAL;
	case $_ABNORMAL:
	    return $_ABNORMAL;
	}
	if (sh_fgets(no_line, MAXLINE, SS$_NORMAL) == -1) return $_ABNORMAL;
        found = find_in_line();
	if (found != $_ABNORMAL) break;
	cur_find++;
    }
    rpt = 0;
    if (found == $_ABNORMAL)
    {
        CHECKRESULT(smg$label_border(&vdisp.id, 
	    outline(NOTHING_FOUND),
	    &vf_inf.inv.pos, &vf_inf.inv.beg, &vf_inf.inv.rend));
	BEEP;
	smg$read_keystroke(&k_id, &any_key);
	CHECKRESULT(smg$set_cursor_mode(&p_id, &c_on));
	vdisp.cursor.col--;    
	return SS$_NORMAL;
    }
    CHECKRESULT(smg$begin_display_update(&vdisp.id));
    if (cur_find - vf_inf.current >= vdisp.wid)
    {
	if ((vf_inf.end != $_ABNORMAL) && (cur_find > vf_inf.end))
	    vf_inf.current = vf_inf.end;
	else if (cur_find <= vdisp.wid/2)
	    vf_inf.current = 0;
	else vf_inf.current = cur_find - vdisp.wid/2;
    }
    CHECKRESULT(new_page());
    vdisp.cursor.row = cur_find - vf_inf.current + 1;
    vdisp.cursor.col = 1;
    any_key = (unsigned short)no_line[found];
    no_line[found] = '\0';
    CHECKRESULT(smg$set_cursor_mode(&p_id, &c_on));
    CHECKRESULT(smg$put_chars(&vdisp.id, outline(no_line),
	&vdisp.cursor.row, &vdisp.cursor.col));
    CHECKRESULT(smg$return_cursor_pos(&vdisp.id,
	&vdisp.cursor.row, &vdisp.cursor.col));
    for (findbeg = 1; findbeg <= vdisp.cursor.col;
	findbeg += (vdisp.vport.len - 1))
	CHECKRESULT(smg$change_viewport(&vdisp.id,
	    &vdisp.vport.firstrow, &findbeg));
    vdisp.vport.curcol = vdisp.cursor.col - findbeg + vdisp.vport.len;
    findbeg = 0;
    no_line[found] = (char)any_key;
    no_line[found + linebuf[TOFIND][fbc[TOFIND]].flen] = '\0';
    CHECKRESULT(smg$put_chars(&vdisp.id, outline(&no_line[found]),
	&vdisp.cursor.row, &vdisp.cursor.col, &findbeg, &rend_find));
    CHECKRESULT(smg$end_display_update(&vdisp.id));
    return SS$_NORMAL;
}

	/* Go to position by line or percentage */

int
go_line()
{
    unsigned int ln = 0, p = 0;
    char *pch = NULL;

    ln = strtoul(linebuf[TOLINE][fbc[TOLINE]].fline, &pch, 0);
    vf_inf.current = 0;
    if (pch == linebuf[TOLINE][fbc[TOLINE]].fline) BEEP
    else
    {
	if (*pch == '%')
	{
	    p = (ln * sh_eof())/100;
	    if ((vf_inf.end == $_ABNORMAL) && (vf_inf.home[vf_inf.all] < p))
	    {
	        rpt = $_ABNORMAL;
	        return SS$_NORMAL;
	    }
	    for (ln = 0; (ln < vf_inf.all) && (vf_inf.home[ln] < p); ln++);
	    ln++;
	}

	if (ln == 0)
	{
	    BEEP;
	    ln = 1;
	}

	if ((vf_inf.end == $_ABNORMAL) && (ln + vdisp.wid >= vf_inf.all))
	{
	    rpt = $_ABNORMAL;
	    return SS$_NORMAL;
	}

	CHECKRESULT(smg$set_cursor_mode(&p_id, &c_on));

	if (ln >= vf_inf.all)
	{
	    BEEP;
	    ln = vf_inf.all - 1;
	}

	if (ln <= vdisp.wid/2) vdisp.cursor.row = ln;
	else
	{
	    vf_inf.current = ln - vdisp.wid/2;
	    vdisp.cursor.row = vdisp.wid/2;
	    if ((vf_inf.end != $_ABNORMAL) && (vf_inf.current > vf_inf.end))
	    {
	        vdisp.cursor.row += (vf_inf.current - vf_inf.end);
		vf_inf.current = vf_inf.end;
	    }
	}
	CHECKRESULT(new_page());
    }
    rpt = 0;
}

	/* Key processing */

int
key_processing(unsigned short ch)
{
    unsigned int l = 0;

	switch(ch)
	{
	case 0:
	    return SS$_NORMAL;
	case UP:
	    if (vdisp.cursor.row == 1)
	    {
		if (vf_inf.current != 0)
		{
		    vf_inf.current--;
		    FBEG(vf_inf.current);
		    l = 1;
		    CHECKRESULT(smg$begin_display_update(&vdisp.id));
		    CHECKRESULT(smg$set_cursor_abs(&vdisp.id, 
			&vdisp.cursor.row, &l));
		    outstr.dsc$w_length = MAXLINE;
		    outstr.dsc$a_pointer = no_line;
		    CHECKRESULT(smg$read_from_display(&vdisp.id, &outstr));
		    if (sh_fgets(line, MAXLINE, SS$_NORMAL) == -1)
			return $_ABNORMAL;
		    l = 0;
		    CHECKRESULT(smg$put_line(&vdisp.id, outline(line), &l));
		    l = 2;	
		    CHECKRESULT(smg$insert_line(&vdisp.id, &l, 
			outline(no_line), &pscroll));
		    CHECKRESULT(smg$end_display_update(&vdisp.id));
		}
		else BEEP;
	    }
	    else vdisp.cursor.row--;
	    return SS$_NORMAL;
	case DOWN:
	    if (vdisp.cursor.row == vdisp.wid)
	    {
		if ((vf_inf.end == $_ABNORMAL) &&
		    (vf_inf.current + vdisp.wid + 1 >= vf_inf.all))
		{
		    rpt = $_ABNORMAL;
		    return SS$_NORMAL;
		}
		if (vf_inf.current != vf_inf.end)
		{
		    l = 1;
		    FBEG(vf_inf.current + vdisp.wid);
		    CHECKRESULT(smg$begin_display_update(&vdisp.id));
		    CHECKRESULT(smg$set_cursor_abs(&vdisp.id, 
			&vdisp.cursor.row, &l));
		    outstr.dsc$w_length = MAXLINE;
		    outstr.dsc$a_pointer = no_line;
		    CHECKRESULT(smg$read_from_display(&vdisp.id, &outstr));
		    if (sh_fgets(line, MAXLINE, SS$_NORMAL) == -1)
			return $_ABNORMAL;
		    l = 0;
		    CHECKRESULT(smg$put_line(&vdisp.id, outline(line), &l));
		    l = vdisp.wid - 1;
		    CHECKRESULT(smg$insert_line(&vdisp.id, &l,
			outline(no_line)));
		    vf_inf.current++;
		    CHECKRESULT(smg$end_display_update(&vdisp.id));
		}
		else BEEP;
	    }
	    else vdisp.cursor.row++;
	    rpt = 0;
	    return SS$_NORMAL;
	case LEFT:
	    if (vdisp.cursor.col > 1)
	    {
		if (vdisp.vport.curcol > 1) vdisp.vport.curcol--;
		else CHECKRESULT(smg$scroll_viewport(&vdisp.id, &scrright));
		vdisp.cursor.col--;
	    }
	    else BEEP;
	    return SS$_NORMAL;
	case RIGHT:
	    if (vdisp.cursor.col < vdisp.len)
	    {
		if (vdisp.vport.curcol < vdisp.vport.len) vdisp.vport.curcol++;
		else CHECKRESULT(smg$scroll_viewport(&vdisp.id, &scrleft));
		vdisp.cursor.col++;
	    }
	    else BEEP;
	    return SS$_NORMAL;
	case 'L':
	case 'l':
	    if (vdisp.vport.curcol > 1)
	    {
		vdisp.cursor.col -= (vdisp.vport.curcol - 1);
		vdisp.vport.curcol = 1;
	    }
	    else if (vdisp.cursor.col > vdisp.vport.len)
	    {
		l = vdisp.vport.len - 1;
	        CHECKRESULT(smg$scroll_viewport(&vdisp.id,
		    &scrright, &l));
		vdisp.cursor.col -= (vdisp.vport.len - 1);
	    }
	    else if (vdisp.cursor.col > 1)
	    {
		CHECKRESULT(smg$change_viewport(&vdisp.id,
		    &vdisp.vport.firstrow, &vdisp.vport.firstcol));
		vdisp.cursor.col = 1;
	    }	
	    else BEEP;
	    return SS$_NORMAL;
	case 'R':
	case 'r':
	    if (vdisp.vport.curcol < vdisp.vport.len)
	    {
		vdisp.cursor.col += (vdisp.vport.len - vdisp.vport.curcol);
		vdisp.vport.curcol = vdisp.vport.len;
	    }
	    else if (vdisp.cursor.col < (vdisp.len - 2 * vdisp.vport.len))
	    {
		l = vdisp.vport.len - 1;
	        CHECKRESULT(smg$scroll_viewport(&vdisp.id, 
		    &scrleft, &l));
		vdisp.cursor.col += (vdisp.vport.len - 1);
	    }
	    else if(vdisp.cursor.col < vdisp.len)
	    {
		l = vdisp.len - vdisp.vport.len + 1;
		CHECKRESULT(smg$change_viewport(&vdisp.id,
		    &vdisp.vport.firstrow, &l));
		vdisp.cursor.col = vdisp.len;
	    }	
	    else BEEP;
	    return SS$_NORMAL;
	case PREV:
	    if (vf_inf.current == 0) BEEP
	    else
	    {
		if (vf_inf.current >= vdisp.wid)
		    vf_inf.current -= (vdisp.wid - 1);
		else vf_inf.current = 0;
		CHECKRESULT(new_page());
	    }
	    return SS$_NORMAL;
	case NEXT:
	    if ((vf_inf.end == $_ABNORMAL) &&
	        (vf_inf.current + 2*vdisp.wid >= vf_inf.all))
		{
		    wn = WORKING_STEP;
		    rpt = $_ABNORMAL;
		    return SS$_NORMAL;
		}
	    if (vf_inf.current != vf_inf.end)
	    {
		if ((vf_inf.end == $_ABNORMAL) ||
		    (vf_inf.end - vf_inf.current >= vdisp.wid))
		    vf_inf.current += (vdisp.wid - 1);
		else vf_inf.current = vf_inf.end;
		CHECKRESULT(new_page());
	    }
	    else BEEP;
	    rpt = 0;
	    return SS$_NORMAL;
	case 'H':
	case 'h':
	    if ((vf_inf.current == 0) &&
		(vdisp.vport.curcol == 1) &&
		(vdisp.cursor.row == 1) &&
		(vdisp.cursor.col == 1)) BEEP
	    else
	    {	
		vdisp.vport.curcol = 1;
		vdisp.cursor.row = vdisp.cursor.col = 1;
		CHECKRESULT(smg$begin_display_update(&vdisp.id));
		if (vf_inf.current != 0)
		{
		    vf_inf.current = 0;
		    CHECKRESULT(new_page());
		}
		CHECKRESULT(smg$change_viewport(&vdisp.id,
		    &vdisp.vport.firstrow, &vdisp.vport.firstcol));
		CHECKRESULT(smg$end_display_update(&vdisp.id));
	    }
	    return SS$_NORMAL;
	case 'E':
	case 'e':
	    if (vf_inf.end == $_ABNORMAL)
	    {
	        rpt = $_ABNORMAL;
	        return SS$_NORMAL;
	    }
	    if ((vf_inf.current == vf_inf.end) &&
		(vdisp.vport.curcol == 1) &&
		(vdisp.cursor.col == 1) &&
		(vdisp.cursor.row == vdisp.wid)) BEEP
	    else
	    {	
		vdisp.vport.curcol = 1;
		vdisp.cursor.row = vdisp.wid;
		vdisp.cursor.col = 1;
		CHECKRESULT(smg$begin_display_update(&vdisp.id));
		if (vf_inf.current != vf_inf.end)
		{
		    vf_inf.current = vf_inf.end;
		    CHECKRESULT(new_page());
		}
		CHECKRESULT(smg$change_viewport(&vdisp.id,
		    &vdisp.vport.firstrow, &vdisp.vport.firstcol));
		CHECKRESULT(smg$end_display_update(&vdisp.id));
	    }
	    CHECKRESULT(smg$set_cursor_mode(&p_id, &c_on));
	    rpt = 0;
	    return SS$_NORMAL;
	case FIND:
	case 'F':
	case 'f':
	    if (rpt == 0) CHECKRESULT(get_line(TOFIND));
	case 'N':
	case 'n':
	    if (linebuf[TOFIND][fbc[TOFIND]].flen != 0)	return nextfind();
	    else BEEP;
	    return SS$_NORMAL;
	case 'G':
	case 'g':
	    if (rpt == 0) CHECKRESULT(get_line(TOLINE));
	    if (linebuf[TOLINE][fbc[TOLINE]].flen != 0) return go_line();
	    else BEEP;
	    return SS$_NORMAL;
	case 'W':
	case 'w':
	    screensize = (screensize == SCREENMAX) ? SCREENMIN : SCREENMAX;
	    CHECKRESULT(smg$unpaste_virtual_display(&vdisp.id, &p_id));
	    CHECKRESULT(smg$change_pbd_characteristics(&p_id,
	        &screensize, &l));
	    if (screensize == l)
	    {
	        vdisp.vport.len = screensize - BORDERFIELD;
		l = vdisp.cursor.col - vdisp.vport.curcol + 1;
		if (l > (vdisp.len - vdisp.vport.len + 1))
		    l = vdisp.len - vdisp.vport.len + 1;
		CHECKRESULT(smg$change_viewport(&vdisp.id, 0, &l,
		    &vdisp.wid, &vdisp.vport.len));
		if (vdisp.vport.curcol > vdisp.vport.len)
		{
		    vdisp.cursor.col -= (vdisp.vport.curcol - vdisp.vport.len);
		    vdisp.vport.curcol = vdisp.vport.len;
		}
		else vdisp.vport.curcol = vdisp.cursor.col - l + 1;
	    }
	    else BEEP;
	    CHECKRESULT(smg$paste_virtual_display(&vdisp.id, &p_id,
	        &vdisp.firstrow, &vdisp.firstcol));
	    return SS$_NORMAL;
	case 'D':
	case 'd':
	    CHECKRESULT(smg$label_border(&vdisp.id, outline(PLEASE_WAIT),
		&vf_inf.inv.pos, &vf_inf.inv.beg, &vf_inf.inv.rend));
	    memset(no_line, 'A', (size_t)SCREENMAX);
	    no_line[SCREENMAX] = '\0';
	    if (lib$sys_trnlog(&elnm, &lnmlen, outline(no_line))
		!= SS$_NORMAL) strcpy(no_line, EDITCOMMAND);
	    sprintf(line, no_line,
		vdisp.f_spec, vf_inf.current + vdisp.cursor.row);
	    return (lib$spawn(outline(line)) == SS$_NORMAL)
		? EXIT_NORMAL : RELOOK;
	case 'P':
	case 'p':
	case hELP:
	    memset(line, 'A', (size_t)SCREENMAX);
	    line[SCREENMAX] = '\0';
	    if (lib$sys_trnlog(&hlnm, &lnmlen, outline(line))
		!= SS$_NORMAL) strcpy(line, HELPCOMMAND);
	case 'S':
	case 's':
	    CHECKRESULT(smg$label_border(&vdisp.id, outline(PLEASE_WAIT),
		&vf_inf.inv.pos, &vf_inf.inv.beg, &vf_inf.inv.rend));
	    CHECKRESULT(smg$set_physical_cursor(&p_id, &k_bar.firstrow,
		&k_bar.firstcol));
	    if ((ch == 'S') || (ch == 's'))
		CHECKRESULT(lib$spawn(0))
	    else CHECKRESULT(lib$spawn(outline(line)))
	case REFRESH:
	    CHECKRESULT(smg$unpaste_virtual_display(&vdisp.id, &p_id));
	    CHECKRESULT(smg$unpaste_virtual_display(&k_bar.id, &p_id));
	    CHECKRESULT(smg$erase_pasteboard(&p_id));
	    CHECKRESULT(smg$paste_virtual_display(&k_bar.id, &p_id,
		&k_bar.firstrow, &k_bar.firstcol));
	    CHECKRESULT(new_page());
	    CHECKRESULT(smg$paste_virtual_display(&vdisp.id, &p_id,
		&vdisp.firstrow, &vdisp.firstcol));
	    return SS$_NORMAL;
	case 'T':
	case 't':
	case '/':
	    vf_inf.bintxt = (vf_inf.bintxt == SS$_NORMAL)
		? $_ABNORMAL : SS$_NORMAL;
	case REREAD:
	    return RELOOK;
	case EXIT:
	case BREAK:
	case 'X':
	case 'x':
	    return EXIT_NORMAL;
	case 'Q':
	case 'q':
	    return 0;
	}
    return SS$_NORMAL;
}

	/* First acquaintance */

int
get_acquaintance(unsigned short *key)
{
    int key_reply = SS$_NORMAL;
    int	mem_all = vdisp.wid + 5;

    memset(line, '\0', (size_t)MAXLINE);
    memset(no_line, '\0', (size_t)MAXLINE);
    rpt	    = 0;
    *key    = 0;
    wn	    = 0;

    vf_inf.home	    = (int *)malloc((size_t)(mem_all * sizeof(int)));
    vf_inf.current  = vf_inf.end = vf_inf.all = 0;

    while(vf_inf.all < vdisp.wid)
    {
	vf_inf.home[vf_inf.all] = sh_tell();
	vf_inf.all++;
	if (sh_fgets(line, MAXLINE, SS$_NORMAL) == -1) break;
        CHECKRESULT(smg$put_line(&vdisp.id, outline(line)));
    }
    CHECKRESULT(smg$paste_virtual_display(&vdisp.id, &p_id,
	&vdisp.firstrow, &vdisp.firstcol));
    CHECKRESULT(smg$paste_virtual_display(&k_bar.id, &p_id,
	&k_bar.firstrow, &k_bar.firstcol));
    if (vf_inf.all < vdisp.wid) return SS$_NORMAL;

    CHECKRESULT(headline());
    vf_inf.end = $_ABNORMAL;

    do
    {
	if (vf_inf.all >= mem_all)
	{
	    mem_all *= 2;
	    vf_inf.home = (int *)realloc(vf_inf.home,
	        (size_t)(mem_all * sizeof(int)));
	}
	vf_inf.home[vf_inf.all] = sh_tell();

	    if (rpt != 0)
	    {
		if (rpt == $_ABNORMAL) key_reply = working();
		if (vf_inf.all % READKEY_STEP == 0)
		{
		    if (key_reply == SS$_NORMAL)
			key_reply = key_processing(*key);
		    else if (key_reply == 0) key_reply = SS$_NORMAL;
		    if (key_reply != SS$_NORMAL) return key_reply;
		    sh_seek(vf_inf.home[vf_inf.all]);
		}
		if (rpt == 0) CHECKRESULT(headline());
	    }
	    else if ((vf_inf.all % READKEY_STEP == 0) &&
		(smg$read_keystroke(&k_id, key, NULL, &timeout) == SS$_NORMAL))
	    {
		wn = 0;
		key_reply = key_processing(*key);
		if (key_reply != SS$_NORMAL) return key_reply;
		sh_seek(vf_inf.home[vf_inf.all]);
		if (rpt == 0) CHECKRESULT(headline());
	    }

	vf_inf.all++;
    }
    while(sh_fgets(line, MAXLINE, $_ABNORMAL) != -1);

    vf_inf.end = vf_inf.all - vdisp.wid - 1;
    return (rpt != 0) ? key_processing(*key) : SS$_NORMAL;
}

	/* Main view routine */

int
view_manager(char file_name_to_view[])
{
    unsigned short svch = 0;
    int num = 0;

    if (file_name_to_view[0] == '?')
    {
	putchar('\n');
	memset(line, 'A', (size_t)MAXLINE);
	line[MAXLINE] = '\0';
	if (lib$sys_trnlog(&hlnm, &lnmlen, outline(line))
	    != SS$_NORMAL) strcpy(line, HELPCOMMAND);
	lib$do_command(outline(line));
	exit(SS$_NORMAL);
    }

    vdisp.f_spec = sh_fname(file_name_to_view);
    if (vdisp.f_spec == NULL)
        printf("\n%%LOOK-F-FFERR, Failed to find file %s.\n", file_name_to_view);
    else
    do
	if (sh_open(vdisp.f_spec) != -1)
	{
	    num = (init_screen() == SS$_NORMAL)
		? get_acquaintance(&svch) : $_ABNORMAL;

	    while (num == SS$_NORMAL)
		if (headline() == SS$_NORMAL)
		{
		    if (smg$read_keystroke(&k_id, &svch) == SS$_NORMAL)
			num = key_processing(svch);
		}
		else num = $_ABNORMAL;

	    if (num != $_ABNORMAL)
		switch(free_screen())
		{
		case $_ABNORMAL:    num = $_ABNORMAL;	break;
		case NO_CLOSE:	    num = NO_CLOSE;	break;
		}

	    switch(num)
	    {
	    case $_ABNORMAL:
		printf("\n%%LOOK-F-LKERR, Failed to use screen.");
	    case 0:
		exit(SS$_NORMAL);
	    case RELOOK:
		break;
	    case NO_CLOSE:
		printf("\n%%LOOK-F-FCERR, Failed to close file %s.\n",
		    vdisp.f_spec);
	    default:
		vdisp.f_spec = sh_fname(NULL);
		vf_inf.bintxt = 0;
	    }
	}
	else
	{
	    printf("\n%%LOOK-F-FOERR, Failed to open file %s.\n", vdisp.f_spec);
	    vdisp.f_spec = sh_fname(NULL);
	}
    while(vdisp.f_spec != NULL);
}

	/* Now, real MAIN */

main(int arguments, char *parameter[])
{
    unsigned char buzz[] =
    {
        '\50',
        '\224',
        '\102',
        '\235',
        '\123',
        '\225',
        '\123',
        '\236',
        '\101',
        '\334',
        '\132',
        '\205',
        '\51',
        '\263',
        '\76',
        '\246',
        '\74',
        '\343',
        '\127',
        '\307',
        '\116',
        '\320',
        '\103',
        '\305',
        '\125',
        '\334',
        '\17',
        '\320',
        '\146',
        '\327',
        '\170',
        '\253',
        '\164',
        '\305',
        '\125',
        '\334',
        '\114',
        '\300',
        '\126',
        '\313',
        '\135',
        '\320',
        '\134',
        '\310',
        '\31',
        '\0'
    };
    char a_file_name[FNAMLEN] = "";
    int i = 0;
    char *b_file_name = "Fast file viewer V5.27.";

    for (i = strlen(buzz) - 1; i > 0; i--) buzz[i] = (~buzz[i]) ^ buzz[i-1];
    if (arguments > 1)
    {
	printf("%s%s", b_file_name, buzz);
	for (i = 1; i < arguments; i++) view_manager(parameter[i]);
    }
    else
    {
	do
	{
	    printf("_File(s) : ");
	    gets(a_file_name);
	    putchar('\n');
	} while(a_file_name[0] == '\0');

	printf("%s%s", b_file_name, buzz);
	for (b_file_name = strtok(a_file_name, " "); b_file_name != NULL;
	    b_file_name = strtok(NULL, " ")) view_manager(b_file_name);
    }
}
