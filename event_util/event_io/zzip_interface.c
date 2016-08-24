/* zzip_interface.c -- interfaces between fortrash, zebra, and zzip.
 *
 * 10/93 sss
 *
 * Exported routines:
 *
 *  integer function zzip_open (lun, pack_level, recl, fname)
 *    integer lun, pack_level, recl
 *    character*(*) fname
 *
 *    Open the file FNAME for writing compressed data through Zebra unit LUN.
 *    PACK_LEVEL should be a integer in the range 1-9 specifying
 *    by how much to compress.  (Large numbers compress further, but
 *    the time required for compression also increases.)
 *    RECL is the Zebra physical record length, in longwords.
 *
 *    Returns 0 on success; nonzero for failures.  After a failure,
 *    zzip_get_last_error may be used to get more information about
 *    the error.
 *
 *    After zzip_open completes successfully, one can do FZOUTs
 *    on LUN as usual.  The data will be automatically compressed and
 *    written to the output file.
 *
 *    Note that the caller should _not_ call FZFILE on a unit opened
 *    with zzip_open.
 *
 *  subroutine zzip_close (lun)
 *    integer lun
 *
 *    Close the zzip file open through Zebra unit LUN.  The file must have
 *    been originally opened with zzip_open.  This routine must be used
 *    to close zzip files being written; otherwise, data may be lost.
 *
 *
 *  integer function zunzip_open (lun, fname)
 *    integer lun
 *    character*(*) fname
 *
 *    Open the compressed file FNAME for reading through Zebra unit LUN.
 *    Returns 0 on success; nonzero for failures.  After a failure,
 *    zzip_get_last_error may be used to get more information about
 *    the error.
 *
 *    After zunzip_open completes successfully, one can do FZINs
 *    on LUN as usual.  The data will be automatically read from the
 *    input file and uncompressed.  FZINXT should work as well.
 *
 *    Note that the caller should _not_ call FZFILE on a unit opened
 *    with zunzip_open.
 *
 *  subroutine zunzip_close (lun)
 *    integer lun
 *
 *    Close the zzip file open through Zebra unit LUN.  The file must have
 *    been originally opened with zunzip_open.  This routine should be used
 *    to close zzip files being read; otherwise, memory allocated by
 *    zzip will not be freed.
 *
 *
 *  subroutine zzip_get_last_err (errstr)
 *    character*(*) errstr
 *
 *    Returns in ERRSTR a readble string describing the last error.
 *
 *
 * Modified 16-MAR-1995 sss - avoid link warning with decc.
 */

#include <stdlib.h>
#include <errno.h>
#include <unistd.h>

#if defined(VMS) && !defined(__GNUC__)
# include <unixio.h>
#endif

#if defined(VMS) && defined(__GNUC__)
 extern char *strerror (int errnum, ...);
#endif

#ifdef VMS
#define ZZIP_INC "d0$c_inc:zzip.h"
#else
#define ZZIP_INC "zzip.h"
#endif
#include ZZIP_INC


/******************************************************************************
 * Utilities for fortrash/C communication.
 */

#ifdef VMS
# include <descrip.h>
# define D0FSTR_DECL(name) struct dsc$descriptor_s *name
# define D0FSTR_BODY(name) ((name)->dsc$a_pointer)
# define D0FSTR_LENGTH(name) ((name)->dsc$w_length)
# define D0FNAME(name) name

 static struct dsc$descriptor_s _fpass_cstring_tmp = {DSC$K_DTYPE_T,
						     DSC$K_CLASS_S,
				 		     0, 0};
# define D0FPASS_CSTRING(s) (_fpass_cstring_tmp.dsc$w_length = strlen (s),  \
	 	  	     _fpass_cstring_tmp.dsc$a_pointer = (s),        \
		 	     &_fpass_cstring_tmp)

#else

# if defined(__STDC__) || defined(__EXTENSIONS__)
#  define D0FSTR_DECL(name) char * name##body, int name##length
#  define D0FSTR_BODY(name) name##body
#  define D0FSTR_LENGTH(name) name##length
#  define D0FNAME(name) name##_
# else
#  define D0FSTR_DECL(name) char * name/**/body, int name/**/length
#  define D0FSTR_BODY(name) name/**/body
#  define D0FSTR_LENGTH(name) name/**/length
#  define D0FNAME(name) name/**/_
# endif

# define D0FPASS_CSTRING(s) s, strlen (s)

# ifdef AIX
#  undef D0FNAME
#  define D0FNAME(name) name
# endif

#endif


static char *fstring_to_cstring (const char *fstring_body, int fstring_length)
{
  char *s;
  int trulen;

  if (fstring_body == NULL)
    return NULL;

  trulen = fstring_length;
  while (trulen > 0 && fstring_body[trulen-1] == ' ')
    --trulen;

  s = (char *) xmalloc (trulen+1);
  strncpy (s, fstring_body, trulen);
  s[trulen] = '\0';

  return s;
}


static void cstring_to_fstring (const char *cstring,
				char *fstring_body, int fstring_length)
{
  if (fstring_body == NULL)
    return;

  if (cstring == NULL)
    memset (fstring_body, ' ', fstring_length);
  else {
    int len = strlen (cstring);
    if (len > fstring_length)
      len = fstring_length;
    memcpy (fstring_body, cstring, len);
    memset (fstring_body+len, ' ', fstring_length-len);
  }
}


/******************************************************************************
 * Manage the association between zebra LUNs and zzip descriptors.
 */

/* Keep an array giving the lun - desc correspondence.
   A zero lun indicates a free slot. */

struct lun_desc {
  int lun;
  void *desc;
  int last_recno;
};

static struct lun_desc *lun_desc_table = NULL;
static int lun_desc_table_size = 0;


/* Return a pointer to the lun_desc struct for lun LUN.
   If one does not exist, a new slot is allocated for it. */

static struct lun_desc *find_lun_desc (int lun)
{
  int i, free_index = -1;

  if (lun == 0)
    error ("find_lun_desc: zero lun");

  /* Scan the table looking for LUN.
     Also remember any free slots we see, in case we don't find it. */
  for (i=0; i<lun_desc_table_size; i++) {
    if (lun_desc_table[i].lun == lun)
      return lun_desc_table + i;
    else if (lun_desc_table[i].lun == 0)
      free_index = i;
  }

  /* If there were no free slots, expand the table. */
  if (free_index < 0) {
    int newsize = lun_desc_table_size * 2 + 5;
    struct lun_desc *p = (struct lun_desc *)xmalloc (newsize *
						     sizeof (struct lun_desc));
    memcpy (p, lun_desc_table, lun_desc_table_size * sizeof (struct lun_desc));
    free_index = lun_desc_table_size;
    if (lun_desc_table != NULL) free (lun_desc_table);
    lun_desc_table_size = newsize;
    lun_desc_table = p;
  }

  lun_desc_table[free_index].lun = lun;
  return lun_desc_table + free_index;
}


/* Record a LUN - DESC correspondence. */

static void register_lun_desc (int lun, void *desc)
{
  struct lun_desc *p = find_lun_desc (lun);
  p->desc = desc;
  p->last_recno = 0;
}


/* Free the slot occupied by LUN. */

static void free_lun_desc (int lun)
{
  struct lun_desc *p = find_lun_desc (lun);
  p->lun = 0;
  p->desc = 0;
}


/* Return the desc for LUN. */

static void *desc_for_lun (int lun)
{
  struct lun_desc *p = find_lun_desc (lun);
  if (p->desc == NULL)
    error ("desc_for_lun: unknown lun");
  return p->desc;
}


/******************************************************************************
 * Zebra declarations.
 */

#ifdef __DECC
#pragma extern_model save
# ifdef __VAX
#  pragma extern_model common_block shr
# else /* alpha */
#  pragma extern_model common_block noshr
# endif
#endif

extern int D0FNAME(quest)[1];

#ifdef __DECC
#pragma extern_model restore
#endif


static int *iquest = D0FNAME(quest);

extern void D0FNAME(fzfile) ();
extern void D0FNAME(fzhook) ();
extern void D0FNAME(fzendo) ();
extern void D0FNAME(fzendi) ();


/******************************************************************************
 * Error reporting.
 */

/* Some special errno codes used for zzip errors. */

#define ZZIP_BAD_MAGIC  -101
#define ZZIP_BAD_RECL   -102
#define ZZIP_SHORT_READ -103

void D0FNAME(zzip_get_last_err) (D0FSTR_DECL (errstr))
{
  char *c_errstr;

  if (errno == ZZIP_BAD_MAGIC)
    c_errstr = "bad zzip magic number";
  else if (errno == ZZIP_BAD_RECL)
    c_errstr = "bad record length in zzip header";
  else if (errno == ZZIP_SHORT_READ)
    c_errstr = "short read on zzip header";
  else
#ifdef VMS
    c_errstr = strerror (errno, vaxc$errno);
#else
    c_errstr = strerror (errno);
#endif

  cstring_to_fstring (c_errstr,
		      D0FSTR_BODY (errstr), D0FSTR_LENGTH (errstr));
}


/******************************************************************************
 * Output (compression).
 */


static void zzip_outproc (char *buf, int *dir_p)
{
  if (*dir_p != 1)
    error ("zzip_outproc: zebra facing backwards");
  _zzip_block (desc_for_lun (iquest[0]), buf, iquest[1]*4);
  iquest[0] = 0;
}


int D0FNAME(zzip_open) (int *lun, int *pack_level, int *recl,
			D0FSTR_DECL (fname_dsc))
{
  struct zzip_desc *z;
  char *fname;

  fname = fstring_to_cstring (D0FSTR_BODY (fname_dsc),
			      D0FSTR_LENGTH (fname_dsc));
  z = _zzip_open (fname, *pack_level);
  free (fname);

  /* write the zzip header */
  if (z != NULL) {
    if (_zzip_write (z, ZZIP_MAGIC, ZZIP_MAGIC_LENGTH) < 0) {
      _zzip_close (z);
      z = NULL;
    }

    write_int_to_zip (z, (ulg)*recl);
  }

  if (z != NULL) {
    D0FNAME(fzfile) (lun, recl, D0FPASS_CSTRING ("CXO"));
    D0FNAME(fzhook) (lun, zzip_outproc, 0);
    register_lun_desc (*lun, z);
  }

  if (z != NULL)
    return 0;
  else
    return -1;
}


void D0FNAME(zzip_close) (int *lun)
{
  D0FNAME(fzendo) (lun, D0FPASS_CSTRING ("T"));
  _zzip_close (desc_for_lun (*lun));
  free_lun_desc (*lun);
}


/******************************************************************************
 * Input (decompression).
 */


static void zunzip_inproc (char *buf, int *dir_p)
{
  struct lun_desc *p = find_lun_desc (iquest[0]);
  int n;
  int recl = iquest[5];

  if (p->desc == NULL)
    error ("znuzip_inproc: unknown lun");

  if (*dir_p != 0)
    error ("zunzip_inproc: zebra facing backwards");

  if (recl > iquest[1])
    error ("zunzip_unproc: bad buffer lengths from zebra");

  ++p->last_recno;
  if (iquest[3] != 0 && iquest[3] != p->last_recno) {
    _zunzip_seek (p->desc, (iquest[3]-1) * recl * 4);
    p->last_recno = iquest[3];
  }

  n = _zunzip_block (p->desc, buf, recl*4);

  iquest[0] = 0;
  iquest[1] = n/4;
}


int D0FNAME(zunzip_open) (int *lun, D0FSTR_DECL (fname_dsc))
{
  struct zunzip_desc *z;
  int recl;
  char *fname;
  
  fname = fstring_to_cstring (D0FSTR_BODY (fname_dsc),
			      D0FSTR_LENGTH (fname_dsc));
  z = _zunzip_open (fname);
  free (fname);

  /* try to read the zzip header. */
  if (z != NULL) {
    int lose = 0;
    uch buf[ZZIP_MAGIC_LENGTH];

    if (read  (z->ifd, buf, sizeof (buf)) != sizeof (buf)) {
      errno = ZZIP_SHORT_READ;
      lose = 1;
    }
    else if (strncmp ((char *)buf, ZZIP_MAGIC, ZZIP_MAGIC_LENGTH) != 0) {
      errno = ZZIP_BAD_MAGIC;
      lose = 1;
    }
    else {
      recl = read_int_from_zip (z);
      if (recl <= 0 || recl > 65535) {
	errno = ZZIP_BAD_RECL;
	lose = 1;
      }
    }

    if (lose) {
      _zunzip_close (z);
      z = NULL;
    }
  }

  /* Declare the file to Zebra. */
  if (z != NULL) {
    D0FNAME(fzfile) (lun, &recl, D0FPASS_CSTRING ("CXID"));
    D0FNAME(fzhook) (lun, zunzip_inproc, 0);
    register_lun_desc (*lun, z);
  }

  if (z != NULL)
    return 0;
  else if (errno == ZZIP_BAD_MAGIC)
    return -2;
  else
    return -1;
}


void D0FNAME(zunzip_close) (int *lun)
{
  D0FNAME(fzendi) (lun, D0FPASS_CSTRING ("T"));
  _zunzip_close (desc_for_lun (*lun));
  free_lun_desc (*lun);
}

