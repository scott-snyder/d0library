/*
-------------------------------------------------------------------------------

 Name      : cfget

 Purpuse   : Replacement for cernlib version of cfget

 Fortran calling sequence:

       CALL CFGET(LUNDES, MEDIUM, NWREC, NWTAK, MBUF, STAT)

 Inputs  : LUNDES (INTEGER) - C file descriptor (returned via IQUEST(1) by 
                              CFOPEN).
           MEDIUM (INTEGER) - 0 = Disk ('L' FZFILE option).
                              1 = Tape ('TL' FZFILE options).
                              2 = Disk, user suppl. code ('K' FZFILE option).
                              3 = Tape, user suppl. code ('TK' FZFILE options).
           NWREC (INTEGER)  - Maximum number of longwords to read.

 Outputs : NWTAK (INTEGER)  - Actual number of longwords read (rounded up, if 
                              necessary).
           MBUF (Any type)  - Data buffer.
           STAT (INTEGER)   - Return status (0 = everything OK,
                              -1 = end-of-file, positive = error).

 Usage note:

   This routine is based on cernlib v92b, SGI flavor (but it should work
   on other UNIXes as well).  It implements the MEDIUM=2 and MEDIUM=3 options 
   to provide downward compatibility for reading early raw data tapes written 
   with the (incorrect) record size of 32,764 bytes.  These files are opened 
   by specifying the 'K' option to FZFILE.  This option is normally supplied 
   automatically to the user by the D0OPEN entry point XZRECL.  At the
   present time, the "tape" medium values function identically to the "disk"
   medium values.  That is, the 'T' FZFILE option has no effect.

 Created 29-Dec-1992   Herbert B. Greenlee

------------------------------------------------------------------------------
*/

#include <unistd.h>          /*  HPX SGI SUN                */
#include <errno.h>
#define NBYTPW 4       /* Number of bytes per word */
void cfget_(lundes, medium, nwrec, nwtak, mbuf, stat)
      char *mbuf;
      int  *lundes, *medium, *nwrec, *nwtak, *stat;
{
      int  fildes;
      int  nbdn, nbdo;

      *stat = 0;
      if (*nwtak <= 0)            return;

/*        read the file      */

      fildes = *lundes;
      nbdo   = *nwrec * NBYTPW;

/* Code added by HBG */

      if(*medium >= 2)
	nbdo = nbdo + 4;

      nbdn   = read (fildes, mbuf, nbdo);
      if (nbdn == 0)               goto heof;
      if (nbdn <  0)               goto herror;
retn: *nwtak = (nbdn - 1) / NBYTPW + 1;
      return;

/*        Handle exceptions        */

heof:     *stat = -1;
          return;

herror:   *stat = errno;
          perror (" error in CFGET");
          return;
}
