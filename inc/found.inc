* Included file: FOUND.INC - contains an array definition for an array
* to hold the RZ keys returned by a call to RZKEYS.  These contain
* in chronological order, the keys of the records stored in an RZ direct-
* access file or subfile.  
*                                       -Ed Gardella  February 1987
*                                      
* Modifications:
*  3/24/87 ECG - added vector to keep cycle number list
*
      INTEGER MAXDIM, MAXKEY
      PARAMETER (MAXDIM = 10000)
      PARAMETER (MAXKEY = 5)
      INTEGER NEXT1, NCYCLE, LOCYCL, LSTKEY, ICYCLE(19)
*                         
* LOGVEC(1..*) contains a bitmap of matching key vectors ( i.e. a .TRUE.
* in position LOGVEC(J) means that key vector # J matches the search criteria)
*
      LOGICAL LOGVEC(1:MAXDIM)
      COMMON/FOUND/NEXT1, LOGVEC, NCYCLE, LOCYCL, LSTKEY, ICYCLE
*
