      SUBROUTINE DBL3_COMPRESS(LDATA,HVEC,NHVEC,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compress the structure under LDATA if required
C-
C-   Inputs  : LDATA  = Top level Zebra bank address
C-             HVEC   = FZ File Header as read in by the server
C-             NHVEC  = Length of header vector
C-   Outputs : LDATA  = Modified Zebra link address. (May be zero on return)
C-             IER    = 0  if everything is ok.
C-             IER    < 0  FATAL, Data should not be inserted into DBL3.
C-                         TODO files should be saved and not deleted
C-                       = -1 Unable to read DBL3 COMPRESS RCP file
C-                       = -2 LDATA on input was zero.
C-                       = -3 Invalid path name in input HVEC.
C-                    > 0  NON-FATAL, but data should not be inserted into
C-                         compressed DBL3 database. If accompanied with LBANK =
C-                         0, data should not be sent to Unix Farm. Loop over
C-                         normally to accept next FZ file.
C-                       = +1, This does not require any compression but data
C-                         must be sent to farm.
C-                       = +2, None of the data is required, All banks are
C-                         dropped in this case and LDATA set to zero. Data not
C-                         to be inserted into any database.
C-   Controls: none
C-
C-   Created  14-NOV-1993   Srini Rajagopalan/Lars Rasmussen
C-   Updated  29-NOV-1993   Srini Rajagopalan, Make 'CCON' character*12 from
C-                          c*3 and 'PATH' from c*25 to c*32
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      LOGICAL first,ldbm
      LOGICAL dbl3_compress_ini
      DATA first /.true./
C
      CHARACTER*32 path
      CHARACTER*12 ccon
      CHARACTER*3 det
      CHARACTER*1 detchr
C
      INTEGER what,nkey,key(100),hvec(*),nhvec
      INTEGER lrcp,ldata,ier
C----------------------------------------------------------------------
C
C read in rcp file if not yet read in
C
      ier = 0
      IF (first) THEN
        first = .false.
        CALL ezloc('DBL3_COMPRESS_RCP',lrcp)
        IF (lrcp.LE.0) THEN
          IF (.NOT.dbl3_compress_ini()) THEN
            CALL errmsg('Initialization failure','DBL3_COMPRESS',
     &                  ' ','W')
            go to 999
          ENDIF
        ENDIF
      ENDIF
C
C Check input link
C
      IF (ldata.LE.0) THEN
        ier = -2
        CALL errmsg('Input link=0','DBL3_COMPRESS',' ','W')
        go to 999
      ENDIF
C
C unpack header, exit if not inserting data (check what) into DBL3
C
      CALL d0dbl3_unphed(what,path,nkey,key,ccon,hvec,nhvec)
      IF (what.NE.10 .AND. what.NE.11) go to 999
C
      det = ' '
C
      ldbm = path(1:7).EQ.'//D0DBM'
      IF (ldbm) THEN
        det = 'DBM'
      ELSE
        IF (path(1:7).NE.'//D0STP') THEN
          ier = -3
          CALL errmsg('Invalid path name','DBL3_COMPRESS',
     &                'Data Not compressed','W')
          go to 999
        ENDIF
        detchr = path(9:9)
        IF (detchr.EQ.'V') det = 'VTX'
        IF (detchr.EQ.'T') det = 'TRD'
        IF (detchr.EQ.'D') det = 'CDC'
        IF (detchr.EQ.'F') det = 'FDC'
        IF (detchr.EQ.'C') det = 'CAL'
C
C other detectors do not need compressed database
C
        IF (det.EQ.' ') THEN
          ier = 1
          go to 999
        ENDIF
      ENDIF
C
C Drop banks ....
C
      CALL dbl3_drop(det,path,ldata,ier)
C
  999 RETURN
      END
