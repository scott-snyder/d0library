      FUNCTION DBL3_COMPRESS_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialization for DBL3 compression package
C-
C-   Returned value  :
C-   Inputs  : none
C-   Outputs : none
C-   Controls:
C-
C-   Created   15-NOV-1993   Srini Rajagopalan
C-   Modified  15-NOV-1993   S. Abachi    MZLINK called only once
C-   Updated   29-NOV-1993   SR, Remove IF FIRST call...in calling routine
C-   Modified  20-DEC-1993   S. Abachi    dbcomp.inc --> dbl3_comp.inc
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'd0$inc:zebstp.inc'
      INCLUDE 'd0$inc:dbl3_comp.inc'
      LOGICAL dbl3_compress_ini
C
      INTEGER ier,ilen
      INTEGER idrop,idtyp,ipath,icls,idev
      CHARACTER*20 name
C
C common for dbmon link area...
C
      INTEGER ldrop(max_drop)
      COMMON /DBDROP/ ldrop
C
C----------------------------------------------------------------------
C
      dbl3_compress_ini = .false.
C
C  Open RCP file.....
C
      CALL inrcp('DBL3_COMPRESS_RCP',ier)
      IF (ier.NE.0) THEN
        CALL errmsg('Unable to read RCP file','DBL3_COMPRESS_INI',
     &              ' ','F')
      ENDIF
C
      CALL ezpick('DBL3_COMPRESS_RCP')
C
C read calib stuff...........................................................
C
      CALL ezget_number_strings('DTYP_CALIB',num_dtyp,ier)
      IF (num_dtyp.LE.0) THEN
        CALL errmsg('No detectors specified','DBCLB_COMPRESS',' ','W')
      ELSE IF (num_dtyp.GT.max_dtyp) THEN
        CALL errmsg('Too many detectors specified','DBCLB_COMPRESS',
     &              ' ','F')
      ENDIF
C
      DO idtyp = 1,num_dtyp
C
        CALL ezgets('DTYP_CALIB',idtyp,dtyp_calib(idtyp),ilen,ier)
C
        name = dtyp_calib(idtyp)(1:3)//'_CALIB_PATH'
        CALL ezget_number_strings(name(1:14),npath(idtyp),ier)
C
        IF (npath(idtyp).LE.0) THEN
          CALL errmsg('No path specified','DBCLB_COMPRESS',' ','W')
        ENDIF
C
        DO ipath=1,npath(idtyp)
          CALL ezgets(name(1:14),ipath,calib_path(ipath,idtyp),
     &                ilen,ier)
        ENDDO
C
        name = dtyp_calib(idtyp)(1:3)//'_CALIB_DROP'
        CALL ezget_number_strings(name(1:14),ndrop(idtyp),ier)
C
        IF (ndrop(idtyp).GT.max_drop) THEN
          CALL errmsg('Too many banks to drop','DBL3_COMPRESS_INI',
     &                ' ','F')
        ENDIF
C
        DO idrop=1,ndrop(idtyp)
          CALL ezgets(name(1:14),idrop,bank_drop(idrop,idtyp),
     &                ilen,ier)
        ENDDO
      ENDDO                           ! End of calib/detector loop
C
C dbmon stuff ..............................................................
C
c- get classes and devices for each class
c
      CALL ezget_number_strings( 'DBM_CLAS', ncls, ier )
C
      IF (ncls .LE. 0) THEN
        CALL errmsg('No classes specified','DBCLB_COMPRESS',' ','W')
      ELSE IF (ncls.GT.max_cls) THEN
        CALL errmsg('Too many classes specified','DBCLB_COMPRESS',
     &              ' ', 'F')
      ENDIF
C
      DO icls = 1,ncls
        CALL ezgets( 'DBM_CLAS',icls,clas(icls),ilen,ier )
        CALL ezget_number_strings( clas(icls),ndev(icls),ier )
        IF ( ndev(icls) .GT. max_dev ) THEN
          CALL errmsg('Too many devices specified','DBCLB_COMPRESS',
     &                ' ','F')
        ENDIF
C
        DO idev = 1,ndev(icls)
          CALL ezgets( clas(icls),idev,devi(idev,icls),ilen,ier )
        END DO
C
      ENDDO                           ! End of dbmon/class loop
C
C Create a link area for dbmon stuff
C
      CALL mzlink( ixstp,'/DBDROP/',ldrop(1),ldrop(max_drop),ldrop(1) )
C
      dbl3_compress_ini = .true.
C
  999 RETURN
      END
