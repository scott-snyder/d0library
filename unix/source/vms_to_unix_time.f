      subroutine vms_to_unix_time(vms_time, unix_time)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert VMS (64-bit) time to UNIX (32-bit) time.
C-
C-   Inputs  : vms_time (integer 2-word array, read only)
C-   Outputs : unix_time (integer, write only)
C-   Controls: 
C-
C-   Created   20-Mar-1992   Herbert Greenlee
C-
C----------------------------------------------------------------------
      implicit none
      integer vms_time(2), unix_time
      integer vms_xt(4)
      double precision vms_sec
      integer utc_tz_offset, daylight_time_offset
      integer i, mask16
      parameter (mask16 = 2**16-1)
C-
C- Convert vms time to base 2**16 to avoid problems with sign bits.
C-
      vms_xt(1) = iand(vms_time(1),mask16)
      vms_xt(2) = ishft(vms_time(1),-16)
      vms_xt(3) = iand(vms_time(2),mask16)
      vms_xt(4) = ishft(vms_time(2),-16)
C-
C- Divide by 10**7 to convert to seconds.  Use 7 divide by 10s to avoid
C- overflow problems.
C-
      do 10 i=1,7
        vms_xt(3) = vms_xt(3) + 2**16 * mod(vms_xt(4),10)
        vms_xt(2) = vms_xt(2) + 2**16 * mod(vms_xt(3),10)
        vms_xt(1) = vms_xt(1) + 2**16 * mod(vms_xt(2),10)
        vms_xt(4) = vms_xt(4)/10
        vms_xt(3) = vms_xt(3)/10
        vms_xt(2) = vms_xt(2)/10
        vms_xt(1) = vms_xt(1)/10
10    continue
C-
C- Now convert the result to double precision.  The magnitude of the time has
C- been reduced sufficiently so that this can now be done without roundoff 
C- error.  Note that vms_xt(4) has been reduced to zero.
C-
      vms_sec = 2.d0**32*vms_xt(3) + 2.d0**16*vms_xt(2) + vms_xt(1)
C-
C- Correct for the base time and timezone offsets.  The base time offset is
C- exactly 40,587 days.  The result can be converted to a signed 32-bit
C- integer without overflow until 2038.
C-
      unix_time = vms_sec - 40587.d0*86400.d0 - 3600.d0*utc_tz_offset()
      unix_time = unix_time - 3600.d0*daylight_time_offset(unix_time)
  999 return
      end
