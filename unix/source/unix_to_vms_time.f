      subroutine unix_to_vms_time(unix_time, vms_time)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert UNIX (32-bit) time to VMS (64-bit) time.
C-
C-   Inputs  : unix_time (integer, read only)
C-   Outputs : vms_time (integer 2-word array, write only)
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
C- Correct the input time for the base time and timezone offsets.  The
C- base time offset is exactly 40,587 days.  We use double precision
C- arithmetic, which does not introduce any roundoff error.
C-
      vms_sec = unix_time + 40587.d0*86400.d0 
     &  + 3600.d0*(utc_tz_offset() + daylight_time_offset(unix_time))
C-
C- Convert to base 2**16 for further manipulation
C-
      do 10 i=1,4
        vms_xt(i) = mod(vms_sec, 2.d0**16)
        vms_sec = 2.d0**(-16) * (vms_sec - vms_xt(i))
10    continue
C-
C- Multiply by 10**7 to get the final result.  Use 7 multiply by 10s.
C-
      do 20 i=1,7
        vms_xt(1) = 10*vms_xt(1)
        vms_xt(2) = 10*vms_xt(2) + vms_xt(1)/2**16
        vms_xt(3) = 10*vms_xt(3) + vms_xt(2)/2**16
        vms_xt(4) = 10*vms_xt(4) + vms_xt(3)/2**16
        vms_xt(1) = iand(vms_xt(1),mask16)
        vms_xt(2) = iand(vms_xt(2),mask16)
        vms_xt(3) = iand(vms_xt(3),mask16)
        vms_xt(4) = iand(vms_xt(4),mask16)
20    continue
C-
C- Pack the result into two 32-bit longwords and we are done.
C-
      vms_time(1) = ior(vms_xt(1), ishft(vms_xt(2),16))
      vms_time(2) = ior(vms_xt(3), ishft(vms_xt(4),16))
  999 return
      end
