C------------------------------------------------------------------
C   Cuts for L2_EM which come as LOGICAL variables
C      Mar 10, 92                        Yi    Xia
C-
C------------------------------------------------------------------
C
C...the following are fed to L2_EM as cuts.  The arrays are the full set; the
C...single values are the cuts for this call.  These are all logical variables
      LOGICAL DO_ISOLATION(NSETS)
      COMMON/L2_EM_CUTS_L/DO_ISOLATION
