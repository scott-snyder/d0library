C CMS REPLACEMENT HISTORY, Element FIX_SHOWERLIB_VERTEX_INI.FOR
C *1    13-OCT-1995 11:58:08 DHIMAN "by Scott Snyder"
C CMS REPLACEMENT HISTORY, Element FIX_SHOWERLIB_VERTEX_INI.FOR
      logical function fix_showerlib_vertex_ini
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-     Initialization for the fix_showerlib_vertex package.
C-     Read the RCP file and get the parameter SHOWERLIB_DATA.
C-     Create the flag FIX_SHLIB_VERTEX, and set it to match
C-     the RCP parameter.
C-
C-   Created  26-SEP-1995   scott snyder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      logical showerlib_data

      character*32 me
      parameter (me = 'fix_showerlib_vertex_ini')

      logical  ez_get_logical
      external ez_get_logical
C----------------------------------------------------------------------

      call ezpick_and_signal ('FIX_SHOWERLIB_VERTEX_RCP', me)
      showerlib_data = ez_get_logical ('SHOWERLIB_DATA', me)
      call ezrset

      call flgbk ('FIX_SHLIB_VERTEX', 1)
      call flgset ('FIX_SHLIB_VERTEX', showerlib_data)

      fix_showerlib_vertex_ini = .true.

  999 RETURN
      END
