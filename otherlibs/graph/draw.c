/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include "libgraph.h"
#include <alloc.h>

value gr_plot(value vx, value vy)
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  gr_check_open();
  if(grremember_mode)
    XDrawPoint(grdisplay, grbstore.win, grbstore.gc, x, Bcvt(y));
  if(grdisplay_mode) {
    XDrawPoint(grdisplay, grwindow.win, grwindow.gc, x, Wcvt(y));
    XFlush(grdisplay);
  }
  return Val_unit;
}

value gr_moveto(value vx, value vy)
{
  grx = Int_val(vx);
  gry = Int_val(vy);
  return Val_unit;
}

value gr_current_x(void)
{
  return Val_int(grx);
}

value gr_current_y(void)
{
  return Val_int(gry);
}

value gr_lineto(value vx, value vy)
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  gr_check_open();
  if(grremember_mode)
    XDrawLine(grdisplay, grbstore.win, grbstore.gc,
              grx, Bcvt(gry), x, Bcvt(y));
  if(grdisplay_mode) {
    XDrawLine(grdisplay, grwindow.win, grwindow.gc,
          grx, Wcvt(gry), x, Wcvt(y));
    XFlush(grdisplay);
  }
  grx = x;
  gry = y;
  return Val_unit;
}

value gr_draw_rect(value vx, value vy, value vw, value vh)
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  int w = Int_val(vw);
  int h = Int_val(vh);

  gr_check_open();
  if(grremember_mode)
    XDrawRectangle(grdisplay, grbstore.win, grbstore.gc,
                   x, Bcvt(y) - h + 1, w, h);
  if(grdisplay_mode) {
    XDrawRectangle(grdisplay, grwindow.win, grwindow.gc,
		   x, Wcvt(y) - h + 1, w, h);
    XFlush(grdisplay);
  }
  return Val_unit;
}

value gr_draw_arc_nat(value vx, value vy, value vrx, value vry, value va1, value va2)
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  int rx = Int_val(vrx);
  int ry = Int_val(vry);
  int a1 = Int_val(va1);
  int a2 = Int_val(va2);

  gr_check_open();
  if(grremember_mode)
    XDrawArc(grdisplay, grbstore.win, grbstore.gc,
             x - rx, Bcvt(y) - ry, rx * 2, ry * 2, a1 * 64, (a2 - a1) * 64);
  if(grdisplay_mode) {
    XDrawArc(grdisplay, grwindow.win, grwindow.gc,
         x - rx, Wcvt(y) - ry, rx * 2, ry * 2, a1 * 64, (a2 - a1) * 64);
    XFlush(grdisplay);
  }
  return Val_unit;
}

value gr_draw_arc(value *argv, int argc)
{
  return gr_draw_arc_nat(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value gr_set_line_width(value vwidth)
{
  int width = Int_val(vwidth);

  gr_check_open();
  XSetLineAttributes(grdisplay, grwindow.gc,
                     width, LineSolid, CapRound, JoinRound);
  XSetLineAttributes(grdisplay, grbstore.gc,
                     width, LineSolid, CapRound, JoinRound);
  return Val_unit;
}
