/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1997 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include "main.h"

QDGlobals qd;
int gHasDragAndDrop = 0;
int gHasPowerManager = 0;
int launch_toplevel_requested = 0;

static OSErr Initialise (void)
{
  long gestval;
  int i;
  OSErr err;

  SetApplLimit (GetApplLimit () - kExtraStackSpace);
  MaxApplZone ();
  for (i = 0; i < kMoreMasters; i++) MoreMasters ();
  InitGraf (&qd.thePort);
  InitFonts ();
  InitWindows ();
  InitMenus ();
  TEInit ();
  InitDialogs (nil);
  InitCursor ();
  FlushEvents (everyEvent, 0);

  /* Unload the clipboard to disk if it's too big. */
  if (InfoScrap ()->scrapSize > kScrapThreshold) UnloadScrap ();

  /* Check for system 7. */
  if (Gestalt (gestaltSystemVersion, &gestval) != noErr
      || gestval < kMinSystemVersion){
    InitCursor ();
    StopAlert (kAlertNeedSys7, NULL);
    ExitToShell ();
  }

  /* Check for 32-bit color QuickDraw. */
  if (Gestalt (gestaltQuickdrawVersion, &gestval) != noErr
      || gestval < gestalt32BitQD){
    InitCursor ();
    StopAlert (kAlertNeed32BitQD, NULL);
    ExitToShell ();
  }

  /* Check for Drag Manager. */
  if (Gestalt (gestaltDragMgrAttr, &gestval) == noErr
      && (gestval & (1 << gestaltDragMgrPresent))
      && (&NewDrag != NULL)){
    gHasDragAndDrop = 1;
  }

  /* Check for Power Manager. */
  if (Gestalt (gestaltPowerMgrAttr, &gestval) == noErr
      && (gestval & (1 << gestaltPMgrExists))){
    gHasPowerManager = 1;
  }

  err = InitialiseErrors ();
  if (err != noErr) goto problem;

  if (gHasDragAndDrop){
    err = InstallDragHandlers ();
    if (err != noErr) goto problem;
  }

  err = InitialiseEvents ();
  if (err != noErr) goto problem;

  err = InitialiseMenus ();
  if (err != noErr) goto problem;

  err = InitialiseScroll ();
  if (err != noErr) goto problem;

  err = InitialiseWindows ();
  if (err != noErr) goto problem;

  err = InitialiseModalFilter ();
  if (err != noErr) goto problem;

  ReadPrefs ();

  return noErr;

  problem: return err;
}

void Finalise (void)
{
  if (gHasDragAndDrop) RemoveDragHandlers ();
  WritePrefs ();
}

int main (void)
{
  OSErr err;

  err = Initialise ();
  if (err != noErr){
    quit_requested = 1;
    exit (0);
  }
  while (!launch_toplevel_requested){
    GetAndProcessEvents (waitEvent, 0, 0);
    if (quit_requested) exit (0);
  }
  err = launch_caml_main (); /* launch bytecode interp and event loop */
  if (err != noErr) ErrorAlertGeneric (err);
  exit (0);
  return 0; /* not reached */
}
