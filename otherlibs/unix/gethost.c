/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <string.h>
#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <fail.h>
#include <signals.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include "socketaddr.h"
#ifndef _WIN32
#include <sys/types.h>
#include <netdb.h>
#endif

#define NETDB_BUFFER_SIZE 10000

#ifdef _WIN32
#define GETHOSTBYADDR_IS_REENTRANT
#define GETHOSTBYNAME_IS_REENTRANT
#endif

static int entry_h_length;

extern int socket_domain_table[];

static value alloc_one_addr(char const *a)
{
  struct in_addr addr;
  memmove (&addr, a, entry_h_length);
  return alloc_inet_addr(addr.s_addr);
}

static value alloc_host_entry(struct hostent *entry)
{
  value res;
  value name = Val_unit, aliases = Val_unit;
  value addr_list = Val_unit, adr = Val_unit;

  Begin_roots4 (name, aliases, addr_list, adr);
    name = copy_string((char *)(entry->h_name));
    aliases = copy_string_array((const char**)entry->h_aliases);
    entry_h_length = entry->h_length;
#ifdef h_addr
    addr_list = alloc_array(alloc_one_addr, (const char**)entry->h_addr_list);
#else
    adr = alloc_one_addr(entry->h_addr);
    addr_list = alloc_small(1, 0);
    Field(addr_list, 0) = adr;
#endif
    res = alloc_small(4, 0);
    Field(res, 0) = name;
    Field(res, 1) = aliases;
    Field(res, 2) = entry->h_addrtype == PF_UNIX ? Val_int(0) : Val_int(1);
    Field(res, 3) = addr_list;
  End_roots();
  return res;
}

CAMLprim value unix_gethostbyaddr(value a)
{
  uint32 adr = GET_INET_ADDR(a);
  struct hostent * hp;
#if HAS_GETHOSTBYADDR_R == 7
  struct hostent h;
  char buffer[NETDB_BUFFER_SIZE];
  int h_errnop;
  enter_blocking_section();
  hp = gethostbyaddr_r((char *) &adr, 4, AF_INET,
                       &h, buffer, sizeof(buffer), &h_errnop);
  leave_blocking_section();
#elif HAS_GETHOSTBYADDR_R == 8
  struct hostent h;
  char buffer[NETDB_BUFFER_SIZE];
  int h_errnop, rc;
  enter_blocking_section();
  rc = gethostbyaddr_r((char *) &adr, 4, AF_INET,
                       &h, buffer, sizeof(buffer), &hp, &h_errnop);
  leave_blocking_section();
  if (rc != 0) hp = NULL;
#elif HAS_GETHOSTBYADDR_R == 5
  struct hostent h;
  struct hostent_data hdata;
  int rc;
  enter_blocking_section();
  rc = gethostbyaddr_r((char *) &adr, 4, AF_INET, &h, &hdata);
  leave_blocking_section();
  hp = rc == 0 ? &h : NULL;
#else
#ifdef GETHOSTBYADDR_IS_REENTRANT
  enter_blocking_section();
#endif
  hp = gethostbyaddr((char *) &adr, 4, AF_INET);
#ifdef GETHOSTBYADDR_IS_REENTRANT
  leave_blocking_section();
#endif
#endif
  if (hp == (struct hostent *) NULL) raise_not_found();
  return alloc_host_entry(hp);
}

CAMLprim value unix_gethostbyname(value name)
{
  struct hostent * hp;
  char * hostname;

#if HAS_GETHOSTBYNAME_R != 0 || GETHOSTBYNAME_IS_REENTRANT
  hostname = stat_alloc(string_length(name) + 1);
  strcpy(hostname, String_val(name));
#else
  hostname = String_val(name);
#endif

#if HAS_GETHOSTBYNAME_R == 5
  {
    struct hostent h;
    char buffer[NETDB_BUFFER_SIZE];
    int h_errno;
    enter_blocking_section();
    hp = gethostbyname_r(hostname, &h, buffer, sizeof(buffer), &h_errno);
    leave_blocking_section();
  }
#elif HAS_GETHOSTBYNAME_R == 6
  {
    struct hostent h;
    char buffer[NETDB_BUFFER_SIZE];
    int h_errno, rc;
    enter_blocking_section();
    rc = gethostbyname_r(hostname, &h, buffer, sizeof(buffer), &hp, &h_errno);
    leave_blocking_section();
    if (rc != 0) hp = NULL;
  }
#elif HAS_GETHOSTBYNAME_R == 3
  {
    struct hostent h;
    struct hostent_data hdata;
    int rc;
    enter_blocking_section();
    rc = gethostbyname_r(hostname, &h, &hdata);
    leave_blocking_section();
    hp = rc == 0 ? &h : NULL;
  }
#else
#ifdef GETHOSTBYNAME_IS_REENTRANT
  enter_blocking_section();
#endif
  hp = gethostbyname(hostname);
#ifdef GETHOSTBYNAME_IS_REENTRANT
  leave_blocking_section();
#endif
#endif

#if HAS_GETHOSTBYNAME_R != 0 || GETHOSTBYNAME_IS_REENTRANT
  stat_free(hostname);
#endif

  if (hp == (struct hostent *) NULL) raise_not_found();
  return alloc_host_entry(hp);
}

#else

CAMLprim value unix_gethostbyaddr(value name)
{ invalid_argument("gethostbyaddr not implemented"); }
  
CAMLprim value unix_gethostbyname(value name)
{ invalid_argument("gethostbyname not implemented"); }
 
#endif
