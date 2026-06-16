/* muddy.c is the layer between the MuDDy ML library and the BuDDy C library.*/
/* Copyright (C) 1997-2002 by Ken Friis Larsen and Jakob Lichtenberg.        */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <memory.h>

/* BDD stuff */
#include <bdd.h>
#include <fdd.h>
#include <bvec.h>

/* Mosml stuff */
#include <caml/mlvalues.h> 
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/memory.h>

#include "custom.h"

/* Detect whether we are compiling with OCaml or Mosml include files: */

#ifdef OCAML_OS_TYPE
#define CUDDY
#else
#define MUDDY
#endif

#ifdef CUDDY
#include <caml/custom.h>
#else /* MUDDY */
#include <str.h>
#endif

void mlbdd_muddy_extract_sub();
#ifdef PARALLEL
#include <pthread.h>
#include <semaphore.h>
#define MAX_BUFFER 1024000
int shared[MAX_BUFFER*3];
int g_pos=0;
int g_read=0;
int g_finished=0;
int g_work=0;
static int g_cores = 1;
pthread_t g_cid;

sem_t empty, full;
#else
#endif

/* Reduced Ordered Binary Decision Diagrams: interface to
   J?rn Lind-Nielsen's <buddy@it.edu> BuDDy library.
   Made by Ken Friis Larsen <kfl@it.edu>

   The type bdd.bdd of a Binary Decision Diagram is an abstract type;
   really a BDD structure.  This will contain an integer which is a
   root number.  The root number cannot just be treated as an ordinary
   int for two reasons:
   
   1. The gc cannot understand the root number (it would be confused
      by the untagged integer field)

   2. The (camlrunm) gc don't know how to garbage collect a bdd 
      (call bdd_delref from the bdd lib.)
   
   This raises the question how to deallocate the bdd structure when
   it is no longer reachable.  One possibility is to use finalized
   objects, calling the bdd_delref function explicitly whenever a bdd
   value is about to be garbage-collected by the camlrunm runtime
   system.

   A bdd should be a finalized object: a pair, 

              header with Final_tag
	      0: finalization function mlbdd_finalize
	      1: the bdd's root number

   whose component 0 is a pointer to the finalizing function
   mlbdd_finalize, and whose component 1 is a root number.  The
   finalization function should apply bdd_delref to the second
   component of the pair: */

/* To make DLL on Windows we need non-standard annotations */
#ifdef WIN32
#define EXTERNML __declspec(dllexport)
#define INLINE
#else
#define EXTERNML
#define INLINE inline
#endif

/* Sometimes it is nice to raise the Domain exception
 */

#ifdef CUDDY
#define RAISE_DOMAIN mlraise(Atom(ZERO_DIVIDE_EXN))

#else /* MUDDY */
#include <globals.h>

#ifndef SMLEXN_DOMAIN /* SMLEXN_DOMAIN is not defined in mosml 2.00 */
#define RAISE_DOMAIN raiseprimitive0(SYS__EXN_DOMAIN)
#else
#define RAISE_DOMAIN mlraise(Atom(SMLEXN_DOMAIN))
#endif
#endif



#if DEBUG
#define DEBUG_MSG(x) x 
#else
#define DEBUG_MSG(x)
#endif

/* A nice macro to have */
#ifdef CUDDY
  #define Bdd_val(x) (*((BDD *) Data_custom_val(x)))
#else
  #define Bdd_val(x) (Field(x, 1))
#endif



/* I don't want to adjust the GC so I've made my own alloc_final,
   stolen from alloc.c
*/
/*static INLINE value mlbdd_alloc_final(mlsize_t len, final_fun fun)
{
  value result;
  result = alloc_shr(len, Final_tag);
  Final_fun(result) = fun;
  return result;
} 
*/

static BDD root_bdd = 0;
static BDD sub_bdd;
#define MAX_BDD 64
#define MAX_SET 81720

int g_v1;
int g_v2;
int g_v3;
int g_v;

// bitv n1 n2 n3(internal)
static inline char* bitv(int n1, int n2, int n3)
{
	static char v[MAX_BDD];
	int i;
	for (i=0; i<g_v1; i++)
	{
		v[i] = n1 & 1;
		n1>>=1;
	}
	for (; i<g_v2+g_v1; i++)
	{
		v[i] = n2 & 1;
		n2>>=1;
	}
	for (; i<g_v3+g_v2+g_v1; i++)
	{
		v[i] = n3 & 1;
		n3>>=1;
	}
	return v;
}

int printbdd(BDD b)
{
	if (b==0)
		return 0;
	if (b==1)
		return 1;
	int z;
	z = printbdd(bdd_high(b));
	if (z)
	{
		printf("1");
		return 1;
	}
	else
	{
		z = printbdd(bdd_low(b));
		if (z)
		{
			printf("0");
			return 1;
		}
	}
	return 0;
}

void print(BDD b)
{
	printbdd(b);
	printf("\n");
}

// singleton n1 n2 n3(internal)
static inline BDD singleton(int n1, int n2, int n3)
{
  char* v = bitv(n1,n2,n3);
  int i;
  DEBUG_MSG(printf("bitv: %d, %d, %d(", n1, n2, n3);)
  DEBUG_MSG(for (i=0;i<g_v;i++) printf("%d",v[i]);)
  DEBUG_MSG(printf(")\n");)
  DEBUG_MSG(printf("g_v: %d, %d, %d, %d\n", g_v1, g_v2, g_v3, g_v);)

  BDD b1 = bddtrue;
  BDD b2 = bddtrue;
  BDD b3 = bddtrue;
  BDD t,tmp;

  for (i=0; i<g_v1; i++)
  {
    t = v[i]?bdd_ithvar(i):bdd_nithvar(i);
    tmp = bdd_apply(b1,t,bddop_and);
    bdd_addref(tmp);
    bdd_delref(b1);
    b1 = tmp;
  }
  for (; i<g_v1+g_v2; i++)
  {
    t = v[i]?bdd_ithvar(i):bdd_nithvar(i);
    tmp = bdd_apply(b2,t,bddop_and);
    bdd_addref(tmp);
    bdd_delref(b2);
    b2 = tmp;
  }
  for (; i<g_v1+g_v2+g_v3; i++)
  {
    t = v[i]?bdd_ithvar(i):bdd_nithvar(i);
    tmp = bdd_apply(b3,t,bddop_and);
    bdd_addref(tmp);
    bdd_delref(b3);
    b3 = tmp;
  }
   
  t = bdd_apply(b1,b2,bddop_and);
  bdd_addref(t);
  tmp = bdd_apply_wo_cache(t,b3,bddop_and);
  bdd_addref(tmp); 

  return tmp;
}

static int cmp_loc_bits(const void *a, const void *b)
{
  int x = *(const int *)a;
  int y = *(const int *)b;
  int i;

  for (i = 0; i < g_v3; i++)
  {
    int xb = (x >> i) & 1;
    int yb = (y >> i) & 1;
    if (xb != yb)
      return xb - yb;
  }
  return 0;
}

static BDD apply_ref(BDD left, BDD right, int op)
{
  BDD r = bdd_apply(left, right, op);
  bdd_addref(r);
  return r;
}

static BDD prefix_cube(int n1, int n2)
{
  BDD acc = bddtrue;
  BDD tmp;
  int i;

  bdd_addref(acc);
  for (i = 0; i < g_v1; i++)
  {
    BDD v = (n1 & 1) ? bdd_ithvar(i) : bdd_nithvar(i);
    tmp = apply_ref(acc, v, bddop_and);
    bdd_delref(acc);
    acc = tmp;
    n1 >>= 1;
  }
  for (; i < g_v1 + g_v2; i++)
  {
    BDD v = (n2 & 1) ? bdd_ithvar(i) : bdd_nithvar(i);
    tmp = apply_ref(acc, v, bddop_and);
    bdd_delref(acc);
    acc = tmp;
    n2 >>= 1;
  }
  return acc;
}

static BDD build_loc_set(int *ids, int lo, int hi, int bit)
{
  BDD low;
  BDD high;
  BDD low_term;
  BDD high_term;
  BDD res;
  int mid;

  if (lo >= hi)
  {
    bdd_addref(bddfalse);
    return bddfalse;
  }
  if (bit >= g_v3)
  {
    bdd_addref(bddtrue);
    return bddtrue;
  }

  mid = lo;
  while (mid < hi && (((ids[mid] >> bit) & 1) == 0))
    mid++;

  low = build_loc_set(ids, lo, mid, bit + 1);
  high = build_loc_set(ids, mid, hi, bit + 1);

  if (low == bddfalse)
  {
    bdd_addref(bddfalse);
    low_term = bddfalse;
  }
  else
  {
    low_term = apply_ref(bdd_nithvar(g_v1 + g_v2 + bit), low, bddop_and);
  }

  if (high == bddfalse)
  {
    bdd_addref(bddfalse);
    high_term = bddfalse;
  }
  else
  {
    high_term = apply_ref(bdd_ithvar(g_v1 + g_v2 + bit), high, bddop_and);
  }

  res = apply_ref(low_term, high_term, bddop_or);
  bdd_delref(low);
  bdd_delref(high);
  bdd_delref(low_term);
  bdd_delref(high_term);
  return res;
}

#ifdef PARALLEL
static void* worker(void *arg)
{
  int v1,v2,v3;
  g_finished=1;
  while(1)
  {
    sem_wait(&full);
    DEBUG_MSG(printf("get a full semaphore\n");)
    if ((g_work == 1) && (g_read == g_pos))
		break;
    v1 = shared[g_read++];
    v2 = shared[g_read++];
    v3 = shared[g_read++];
    g_read %= MAX_BUFFER*3;

    DEBUG_MSG(printf("read: %d %d %d\n", v1, v2, v3);)
    BDD b = singleton(v1, v2, v3);
    DEBUG_MSG(printf("union\n");)
    BDD r = bdd_apply_wo_cache(root_bdd,b, bddop_or);
    DEBUG_MSG(printf("delref\n");)
    bdd_delref(b);
    bdd_delref(root_bdd);
    DEBUG_MSG(printf("addref\n");)
    bdd_addref(r);
  
    root_bdd = r;
    sem_post(&empty);
    DEBUG_MSG(printf("post empty\n");)
  }
  DEBUG_MSG(printf("consumer finished\n");)
  g_finished=0;
  return NULL;
}
#endif

// muddy_init v1 v2 v3
/* ML type: int -> unit */
EXTERNML value mlbdd_muddy_init(value v1, value v2, value v3) /* ML */
{ 
  g_v1 = Int_val(v1);
  g_v2 = Int_val(v2);
  g_v3 = Int_val(v3);
  g_v = g_v1+g_v2+g_v3;
  if (g_v > MAX_BDD)
    caml_failwith("mlbdd_muddy_init: too many BDD variables");

#ifdef PARALLEL
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setscope(&attr, PTHREAD_SCOPE_SYSTEM);

  if (g_finished==0)
  {
    sem_init(&empty, 0, MAX_BUFFER);
    sem_init(&full, 0, 0);
  
    pthread_create(&g_cid, &attr, &worker, NULL);
  }
#endif
  bdd_setvarnum(g_v);
  DEBUG_MSG(printf("myddy_init(%d, %d, %d)\n", g_v1, g_v2, g_v3);)
  DEBUG_MSG(printf("setvarnum(%d)\n", g_v);)
  root_bdd = bddfalse;
  bdd_addref(root_bdd);
  return Val_unit;
}

// bitv_src n1 (internal)
static inline char* bitv_src(int n1)
{
	static char v[MAX_BDD];
	int i;
	for (i=0; i<g_v1; i++)
	{
		v[i] = n1 & 1;
		n1>>=1;
	}
	return v;
}

// bitv_sub n1 n2(internal)
static inline char* bitv_sub2(int n1)
{
	static char v[MAX_BDD];
	int i;
	for (i=0; i<g_v2; i++)
	{
		v[i] = n1 & 1;
		n1>>=1;
	}
	return v;
}

// bitv_sub n1 n2(internal)
static inline char* bitv_sub(int n1, int n2)
{
	static char v[MAX_BDD];
	int i;
	for (i=0; i<g_v1; i++)
	{
		v[i] = n1 & 1;
		n1>>=1;
	}
	for (; i<g_v2+g_v1; i++)
	{
		v[i] = n2 & 1;
		n2>>=1;
	}
	return v;
}

EXTERNML value mlbdd_muddy_finished(value dummy) /* ML */
{
#ifdef PARALLEL
  if (g_finished==1)
  {
	g_work = 1;
    sem_post(&full);
    pthread_join(g_cid, NULL);
  }
  
  printf("worker thread finished\n");
#endif
  return dummy;
}

EXTERNML value mlbdd_muddy_printstat(value dummy) /* ML */
{
  DEBUG_MSG(fprintf(stderr, "called muddy_printstat\n");)
//  printf("-------------------------------\n");
//  printf("cardinality: %f\n", bdd_satcount(root_bdd));
//  printf("nodecount: %d\n", bdd_nodecount(root_bdd));
  bddCacheStat c;
  bdd_cachestats(&c);
//  printf("unique access: %ld\n", c.uniqueAccess);
//  printf("unique chain: %ld\n", c.uniqueChain);
//  printf("unique hit: %ld\n", c.uniqueHit);
//  printf("unique miss: %ld\n", c.uniqueMiss);
//  printf("op hit: %ld\n", c.opHit);
//  printf("op miss: %ld\n", c.opMiss);
//  printf("swap count: %ld\n", c.swapCount);

  return dummy;
}
// void add (n1,n2,n3)
// exception
EXTERNML value mlbdd_muddy_add(value n1, value n2, value n3) /* ML */
{
#ifdef PARALLEL
  sem_wait(&empty);
  DEBUG_MSG(printf("get an empty semaphore\n");)

  shared[g_pos++] = Int_val(n1);
  shared[g_pos++] = Int_val(n2);
  shared[g_pos++] = Int_val(n3);
  g_pos %= MAX_BUFFER*3;
  sem_post(&full);
#else
  BDD b = singleton(Int_val(n1), Int_val(n2), Int_val(n3));
  BDD r = bdd_apply_wo_cache(root_bdd,b, bddop_or);
  bdd_delref(b);
  bdd_delref(root_bdd);
  bdd_addref(r);

  root_bdd = r;
#endif
  
  return Val_unit;
}

EXTERNML value mlbdd_muddy_add_set(value n1, value n2, value ns) /* ML */
{
  CAMLparam3(n1, n2, ns);
#ifdef PARALLEL
  int len = Wosize_val(ns);
  int i;
  for (i = 0; i < len; i++)
  {
    sem_wait(&empty);
    shared[g_pos++] = Int_val(n1);
    shared[g_pos++] = Int_val(n2);
    shared[g_pos++] = Int_val(Field(ns, i));
    g_pos %= MAX_BUFFER*3;
    sem_post(&full);
  }
#else
  int len = Wosize_val(ns);
  int *ids;
  BDD locs;
  BDD prefix;
  BDD edge_set;
  BDD r;
  int i;

  ids = malloc(sizeof(int) * len);
  if (ids == NULL)
    caml_failwith("mlbdd_muddy_add_set: out of memory");

  for (i = 0; i < len; i++)
    ids[i] = Int_val(Field(ns, i));
  qsort(ids, len, sizeof(int), cmp_loc_bits);

  locs = build_loc_set(ids, 0, len, 0);
  free(ids);

  prefix = prefix_cube(Int_val(n1), Int_val(n2));
  edge_set = apply_ref(prefix, locs, bddop_and);
  r = bdd_apply_wo_cache(root_bdd, edge_set, bddop_or);
  bdd_delref(locs);
  bdd_delref(prefix);
  bdd_delref(edge_set);
  bdd_delref(root_bdd);
  bdd_addref(r);
  root_bdd = r;
#endif
  CAMLreturn(Val_unit);
}

EXTERNML value mlbdd_muddy_remove(value n1, value n2, value n3)
{
  int i1 = Int_val(n1);
  int i2 = Int_val(n2);
  int i3 = Int_val(n3);
  char* v = bitv(i1, i2, i3);

  BDD q = root_bdd;
  int i;
  while (q>1)
  {
    i = bdd_var(q);
    q = v[i]?bdd_high(q):bdd_low(q);
  }
  
  if (q)
  {
    BDD b = singleton(i1, i2, i3);

    BDD r = bdd_apply_wo_cache(root_bdd,b, bddop_xor);
    bdd_delref(b);
    bdd_delref(root_bdd);
    bdd_addref(r);
  
    root_bdd = r;
  }
  return Val_bool(1);
}

// bool mem (n1, n2, n3)
EXTERNML value mlbdd_muddy_mem(value n1, value n2, value n3) /* ML */
{
  char* v = bitv(Int_val(n1),Int_val(n2),Int_val(n3));

  BDD q = root_bdd;
  int i;
  while (q>1)
  {
    i = bdd_var(q);
	q = v[i]?bdd_high(q):bdd_low(q);
  }

  return Val_bool(q);
}

// void find_sub_bdd (n1, n2)
// exception
static int bdd_reset=0;

EXTERNML value mlbdd_muddy_find_sub_bdd(value n1, value n2)
{
  char* v = bitv_sub(Int_val(n1),Int_val(n2));

  BDD q = root_bdd;
  int i;
  while (q>1)
  {
    i = bdd_var(q);
	if (g_v1+g_v2<=i)
	  break;
	q = v[i]?bdd_high(q):bdd_low(q);
  }

  sub_bdd = q;       /* kept for the legacy enumeration path (next/extract_sub) */
  bdd_reset=1;
  return Val_int(q);  /* REENTRANT: return the (src,dst) sub-BDD handle so a later
                         mem_sub queries THIS edge, not a global sub_bdd that other
                         edges' find_sub calls clobber. Safe: the BDD is frozen
                         during the fixpoint (no add/remove -> no GC). */
}

// bool mem_sub (handle, n3)
EXTERNML value mlbdd_muddy_mem_sub(value handle, value n3)
{
  char* v = bitv(0,0,Int_val(n3));

  BDD q = Int_val(handle);
  int i;
  while (q>1)
  {
    i = bdd_var(q);
	q = v[i]?bdd_high(q):bdd_low(q);
  }

  return Val_bool(q);
}

static int list[MAX_SET];
static int list_pos=0;
static int next_pos=0;
EXTERNML value mlbdd_muddy_next()
{
  if (bdd_reset ==1)
  {
    mlbdd_muddy_extract_sub();
    bdd_reset=0;
	next_pos=0;
  }
  if (list_pos<=next_pos)
    return Val_int(-1);
  next_pos++;
  return Val_int(list[next_pos-1]);
}

void add_list(int i)
{
  if (list_pos >= MAX_SET)
    caml_failwith("mlbdd_muddy_next: extracted set is too large");
  list[list_pos++]=i;
}
void clear_list()
{
  list_pos=0;
}
void print_list()
{
  int i;
  for (i=0; i<list_pos; i++)
    printf("%d ", list[i]);
  printf("\n");
}

static char map[MAX_BDD];
void generate(int p, int v)
{
  if (p == g_v3)
    add_list(v);
  else
  {
    if (map[g_v3-p-1] == 0)
	{
      generate(p+1,v<<1|1);
      generate(p+1,v<<1);
	}
	else if (map[g_v3-p-1] == 1)
      generate(p+1,v<<1|1);
	else
      generate(p+1,v<<1);
  }
}

void traverse(BDD b)
{
  if (b==bddfalse)
    return;
  if (b==bddtrue)
	generate(0, 0);
  else
  {
    int depth = bdd_var(b)-g_v1-g_v2;
    map[depth] = 1;
    traverse(bdd_high(b));
    map[depth] = 2;
    traverse(bdd_low(b));
	map[depth] = 0;
  }
}

void mlbdd_muddy_extract_sub()
{
  memset(map, 0, MAX_BDD);
  clear_list();
  traverse(sub_bdd);
}

static void mlbdd_errorhandler(int errorcode) 
{
  /* printf("mlbdd error: %d\n",errorcode); */
  failwith((char *) bdd_errstring(errorcode));
} 

#ifdef DEBUG_GC
static char* pregc     = NULL;
static char* postgc    = NULL;
static int printgc = 0; /* Invariant: if printgc != 0 then will the
                           two strings above point to valid strings */
static void mlbdd_gc(int num, bddGbcStat* foo)
{
  if(num==1 && printgc) { printf ("%s", pregc); fflush(stdout); }
  else if(num==0 && printgc) { printf("%s", postgc); fflush(stdout); }
  
}
#endif

/* ML type: int -> int -> unit */
EXTERNML value mlbdd_bdd_init(value nodes, value cachesize, value num_of_cores) /* ML */
{
  /* setup the our error handler */
  bdd_init(Int_val(nodes), Int_val(cachesize));
#ifdef PARALLEL
  g_cores = Int_val(num_of_cores);
#endif
  bdd_error_hook(mlbdd_errorhandler);
#ifndef DEBUG_GC
  bdd_gbc_hook(NULL);
#else
  bdd_gbc_hook(mlbdd_gc);
#endif
  return Val_unit;    
}

/* ML type: int -> int */
EXTERNML value mlbdd_bdd_setcacheratio(value n) /* ML */
{
  return Val_int(bdd_setcacheratio(Int_val(n)));
}

/* ML type: string -> unit */
EXTERNML value mlbdd_bdd_fnsave(value filename) /* ML */
{
  DEBUG_MSG(fprintf(stderr, "called mlbdd_bdd_fnsave: %s\n", String_val(filename));)
  bdd_fnsave(String_val(filename), root_bdd);

  return Val_unit;
}

/* ML type: string -> unit */
EXTERNML value mlbdd_bdd_fnload(value filename) /* ML */
{
  DEBUG_MSG(fprintf(stderr, "called mlbdd_bdd_fnload: %s\n", String_val(filename));)
  bdd_fnload(String_val(filename), &root_bdd);
  return Val_unit;
}

//static INLINE void mlbdd_freegcstrings () {
//  if(printgc) {
//    free(pregc);
//    free(postgc);
//    printgc = 0;
//  }
//}
//
///* ML type: bool -> string -> string -> unit */
//EXTERNML value mlbdd_setprintgc(value print, value pre, value post) /* ML */
//{
//  mlbdd_freegcstrings();
//
//  if(print == Val_true) {
//    pregc  = strdup(String_val(pre));
//    postgc = strdup(String_val(post));
//    printgc = 1;
//  }
//  
//  return Val_unit;
//}
//
///* ML type: unit -> unit */
//EXTERNML value mlbdd_bdd_done(value nill) /* ML */
//{
//  bdd_done();
//  mlbdd_freegcstrings();
//  return Val_unit;
//}
//
///* ML type: unit -> bool */
//EXTERNML value mlbdd_bdd_isrunning(value nill) /* ML */
//{
//  return bdd_isrunning() ? Val_true : Val_false;
//}
//
///* ML type: int -> unit */
//EXTERNML value mlbdd_bdd_setvarnum(value n) /* ML */
//{ 
//  bdd_setvarnum(Int_val(n));
//  return Val_unit;
//}
//
///* ML type: unit -> int */
//EXTERNML value mlbdd_getvarnum(value dummy) /* ML */
//{ 
//  return Val_long(bdd_varnum());
//}
//
//
///* When the bdd becomes unreachable from the ML process, it will be
//   garbage-collected, mlbdd_finalize() will be called on the bdd,
//   which will do the necessary bdd-bookkeeping.  */
//static void mlbdd_finalize(value obj) 
//{
//  DEBUG_MSG(printf("bdd_delref(%d)\n", Bdd_val(obj));)
//  bdd_delref(Bdd_val(obj));
//}
//
//#ifdef CUDDY
//static int mlbdd_compare(value r1, value r2)
//{
//  CAMLparam2(r1, r2);
//  BDD b1, b2;
//  b1 = Bdd_val(r1);
//  b2 = Bdd_val(r2);
//  if(b1 == b2) CAMLreturn(0);
//  else if(b1 < b2) CAMLreturn(-1);
//  else CAMLreturn(1);
//}
//
//static long mlbdd_hash(value r)
//{
//  CAMLparam1(r);
//  CAMLreturn(Bdd_val(r));
//}
//
//static struct custom_operations mlbdd_custom_oprs =
//  {
//    "edu.it.research/MuDDy/2.01",
//    mlbdd_finalize,
//    mlbdd_compare,
//    mlbdd_hash,
//    custom_serialize_default,
//    custom_deserialize_default
//  };
//
//
///* Creation of a bdd makes a custom block with the root no as data */
//EXTERNML value mlbdd_make(BDD root) 
//{
//  CAMLparam0();
//  CAMLlocal1 (res);
//  bdd_addref(root);
//  res = alloc_custom(&mlbdd_custom_oprs, sizeof(BDD), 0, 1);
//  Bdd_val(res) = root; /* Assumes the sizeof(BDD) == sizeof(void*) */
//  CAMLreturn(res);
//}
//#else
///* Creation of a bdd makes a finalized pair (mlbdd_finalize, root) as
//   described above. */
//EXTERNML value mlbdd_make(BDD root) 
//{
//  value res;
//  bdd_addref(root);  
//  res = mlbdd_alloc_final(2, &mlbdd_finalize);
//  Bdd_val(res) = root;  /* Hopefully a BDD fits in a long */
//  return res;
//}
//#endif
//
///* FOR INTERNAL USAGE */
///* ML type: bdd -> int */
//EXTERNML value mlbdd_root(value r) /* ML */
//{
//  return Val_int(Bdd_val(r));
//}
//
///* ML type: varnum -> bdd */
//EXTERNML value mlbdd_bdd_ithvar(value i) /* ML */
//{ 
//  return mlbdd_make(bdd_ithvar(Int_val(i)));
//}
//
///* ML type: varnum-> bdd */
//EXTERNML value mlbdd_bdd_nithvar(value i) /* ML */
//{ 
//  return mlbdd_make(bdd_nithvar(Int_val(i)));
//}
//
///* ML type: bool -> bdd */
//EXTERNML value mlbdd_fromBool(value b) /* ML */
//{
//  return mlbdd_make(Bool_val(b) ? bddtrue : bddfalse);
//}
//
///* ML type: bdd -> varnum */
//EXTERNML value mlbdd_bdd_var(value r) /* ML */
//{
//  return Val_int(bdd_var(Bdd_val(r)));
//}
//
///* ML type: bdd -> bdd */
//EXTERNML value mlbdd_bdd_low(value r) /* ML */
//{
//  return mlbdd_make(bdd_low(Bdd_val(r)));
//}
//
///* ML type: bdd -> bdd */
//EXTERNML value mlbdd_bdd_high(value r) /* ML */
//{
//  return mlbdd_make(bdd_high(Bdd_val(r)));
//}
//
///* Pass the opr constants from <bdd.h> to ML */ 
///* ML type: unit -> int * int * int * int * int * int * int * int * int *int 	                   * int * int * int * int * int * int * int * int */
//EXTERNML value mlbdd_constants(value unit)	/* ML */
//{
//  value res = alloc_tuple(18);
//  Field(res, 0)  = Val_long(bddop_and);
//  Field(res, 1)  = Val_long(bddop_xor);
//  Field(res, 2)  = Val_long(bddop_or);
//  Field(res, 3)  = Val_long(bddop_nand);
//  Field(res, 4)  = Val_long(bddop_nor);
//  Field(res, 5)  = Val_long(bddop_imp);
//  Field(res, 6)  = Val_long(bddop_biimp);
//  Field(res, 7)  = Val_long(bddop_diff);
//  Field(res, 8)  = Val_long(bddop_less);
//  Field(res, 9)  = Val_long(bddop_invimp);
//  Field(res, 10) = Val_long(BDD_REORDER_FIXED);
//  Field(res, 11) = Val_long(BDD_REORDER_FREE);
//  Field(res, 12) = Val_long(BDD_REORDER_WIN2);
//  Field(res, 13) = Val_long(BDD_REORDER_WIN2ITE);
//  Field(res, 14) = Val_long(BDD_REORDER_SIFT);
//  Field(res, 15) = Val_long(BDD_REORDER_SIFTITE);
//  Field(res, 16) = Val_long(BDD_REORDER_RANDOM);
//  Field(res, 17) = Val_long(BDD_REORDER_NONE);
//
//  DEBUG_MSG(printf("        MuDDy 2.01 Beta  2002-03-12\n");)
//  return res;
//}
//
//
///* ML type: bdd -> bdd -> int -> bdd */
//EXTERNML value mlbdd_bdd_apply(value left, value right, value opr) /* ML */
//{
//  return mlbdd_make(bdd_apply(Bdd_val(left),Bdd_val(right), 
//				Int_val(opr)));
//}
//
///* ML type: bdd -> bdd -> int -> bdd */
//EXTERNML value mlbdd_bdd_apply_wo_cache(value left, value right, value opr) /* ML */
//{
//  return mlbdd_make(bdd_apply_wo_cache(Bdd_val(left),Bdd_val(right), 
//				Int_val(opr)));
//}
///* ML type: bdd -> bdd */
//EXTERNML value mlbdd_bdd_not(value r) /* ML */
//{
//  return mlbdd_make(bdd_not(Bdd_val(r)));
//}
//
///* ML type: bdd -> bdd -> bdd -> bdd */
//EXTERNML value mlbdd_bdd_ite(value x, value y, value z) /* ML */
//{
//  return mlbdd_make(bdd_ite(Bdd_val(x), Bdd_val(y), Bdd_val(z)));
//}
//
///* ML type: bdd -> bdd -> bool */
//EXTERNML value mlbdd_equal(value left, value right) /* ML */
//{
//  return ((Bdd_val(left) == Bdd_val(right)) ? Val_true : Val_false);
//}
//
///* ML type: bdd -> bdd -> bdd */
//EXTERNML value mlbdd_bdd_restrict(value r, value var) /* ML */
//{
//  return mlbdd_make(bdd_restrict(Bdd_val(r),Bdd_val(var)));
//}
//
///* ML type: bdd -> bdd -> int -> bdd */
//EXTERNML value mlbdd_bdd_compose(value f, value g, value var) /* ML */
//{
//  return mlbdd_make(bdd_compose(Bdd_val(f),Bdd_val(g),Int_val(var)));
//}
//
//
//
///* ML type: bdd -> bdd -> bdd */
//EXTERNML value mlbdd_bdd_simplify(value f, value d) /* ML */
//{
//  return mlbdd_make(bdd_simplify(Bdd_val(f), Bdd_val(d)));
//}
//
///* ML type: bdd -> unit */
//EXTERNML value mlbdd_bdd_printdot(value r) /* ML */
//{
//  bdd_printdot(Bdd_val(r));
//  return Val_unit;
//}
//
///* ML type: bdd -> unit */
//EXTERNML value mlbdd_bdd_printset(value r) /* ML */
//{
//  bdd_printset(Bdd_val(r));
//  fflush(stdout);
//  return Val_unit;
//}
//
///* ML type: string -> bdd -> unit */
//EXTERNML value mlbdd_bdd_fnprintset(value filename, value r) /* ML */
//{
//  char *fname;
//  FILE *ofile;
//  fname = String_val(filename);
//  ofile = fopen(fname, "w");
//  if (ofile == NULL)
//    failwith("Unable to open file");
//  else {
//    bdd_fprintset(ofile, Bdd_val(r));
//    fclose(ofile);
//  }
//  return Val_unit;
//}
//
//
///* ML type: string -> bdd -> unit */
//EXTERNML value mlbdd_bdd_fnprintdot(value filename, value r) /* ML */
//{
//  bdd_fnprintdot(String_val(filename), Bdd_val(r));
//  return Val_unit;
//}
//
///* ML type: unit -> unit */
//EXTERNML value mlbdd_bdd_printall(value nill) /* ML */
//{
//  bdd_printall();
//  return nill;
//}
//
///* ML type: unit -> int * int * int * int * int * int * int * int */
//EXTERNML value mlbdd_bdd_stats(value nill)
//{
//  static bddStat stat;
//  value result = alloc_tuple(8);
//
//  bdd_stats(& stat);
//
//  Field(result, 0) = Val_long(stat.produced);
//  Field(result, 1) = Val_long(stat.nodenum);
//  Field(result, 2) = Val_long(stat.maxnodenum);
//  Field(result, 3) = Val_long(stat.freenodes);
//  Field(result, 4) = Val_long(stat.minfreenodes);
//  Field(result, 5) = Val_long(stat.varnum);
//  Field(result, 6) = Val_long(stat.cachesize);
//  Field(result, 7) = Val_long(stat.gbcnum);
//
//  return result;
//}
//
///* ML type: bdd -> real */ 
//EXTERNML value mlbdd_bdd_satcount(value r) /* ML */
//{
//  return copy_double(bdd_satcount(Bdd_val(r)));
//}
//
///* ML type: bdd -> varSet */
//EXTERNML value mlbdd_bdd_satone(value r) /* ML */
//{
//  return mlbdd_make(bdd_satone(Bdd_val(r)));
//}
//
///* ML type: bdd -> int */ 
//EXTERNML value mlbdd_bdd_nodecount(value r) /* ML */
//{
//  return Val_int(bdd_nodecount(Bdd_val(r)));
//}
//
///* ML type: int -> int */
//EXTERNML value mlbdd_bdd_setmaxincrease(value n) /* ML */
//{
//  return Val_int(bdd_setmaxincrease(Int_val(n)));
//}
//
//
//
///* Some helper functions for creating variable sets, these will be
//   represented as BDD's on the C side but as a different type (varSet) on
//   the ML side.
//*/
//
///* ML type: varnum vector -> varSet */
//EXTERNML value mlbdd_makeset(value varvector) /* ML */
//{
//  int size, i, *v;
//  value result;
//
//  size = Wosize_val(varvector);
//
//  /* we use stat_alloc which guarantee that we get the memory (or it
//     will raise an exception). */
//  v  = (int *) stat_alloc(sizeof(int) * size);
//  for (i=0; i<size; i++) {
//     v[i] = Int_val(Field(varvector, i));
//  }
//
//  /* we assume that vector is sorted on the ML side */
//  result = mlbdd_make(bdd_makeset(v, size));
// 
//  /* memory allocated with stat_alloc, should be freed with
//     stat_free.*/
//  stat_free((char *) v);
//
//  return result;
//}
//
///* ML type: varSet -> varnum vector */
//EXTERNML value mlbdd_bdd_scanset(value varset)
//{
//  value result;
//  int *v, n, i;
//
//  if(bdd_scanset(Bdd_val(varset), &v, &n)) {
//    failwith("Illformed varSet");
//    return Val_unit; /* unreachable, here to prevent warnings */
//  } else {
//    if(n == 0)
//      result = Atom(0); /* The empty vector */
//    else {
//      result = n < Max_young_wosize ? alloc(n, 0) : alloc_shr(n, 0);
//      for (i = 0; i < n; i++) {
//	Field(result, i) = Val_long(v[i]);
//      }
//      free(v);
//    }
//  }
//  return result;
//}
//
///* ML type: bdd -> varSet */
//EXTERNML value mlbdd_bdd_support(value b) /* ML */
//{
//  return mlbdd_make(bdd_support(Bdd_val(b)));
//}
//
///* ML type: bdd -> varSet -> bdd */
//EXTERNML value mlbdd_bdd_exist(value b1, value varset) /* ML */
//{
//  return mlbdd_make(bdd_exist(Bdd_val(b1),Bdd_val(varset)));
//}
//
///* ML type: bdd -> varSet -> bdd */
//EXTERNML value mlbdd_bdd_forall(value b1, value varset) /* ML */
//{
//  return mlbdd_make(bdd_forall(Bdd_val(b1),Bdd_val(varset)));
//}
//
///* ML type: bdd -> bdd -> int -> varSet -> bdd */
//EXTERNML value mlbdd_bdd_appall(value left, value right, 
//			 value opr,  value varset) /* ML */
//{
//  return mlbdd_make(bdd_appall(Bdd_val(left),Bdd_val(right), 
//				 Int_val(opr), Bdd_val(varset)));
//}
//
///* ML type: bdd -> bdd -> int -> varSet -> bdd */
//EXTERNML value mlbdd_bdd_appex(value left, value right, 
//			value opr,  value varset) /* ML */
//{
//  return mlbdd_make(bdd_appex(Bdd_val(left),Bdd_val(right), 
//				 Int_val(opr), Bdd_val(varset)));
//}
//
//
///* Some helper for making BddPairs, which on the ML side is called
//   pairSet.  
//
//   A pairSet is handled similar to a bdd.  That is, as as a finalized
//   object.
//*/
//#define PairSet_val(x) (((bddPair **) (x)) [1]) // Also an l-value
//
//
//void mlbdd_pair_finalize(value pairset)
//{
//  bdd_freepair(PairSet_val(pairset));
//}
//
///* ML type: varnum vector -> varnum vector -> pairSet */
//EXTERNML value mlbdd_makepairset(value oldvar, value newvar) /* ML */
//{
//  int size, i, *o, *n;
//  bddPair *pairs;
//  value result;
//
//  size = Wosize_val(oldvar);
//
//  /* we use stat_alloc which guarantee that we get the memory (or it
//     will raise an exception). */
//  o    = (int *) stat_alloc(sizeof(int) * size);
//  n    = (int *) stat_alloc(sizeof(int) * size);
//
//  for (i=0; i<size; i++) {
//     o[i] = Int_val(Field(oldvar, i));
//     n[i] = Int_val(Field(newvar, i));
//  }
//
//  pairs = bdd_newpair();
//  bdd_setpairs(pairs, o, n, size);
//
//  /* memory allocated with stat_alloc, should be freed with
//     stat_free.*/
//  stat_free((char *) o);
//  stat_free((char *) n);
//
//  result = mlbdd_alloc_final(2, &mlbdd_pair_finalize);
//  PairSet_val(result) = pairs;
//
//  return result;
//}
//
//
///* ML type: varnum vector -> bdd vector -> pairSet */
//EXTERNML value mlbdd_makebddpairset(value oldvar, value newvar) /* ML */
//{
//  int size, i, *o, *n;
//  bddPair *pairs;
//  value result;
//
//  size = Wosize_val(oldvar);
//
//  /* we use stat_alloc which guarantee that we get the memory (or it
//     will raise an exception). */
//  o    = (int *) stat_alloc(sizeof(int) * size);
//  n    = (BDD *) stat_alloc(sizeof(int) * size);
//
//  for (i=0; i<size; i++) {
//     o[i] = Int_val(Field(oldvar, i));
//     n[i] = Bdd_val(Field(newvar, i));
//  }
//
//  pairs = bdd_newpair();
//  bdd_setbddpairs(pairs, o, n, size);
//
//  /* memory allocated with stat_alloc, should be freed with
//     stat_free.*/
//  stat_free((char *) o);
//  stat_free((char *) n);
//
//  result = mlbdd_alloc_final(2, &mlbdd_pair_finalize);
//  PairSet_val(result) = pairs;
//
//  return result;
//}
//
//
//
///* ML type: bdd -> pairSet -> bdd */
//EXTERNML value mlbdd_bdd_replace(value r, value pair) /* ML */
//{
//  return mlbdd_make(bdd_replace(Bdd_val(r), PairSet_val(pair)));
//}
//
//
///* ML type: pairSet -> bdd -> bdd */
//EXTERNML value mlbdd_bdd_veccompose(value pair, value r) /* ML */
//{
//  return mlbdd_make(bdd_veccompose(Bdd_val(r), PairSet_val(pair)));
//}
//
//
///* REORDER FUNCTIONS */
//
///* ML type: varnum -> varnum -> fixed -> unit */
//EXTERNML value mlbdd_bdd_intaddvarblock(value first, value last, value fixed) /* ML */
//{
//  bdd_intaddvarblock(Int_val(first), Int_val(last), Int_val(fixed));
//  return Val_unit;
//}
//
///* ML type:  unit -> unit */
//EXTERNML value mlbdd_bdd_clrvarblocks(value dummy) /* ML */
//{
//  bdd_clrvarblocks();
//  return dummy;
//}
//
//EXTERNML value mlbdd_bdd_printorder(value dummy)
//{
//  bdd_printorder();
//  return dummy;
//}
//
///* ML type: method -> unit  */
//EXTERNML value mlbdd_bdd_reorder(value method) /* ML */
//{
//  bdd_reorder(Int_val(method));
//  return Val_unit;
//}
//
///* ML type: method -> method  */
//EXTERNML value mlbdd_bdd_autoreorder(value method) /* ML */
//{
//  return Val_long(bdd_autoreorder(Int_val(method)));
//}
//
///* ML type: method -> int -> method  */
//EXTERNML value mlbdd_bdd_autoreorder_times(value method, value times) /* ML */
//{
//  return Val_long(bdd_autoreorder_times(Int_val(method), Int_val(times)));
//}
//
//
///* ML type: unit -> method  */
//EXTERNML value mlbdd_bdd_getreorder_method(value dummy) /* ML */
//{
//  return Val_long(bdd_getreorder_method());
//
//}
//
///* ML type: unit -> int     */
//EXTERNML value mlbdd_bdd_getreorder_times(value dummy) /* ML */
//{
//  return Val_long(bdd_getreorder_times());
//}
//
//
///* ML type: unit -> unit  */
//EXTERNML value mlbdd_bdd_disable_reorder(value dummy) /* ML */
//{
//  bdd_disable_reorder();
//  return dummy;
//}
//
///* ML type: unit -> unit  */
//EXTERNML value mlbdd_bdd_enable_reorder(value dummy) /* ML */
//{
//  bdd_enable_reorder();
//  return dummy;
//}
//
//
///* ML type: varnum -> level  */
//EXTERNML value mlbdd_bdd_var2level(value num) /* ML */
//{
//  return Val_long(bdd_var2level(Int_val(num)));
//}
//
///* ML type: level -> varnum  */
//EXTERNML value mlbdd_bdd_level2var(value lev) /* ML */
//{
//  return Val_long(bdd_level2var(Int_val(lev)));
//}
//
///* FDD FUNCTIONS */
//
///* ML type: int vector -> fddvar */
//EXTERNML value mlfdd_extdomain(value vector) /* ML */
//{
//  int size, i, *v,k;
//  value result;
//
//  size = Wosize_val(vector);
//
//  /* we use stat_alloc which guarantee that we get the memory (or it
//     will raise an exception). */
//  v  = (int *) stat_alloc(sizeof(int) * size);
//  for (i=0; i<size; i++) {
//    v[i] = Int_val(Field(vector, i));
//  }
//  k = fdd_extdomain(v, size);
//  result = Val_int(k);
// 
//  /* memory allocated with stat_alloc, should be freed with
//     stat_free.*/
//  stat_free((char *) v);
//
//  return result;
//}
//
///* ML type: unit -> unit */
//EXTERNML value mlfdd_clearall(value foo) /* ML */
//{
//  fdd_clearall();
//  
//  return Val_unit;
//}
//
///* ML type: unit -> int */
//EXTERNML value mlfdd_domainnum(value foo) /* ML */
//{
//  return Val_int(fdd_domainnum());
//}
//
///* ML type: fddvar -> int */
//EXTERNML value mlfdd_domainsize(value var) /* ML */
//{
//  return Val_int(fdd_domainsize(Int_val(var)));
//}
//
///* ML type: fddvar -> int */
//EXTERNML value mlfdd_varnum(value var) /* ML */
//{
//  return Val_int(fdd_varnum(Int_val(var)));
//}
//
///* ML type: fddvar -> varnum vector */
//EXTERNML value mlfdd_vars(value var) /* ML */
//{
//  value result;
//  int *v, n, i;
//
//  n = fdd_varnum(Int_val(var));
//  v = fdd_vars(Int_val(var));
//  
//  if(n == 0)
//    result = Atom(0);  /* The empty vector */
//  else {
//    result = n < Max_young_wosize ? alloc(n, 0) : alloc_shr(n, 0);
//    for (i = 0; i < n; i++) {
//      Field(result, i) = Val_long(v[i]);
//    }
//  }
//
//  return result;
//}
//
///* ML type: bdd -> int vector */
//EXTERNML value mlfdd_scanallvars(value basev,value nv, value r) /* ML */
//{
//  value result;
//  int *v, n, i, b;
//
//  n = Int_val(nv);
//  b = Int_val(basev);
//  v = fdd_scanallvar(Bdd_val(r));
//  
//  if(n == 0 || v== NULL)
//    {// printf("failure\n");
//    result = Atom(0); }  /* The empty vector */
//  else {
//    //    printf("success\n");
//    result = n < Max_young_wosize ? alloc(n, 0) : alloc_shr(n, 0);
//    for (i = 0; i < n; i++) {
//      Field(result, i) = Val_long(v[i+b]);
//    }
//    free(v);
//  }
//
//  return result;
//}
//
///* ML type: fddvar -> fddvar -> fixed -> unit */
//EXTERNML value mlfdd_intaddvarblock(value first, value last, value fixed) /* ML */
//{
//  fdd_intaddvarblock(Int_val(first), Int_val(last), Int_val(fixed));
//  return Val_unit;
//}
//
///* ML type: fddvar -> varSet */
//EXTERNML value mlfdd_ithset(value var) /* ML */
//{
//  return mlbdd_make(fdd_ithset(Int_val(var)));
//}
//
///* ML type: fddvar -> bdd */
//EXTERNML value mlfdd_domain(value var) /* ML */
//{
//  return mlbdd_make(fdd_domain(Int_val(var)));
//}
//
///* ML type: fddvar vector -> varSet */
//EXTERNML value mlfdd_makeset(value vector) /* ML */
//{
//  int size, i, *v;
//  value result;
//
//  size = Wosize_val(vector);
//
//  /* we use stat_alloc which guarantee that we get the memory (or it
//     will raise an exception). */
//  v  = (int *) stat_alloc(sizeof(int) * size);
//  for (i=0; i<size; i++) {
//     v[i] = Int_val(Field(vector, i));
//  }
//
//  result = mlbdd_make(fdd_makeset(v, size));
// 
//  /* memory allocated with stat_alloc, should be freed with
//     stat_free.*/
//  stat_free((char *) v);
//
//  return result;
//}
//
//
///* ML type: fddvar vector -> fddvar vector -> pairSet */
//EXTERNML value mlfdd_setpairs(value oldvar, value newvar) /* ML */
//{
//  int size, i, *o, *n;
//  bddPair *pairs;
//  value result;
//
//  size = Wosize_val(oldvar);
//
//  /* we use stat_alloc which guarantee that we get the memory (or it
//     will raise an exception). */
//  o    = (int *) stat_alloc(sizeof(int) * size);
//  n    = (int *) stat_alloc(sizeof(int) * size);
//
//  for (i=0; i<size; i++) {
//     o[i] = Int_val(Field(oldvar, i));
//     n[i] = Int_val(Field(newvar, i));
//  }
//
//  pairs = bdd_newpair();
//  fdd_setpairs(pairs, o, n, size);
//
//  /* memory allocated with stat_alloc, should be freed with
//     stat_free.*/
//  stat_free((char *) o);
//  stat_free((char *) n);
//
//  result = mlbdd_alloc_final(2, &mlbdd_pair_finalize);
//  PairSet_val(result) = pairs;
//
//  return result;
//}
//
///* ML type: fddvar -> int -> bdd */
//EXTERNML value mlfdd_ithvar(value var, value val) /* ML */
//{
//  return mlbdd_make(fdd_ithvar(Int_val(var), Int_val(val)));  
//}
//
//
///* ML type: fddvar -> int -> bdd */
//EXTERNML value mlfdd_equals(value var1, value var2) /* ML */
//{
//  return mlbdd_make(fdd_equals(Int_val(var1), Int_val(var2)));  
//}
//
//
//
///* BVEC FUNCTIONS */
//#define bvecbitnum_val(x) (((int *) (x))  [1]) // Also an l-value
//#define bvecbitvec_val(x) (((BDD **) (x)) [2]) // Also an l-value
//
//static INLINE BVEC BVEC_val(value obj) {
//  BVEC t;
//  t.bitnum=bvecbitnum_val(obj);
//  t.bitvec=bvecbitvec_val(obj);
//  return t;
//}
//
///* When the bvec becomes unreachable from the ML process, it will be
//   garbage-collected, mlbdd_finalize_bvec() will be called on the bvec,
//   which will do the necessary bvec-bookkeeping.  */
//void mlbdd_finalize_bvec(value obj) 
//{
//  bvec_free(BVEC_val(obj));
//}
//
///* Creation of a bvec makes a finalized pair (mlbdd_finalize, bitnum, bitvec) */
//EXTERNML value mlbdd_make_bvec(BVEC v) 
//{
//  value res;
//  res = mlbdd_alloc_final(3, &mlbdd_finalize_bvec);
//  bvecbitnum_val(res) = v.bitnum; 
//  bvecbitvec_val(res) = v.bitvec;  /* Hopefully a pointer fits in a long */
//  return res;
//}
//
///* ML type: precision -> bvec */
//EXTERNML value mlbvec_true(value bits) {
//  return mlbdd_make_bvec(bvec_true(Int_val(bits)));
//}
//
///* ML type: precision -> bvec */
//EXTERNML value mlbvec_false(value bits) {
//  return mlbdd_make_bvec(bvec_false(Int_val(bits)));
//}
//
///* ML type: precision -> const -> bvec */
//EXTERNML value mlbvec_con(value bits, value val) /* ML */
//{
//  return mlbdd_make_bvec(bvec_con(Int_val(bits), Int_val(val)));
//}
//
///* ML type: precision -> varnum -> int -> bvec */
//EXTERNML value mlbvec_var(value bits, value var, value step) /* ML */
//{
//  return mlbdd_make_bvec(bvec_var(Int_val(bits), Int_val(var), Int_val(step)));
//}
//
///* ML type:  bvecvar -> bvec */
//EXTERNML value mlbvec_varfdd(value var) /* ML */
//{
//  return mlbdd_make_bvec(bvec_varfdd(Int_val(var)));
//}
//
///* ML type: precision -> bvec -> bvec */
//EXTERNML value mlbvec_coerce(value bits, value v) /* ML */
//{
//  return mlbdd_make_bvec(bvec_coerce(Int_val(bits), BVEC_val(v)));
//}
//
///* ML type: bvec -> bool */
//EXTERNML value mlbvec_isconst(value v) /* ML */
//{
//  return bvec_isconst(BVEC_val(v)) ? Val_true : Val_false;
//}
//
///* ML type: bvec -> bool */
//EXTERNML value mlbvec_getconst(value v) /* ML */
//{
//  if(bvec_isconst(BVEC_val(v))) {
//    return Val_int(bvec_val(BVEC_val(v)));
//  }
//  else {
//    failwith("The bvec does not represent a single constant.");
//    return Val_unit; /* unreachable, here to prevent warnings */
//  }
//}
//
///* ML type: bvec -> bvec -> bvec */
//EXTERNML value mlbvec_add(value s1, value s2) /* ML */
//{
//  return mlbdd_make_bvec(bvec_add(BVEC_val(s1), BVEC_val(s2)));
//}
//
///* ML type: bvec -> bvec -> bvec */
//EXTERNML value mlbvec_sub(value s1, value s2) /* ML */
//{
//  return mlbdd_make_bvec(bvec_sub(BVEC_val(s1), BVEC_val(s2)));
//}
//
///* ML type: bvec -> const -> bvec */
//EXTERNML value mlbvec_mulfixed(value s1, value con) /* ML */
//{
//  return mlbdd_make_bvec(bvec_mulfixed(BVEC_val(s1), Int_val(con)));
//}
//
///* ML type: bvec -> bvec -> bvec */
//EXTERNML value mlbvec_mul(value s1, value s2) /* ML */
//{
//  return mlbdd_make_bvec(bvec_mul(BVEC_val(s1), BVEC_val(s2)));
//}
//
//#ifdef CUDDY
///* ML type: bvec -> const -> bvec * bvec */
//EXTERNML value mlbvec_divfixed(value s1, value con) /* ML */
//{
//  CAMLparam2(s1, con);
//  CAMLlocal1(result);
//  BVEC res, rem;
//  bvec_divfixed(BVEC_val(s1), Int_val(con), &res, &rem);
//  result = alloc_tuple(2);
//  Store_field(result, 0, mlbdd_make_bvec(res)); 
//  Store_field(result, 1, mlbdd_make_bvec(rem)); 
//  CAMLreturn(result);
//}
//
///* ML type: bvec -> bvec -> bvec * bvec  */
//EXTERNML value mlbvec_div(value s1, value s2) /* ML */
//{
//  CAMLparam2(s1, s2);
//  CAMLlocal1(result);
//  BVEC res, rem;
//  bvec_div(BVEC_val(s1), BVEC_val(s2), &res, &rem);
//  result = alloc_tuple(2);
//  Store_field(result, 0, mlbdd_make_bvec(res)); 
//  Store_field(result, 1, mlbdd_make_bvec(rem)); 
//  CAMLreturn(result);
//}
//
//#else
///* ML type: bvec -> const -> bvec * bvec */
//EXTERNML value mlbvec_divfixed(value s1, value con) /* ML */
//{
//  BVEC res, rem;
//  Push_roots(result, 1);
//    bvec_divfixed(BVEC_val(s1), Int_val(con), &res, &rem);
//    result[0] = alloc_tuple(2);
//    Field(result[0], 0) = 0;
//    Field(result[0], 1) = 0;
//    Field(result[0], 0) = mlbdd_make_bvec(res); 
//    Field(result[0], 1) = mlbdd_make_bvec(rem); 
//  Pop_roots();
//  return result[0];
//}
//
///* ML type: bvec -> bvec -> bvec * bvec  */
//EXTERNML value mlbvec_div(value s1, value s2) /* ML */
//{
//  BVEC res, rem;
//  Push_roots(result, 1);
//    bvec_div(BVEC_val(s1), BVEC_val(s2), &res, &rem);
//    result[0] = alloc_tuple(2);
//    Field(result[0], 0) = 0;
//    Field(result[0], 1) = 0;
//    Field(result[0], 0) = mlbdd_make_bvec(res); 
//    Field(result[0], 1) = mlbdd_make_bvec(rem); 
//  Pop_roots();
//  return result[0];
//}
//#endif
//
///* ML type: bvec -> bvec -> bdd -> bvec */
//EXTERNML value mlbvec_shl(value s1, value c, value b) /* ML */
//{
//  return mlbdd_make_bvec(bvec_shl(BVEC_val(s1), BVEC_val(c), Bdd_val(b)));
//}
//
///* ML type: bvec -> const -> bdd -> bvec */
//EXTERNML value mlbvec_shlfixed(value s1, value c, value b) /* ML */
//{
//  return mlbdd_make_bvec(bvec_shlfixed(BVEC_val(s1), Int_val(c), Bdd_val(b)));
//}
//
///* ML type: bvec -> bvec -> bdd -> bvec */
//EXTERNML value mlbvec_shr(value s1, value c, value b) /* ML */
//{
//  return mlbdd_make_bvec(bvec_shr(BVEC_val(s1), BVEC_val(c), Bdd_val(b)));
//}
//
///* ML type: bvec -> const -> bdd -> bvec */
//EXTERNML value mlbvec_shrfixed(value s1, value c, value b) /* ML */
//{
//  return mlbdd_make_bvec(bvec_shrfixed(BVEC_val(s1), Int_val(c), Bdd_val(b)));
//}
//
///* ML type: bvec -> bvec -> bdd */
//EXTERNML value mlbvec_lth(value s1, value s2) /* ML */
//{
//  return mlbdd_make(bvec_lth(BVEC_val(s1), BVEC_val(s2)));
//}
//
///* ML type: bvec -> bvec -> bdd */
//EXTERNML value mlbvec_lte(value s1, value s2) /* ML */
//{
//  return mlbdd_make(bvec_lte(BVEC_val(s1), BVEC_val(s2)));
//}
//
///* ML type: bvec -> bvec -> bdd */
//EXTERNML value mlbvec_gth(value s1, value s2) /* ML */
//{
//  return mlbdd_make(bvec_gth(BVEC_val(s1), BVEC_val(s2)));
//}
//
///* ML type: bvec -> bvec -> bdd */
//EXTERNML value mlbvec_gte(value s1, value s2) /* ML */
//{
//  return mlbdd_make(bvec_gte(BVEC_val(s1), BVEC_val(s2)));
//}
//
///* ML type: bvec -> bvec -> bdd */
//EXTERNML value mlbvec_equ(value s1, value s2) /* ML */
//{
//  return mlbdd_make(bvec_equ(BVEC_val(s1), BVEC_val(s2)));
//}
//
///* ML type: bvec -> bvec -> bdd */
//EXTERNML value mlbvec_neq(value s1, value s2) /* ML */
//{
//  return mlbdd_make(bvec_neq(BVEC_val(s1), BVEC_val(s2)));
//}
