//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

#include "minisat_ifc.h"

typedef char *string;
#include "prefs_ext.h"

using namespace Minisat;

static Solver *Sp;
static Solver *TC_Sp;

// Routine to be used to handle SIGXCPU exceptions
static void
signal_interrupt(int signum)
{
    (void) signum;
    Sp->interrupt();
    signal (SIGVTALRM, signal_interrupt);
}

extern "C" void Eprintf(const char *fmt, ...);
extern "C" void Wprintf(const char *fmt, ...);

extern "C" void TC_Sat_Init();
void
TC_Sat_Init()
{
    TC_Sp = new(Solver);
    TC_Sp->random_seed = 1234;
    TC_Sp->verbosity = 0;
}

extern "C" void TC_Sat_Free();
void
TC_Sat_Free()
{
    delete TC_Sp;
}

extern "C" void DBG_TC_toDimacs();
void
DBG_TC_toDimacs(char *file)
{
    TC_Sp->toDimacs(file);
}

static int
max(int i, int j)
{
    if( i >= j ) return i; else return j;
}

extern "C" int TC_Sat_NewVar();
int
TC_Sat_NewVar()
{
    int res = (int) (TC_Sp->newVar());
    return (res+2);
}

extern "C" int TC_Sat_Not(int l);
int
TC_Sat_Not(int f)
{
    switch( f ) {
	case 0:
	    return 1;
	case 1:
	    return 0;
	default:
	    return( -1*f );
    }
}

extern "C" int TC_Sat_Or(int l, int r);
int
TC_Sat_Or(int l, int r)
{
    if( l == 0 ) return r;
    if( r == 0 ) return l;
    if( l == 1) return 1;
    if( r == 1) return 1;
    int vars = max(abs(l), abs(r));
    while (vars >= TC_Sp->nVars()) {
	TC_Sp->newVar();
    }
    // Now add a new variable for the output of the AND
    int res = (int) (TC_Sp->newVar());
    // Now for g = l \/ r, add the clauses 
    // (g \/ a'), (g \/ b'), and (g' \/ a \/ b)
    Lit	lres = mkLit(res); 
    Lit ll   = (l < 0)? ~mkLit(abs(l)-2) : mkLit(abs(l)-2);
    Lit lr   = (r < 0)? ~mkLit(abs(r)-2) : mkLit(abs(r)-2);
    vec<Lit> c1, c2, c3;
    c1.push(lres); c1.push(~ll); TC_Sp->addClause_(c1);
    c2.push(lres); c2.push(~lr); TC_Sp->addClause_(c2);
    c3.push(~lres); c3.push(ll); c3.push(lr); TC_Sp->addClause_(c3);
    return (res+2);
}

extern "C" int TC_Sat_And(int l, int r);
int
TC_Sat_And(int l, int r)
{
    if( l == 0 ) return 0;
    if( r == 0 ) return 0;
    if( l == 1) return r;
    if( r == 1) return l;
    int vars = max(abs(l), abs(r));
    while (vars >= TC_Sp->nVars()) {
	TC_Sp->newVar();
    }
    // Now add a new variable for the output of the AND
    int res = (int) (TC_Sp->newVar());
    // Now for g = l /\ r, add the clauses 
    // (g' \/ a), (g' \/ b), and (g \/ a' \/ b')
    Lit	lres = mkLit(res); 
    Lit ll   = (l < 0)? ~mkLit(abs(l)-2) : mkLit(abs(l)-2);
    Lit lr   = (r < 0)? ~mkLit(abs(r)-2) : mkLit(abs(r)-2);
    vec<Lit> c1, c2, c3;
    c1.push(~lres); c1.push(ll); TC_Sp->addClause_(c1);
    c2.push(~lres); c2.push(lr); TC_Sp->addClause_(c2);
    c3.push(lres); c3.push(~ll); c3.push(~lr); TC_Sp->addClause_(c3);
    return (res+2);
}

extern "C" int TC_Sat_Model(int cond, int *assumptions, int n);
// Determine if there is a model given the assumptions
// Returns: 0 if the model is UNSAT assuming the assumptions
//	    1 if there is a model satisfying the assumptions
int
TC_Sat_Model(int cond, int *assumptions, int n)
{
    if (!TC_Sp->simplify()) {
	return 0;
    }
    vec<Lit> as;
    int *ip = assumptions;
    for(int i = 0; i < n; i++) {
	int var = abs(*ip)-2;
        while (var >= TC_Sp->nVars())  {
	    TC_Sp->newVar();
	}
        as.push( (*ip > 0)? mkLit(var) : ~mkLit(var) );
	ip++;
    }
    int var = abs(cond)-2;
    while (var >= TC_Sp->nVars())  { TC_Sp->newVar(); }
    as.push( (cond > 0)? mkLit(var) : ~mkLit(var) );
    bool ret = TC_Sp->solve(as);
    return (ret? 1 : 0);
}

extern "C" int TC_Sat_Get_Model_Value(int var);
int
TC_Sat_Get_Model_Value(int var)
{
    lbool res = TC_Sp->modelValue(var-2);
    if( res == l_False ) return 0;
    if( res == l_True )  return 1;
    if( res == l_Undef ) return 2;
    return 3;
}


extern "C" void Sat_Init();
void
Sat_Init()
{
    Sp = new(Solver);
    Sp->random_seed = 1234;
    Sp->verbosity = 0;
    struct sigaction action;
    memset(&action, '\0', sizeof(action));
    action.sa_handler = signal_interrupt;
    sigaction(SIGXCPU, &action, NULL);
}

extern "C" int SAT_Add_Var();
int
SAT_Add_Var()
{
    // Now add a new variable for the output of the AND
    int res = (int) (Sp->newVar());
    return (res+1);
}

extern "C" int SAT_Add_AND_Clauses(int l, int r);
int
SAT_Add_AND_Clauses(int l, int r)
{
    int vars = max(abs(l), abs(r));
    while (vars >= Sp->nVars()) {
	Sp->newVar();
    }
    // Now add a new variable for the output of the AND
    int res = (int) (Sp->newVar());
    // Now for g = l /\ r, add the clauses 
    // (g' \/ a), (g' \/ b), and (g \/ a' \/ b')
    Lit	lres = mkLit(res); 
    Lit ll   = (l < 0)? ~mkLit(abs(l)-1) : mkLit(abs(l)-1);
    Lit lr   = (r < 0)? ~mkLit(abs(r)-1) : mkLit(abs(r)-1);
    vec<Lit> c1, c2, c3;
    c1.push(~lres); c1.push(ll); Sp->addClause_(c1);
    c2.push(~lres); c2.push(lr); Sp->addClause_(c2);
    c3.push(lres); c3.push(~ll); c3.push(~lr); Sp->addClause_(c3);
    return (res+1);
}

static void
set_SAT_time_limit(int seconds)
{
    struct itimerval it;

    /* make sure handler is installed   */
    signal (SIGVTALRM, signal_interrupt);

    it.it_value.tv_sec     = seconds;
    it.it_value.tv_usec    = 0;
    it.it_interval.tv_sec  = 0;
    it.it_interval.tv_usec = 0;

    if (setitimer (ITIMER_VIRTUAL, &it, NULL) < 0) {
        fprintf(stderr,"ITIMER is broken!\n");
	exit(-1);
    }
}

extern "C" int SAT_and_is_same(int l, int r, int c);
int
SAT_and_is_same(int l, int r, int c)
{
    if( RC_BEXPR_SAT_LIMIT == 0 ) return( 0 );

    if (!Sp->simplify()) {
	Eprintf("Inconsistent model");
    }

    vec<Lit> as;
    // Try assuming three cases:
    //	l=r=1 and c=0
    //  l=0 and c=1
    //  r=0 and c=1
    // If these cases all lead to a UNSAT systems, then c == (l & r)
    //
    Lit l_l = (l < 0)? ~mkLit(abs(l)-1) : mkLit(abs(l)-1);
    Lit l_r = (r < 0)? ~mkLit(abs(r)-1) : mkLit(abs(r)-1);
    Lit l_c = (c < 0)? ~mkLit(abs(c)-1) : mkLit(abs(c)-1);
    vec<Lit>	assumes;

    assumes.push(l_l);
    assumes.push(l_r);
    assumes.push(~l_c);
    set_SAT_time_limit( RC_BEXPR_SAT_LIMIT );
    lbool rr = Sp->solveLimited(assumes);
    set_SAT_time_limit( 0 );
    if( (rr == l_True) || (rr == l_Undef) ) { return 0; }

    assumes.clear();
    assumes.push(~l_l);
    assumes.push(l_c);
    set_SAT_time_limit( RC_BEXPR_SAT_LIMIT );
    rr = Sp->solveLimited(assumes);
    set_SAT_time_limit( 0 );
    if( rr == l_True || rr == l_Undef ) { return 0; }

    assumes.clear();
    assumes.push(~l_r);
    assumes.push(l_c);
    set_SAT_time_limit( RC_BEXPR_SAT_LIMIT );
    rr = Sp->solveLimited(assumes);
    set_SAT_time_limit( 0 );
    if( rr == l_True || rr == l_Undef ) { return 0; }

    return 1;
}

extern "C" int Find_model(int *assumptions, int n, int time_limit);
// Determine if there is a model given the assumptions
// Returns: -1 if the model is UNSAT by itself
//	     0 if the model is UNSAT if assumptions are true
//	     1 if there is a model satisfying the assumptions
//	     2 if minisat ran out of time/resources
int
Find_model(int *assumptions, int n, int time_limit)
{
    if (!Sp->simplify()) {
	return -1;
    }
    vec<Lit> as;
    int *ip = assumptions;
    for(int i = 0; i < n; i++) {
	int var = abs(*ip)-1;
        while (var >= Sp->nVars())  {
	    Sp->newVar();
	}
        as.push( (*ip > 0)? mkLit(var) : ~mkLit(var) );
	ip++;
    }
    set_SAT_time_limit( time_limit );
    lbool ret = Sp->solveLimited(as);
    set_SAT_time_limit( 0 );
    if( ret == l_True ) return 1;
    if( ret == l_False) return 0;
    return 2;
}


extern "C" int Get_Model_Value(int var);
int
Get_Model_Value(int var)
{
    lbool res = Sp->modelValue(var-1);
    if( res == l_False ) return 0;
    if( res == l_True )  return 1;
    if( res == l_Undef ) return 2;
    return 3;
}
