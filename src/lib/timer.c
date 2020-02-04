//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

#include "timer.h"

/************************************************************************/
/*                      Private Variables                               */
/************************************************************************/
static struct timezone	tzp;

/************************************************************************/
/*                      Public Functions                                */
/************************************************************************/

void
Start_wtimer(wtimer_ptr tp)
{
    gettimeofday (&(tp->start), &tzp);
    tp->initialized = TIMER_MAGIC_NBR;
}

void
Stop_wtimer(wtimer_ptr tp)
{
    ASSERT(tp->initialized == TIMER_MAGIC_NBR);
    gettimeofday (&(tp->stop), &tzp);
    tp->initialized = STOP_TIMER;
}

int
Get_wseconds(wtimer_ptr tp)
{
    struct timeval cur;
    struct timeval *tvp;

    if(tp->initialized == STOP_TIMER) {
	tvp = &(tp->stop);
    } else {
	gettimeofday(&cur, &tzp);
	tvp = &cur;
    }

    if((tp->start).tv_usec > tvp->tv_usec)
	return( tvp->tv_sec - (tp->start).tv_sec - 1 );
    else
	return( tvp->tv_sec - (tp->start).tv_sec );
}

int
Get_wmicroseconds(wtimer_ptr tp)
{
    struct timeval cur;
    struct timeval *tvp;

    if(tp->initialized == STOP_TIMER) {
	tvp = &(tp->stop);
    } else {
	gettimeofday(&cur, &tzp);
	tvp = &cur;
    }

    if((tp->start).tv_usec > tvp->tv_usec)
	return( (1000000 + tvp->tv_usec - (tp->start).tv_usec) );
    else
	return( (tvp->tv_usec - (tp->start).tv_usec) );
}

void
Start_timer(timer_ptr tp)
{
    getrusage(RUSAGE_SELF, &(tp->start));
    tp->initialized = TIMER_MAGIC_NBR;
}

void
Stop_timer(timer_ptr tp)
{
    ASSERT(tp->initialized == TIMER_MAGIC_NBR);
    getrusage(RUSAGE_SELF, &(tp->stop));
    tp->initialized = STOP_TIMER;
}

int
Get_seconds(timer_ptr tp)
{
    struct rusage cur;
    struct rusage *tvp;

    if(tp->initialized == STOP_TIMER) {
	tvp = &(tp->stop);
    } else {
	getrusage(RUSAGE_SELF, &cur);
	tvp = &cur;
    }
    if(tp->start.ru_utime.tv_usec > tvp->ru_utime.tv_usec)
	return( tvp->ru_utime.tv_sec - tp->start.ru_utime.tv_sec - 1 );
    else
	return( tvp->ru_utime.tv_sec - tp->start.ru_utime.tv_sec );
}

int
Get_microseconds(timer_ptr tp)
{
    struct rusage cur;
    struct rusage *tvp;

    if(tp->initialized == STOP_TIMER) {
	tvp = &(tp->stop);
    } else {
	getrusage(RUSAGE_SELF, &cur);
	tvp = &cur;
    }
    if(tp->start.ru_utime.tv_usec > tvp->ru_utime.tv_usec)
	return(1000000 + tvp->ru_utime.tv_usec - tp->start.ru_utime.tv_usec);
    else
	return(tvp->ru_utime.tv_usec - tp->start.ru_utime.tv_usec);
}

