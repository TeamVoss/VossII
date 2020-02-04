//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 1993                        *
*                                                                   *
*********************************************************************/
/* timer.h -- header for timer.c */

#ifndef _TIMER_H
#define _TIMER_H
#include <sys/time.h>
#include <sys/resource.h>
#include "types.h"
#include "alloc.h"

#define TIMER_MAGIC_NBR	589210521
#define STOP_TIMER	221541

typedef struct wtimer_rec {
	int		initialized;
	struct timeval	start;
	struct timeval	stop;
} wtimer_rec, *wtimer_ptr;

typedef struct timer_rec {
	int		initialized;
	struct rusage	start;
	struct rusage	stop;
} timer_rec, *timer_ptr;

void	Start_timer(timer_ptr tp);
void	Stop_timer(timer_ptr tp);
int	Get_seconds(timer_ptr tp);
int	Get_microseconds(timer_ptr tp);

void	Start_wtimer(wtimer_ptr tp);
void	Stop_wtimer(wtimer_ptr tp);
int	Get_wseconds(wtimer_ptr tp);
int	Get_wmicroseconds(wtimer_ptr tp);

#endif /* _TIMER_H */

