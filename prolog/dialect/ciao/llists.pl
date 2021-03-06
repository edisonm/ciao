/*  Part of Ciao Prolog compatibility library

    Author:        The Ciao Development Team, ported by Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           http://www.edisonm.com
    Copyright (C): 2013, Process Design Center, Breda, The Netherlands.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(llists, [collect_singletons/2], [assertions]).

:- reexport(library(lists), [append/2, flatten/2]).

%% :- reexport(library(clpfd), [transpose/2]). % BUG: this cause
%%                                             % problems, I do not know
%%                                             % why --EMM

:- pred collect_singletons(+list(list), ?(list))
        # "Collects in a list the singletons lists appearing in a list
          of lists.".

collect_singletons([],[]).
collect_singletons([[X]|Xss],[X|Ys]):- !,
	collect_singletons(Xss,Ys).
collect_singletons([_|Xss],Ys):-
	collect_singletons(Xss,Ys).
