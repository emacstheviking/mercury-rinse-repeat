%-----------------------------------------------------------------------------%
%
% File: rr.m
% Main author: Sean Charles
% Date: Sat Aug 13 16:07:42 2022
%
% Rinse repeat. Having gotten fed up with the sheer amount of baggage that a
% lot of 'file watchers' come with (forget NPM, not going on my machine ever)
% I decided that what I needed was something utterly simple, that doesn't grind
% the CPU (mine sleeps to yield) and that just does what it says on the tin, I
% offer `rr`. Rinse, repeat.
%
% Once installed and on your OS path, all you do is:
%
%    $ rr "command-to-run" list-of-files-to-trigger-a-change
%
% The list can include wildcards, but I am relying on the operating system to
% expand that list prior to beginning execution i.e. the code itself does not
% do anything when given "*.c", the OS would have scanned the local folder and
% handed over all files ending with ".c" as the command line argument list.
%
% My particular use case was to work in conjunction with my own hand-rolled
% literate programming tool, `mlpt`, so all I needed was to look for changes to
% the Markdown source file and then trigger a rebuild of the Mercury sources in
% the file and also to regenerate the PDF documentation at the same time.
%
% rr does all of that for me so all I need to do is sit and think and work!
%
% Changed: 23 Sep 2023 07:27:08 BST -- use nanosleep().
%
%-----------------------------------------------------------------------------%
:- module rr.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module calendar.
:- import_module list.
:- import_module maybe.
:- import_module string.

main(!IO) :-
    io.command_line_arguments(Args, !IO),

    ( if Args = [ Rinser | Files ],
        list.is_not_empty(Files)
    then
        io.format("run: %s when changes in %s\n",
            [s(Rinser), s(string(Files))], !IO),

        statfiles(Files, Sres, !IO),
        (
            Sres = yes(Stats),
            watch_files(Rinser, Files, Stats, !IO)
        ;
            Sres = no,
            io.format("Oops! Did all these files exist?\n", [], !IO),
            io.set_exit_status(1, !IO)
        )
    else
        io.format("Usage :- rr COMMAND FILES\n", [], !IO)
    ).

%----------------------------------------------------------------------------%

    % Watch a bunch of files for any changes to them.
    % Stats is the initial set of statinfo() records, we ask for them again
    % in the same order, and if any are changed, we update the list and return
    % an indicator of a change.
    %
    % CTRL-C is the only way to kill this application now!
    %
:- pred watch_files(string::in, list(string)::in, list(statinfo)::in,
    io::di, io::uo) is det.

watch_files(Command, Files, Stats, !IO) :-
    % M1 mac-mini: averages 0.0 to 0.1% CPU time but feels 'responsive'.
    nanosleep(1200000, !IO),
    statfiles(Files, Sres, !IO),
    (
        Sres = yes(NewStats),
        check_changes(Stats, NewStats, Changed, !IO),
        ( if list.is_not_empty(Changed) then
            io.format("\nrr --> ", [], !IO),
            io.write_list(Changed, ", ", io.print, !IO),
            io.format("\nrr --> ""%s""\n\n", [s(Command)], !IO),
            io.flush_output(!IO),
            io.call_system(Command, _, !IO),
            io.format("\nrr --> OK\n", [], !IO),
            %> uncomment for detailed debugging.
            % show_changes(Stats, NewStats, !IO),
            watch_files(Command, Files, NewStats, !IO)
        else
            watch_files(Command, Files, Stats, !IO)
        )
    ;
        Sres = no,
        io.format("Ending, something went wrong!\n", [], !IO)
    ).    

%----------------------------------------------------------------------------%

    % Check for change based only on 'mtime' values.
    %
:- pred check_changes(list(statinfo)::in, list(statinfo)::in,
    list(string)::out, io::di, io::uo) is det.

check_changes(Old, New, Changed, !IO) :-
    Check = (pred(
        Pair::in, !.Ch::in, !:Ch::out, !.IO::di, !:IO::uo
    ) is det :-
        Pair0 = list.det_index0(Pair, 0),
        Pair1 = list.det_index0(Pair, 1),
        Mtime0 = si_mtime(Pair0),
        Mtime1 = si_mtime(Pair1),
        ( if Mtime0 = Mtime1 then
            true
        else
            list.cons(si_file(Pair0), !Ch)
        )
    ),
    list.foldl2(Check, list.chunk(list.zip(Old, New), 2), [], Changed, !IO).

%----------------------------------------------------------------------------%

    % Display a statinfo() against its changed counterpart.

:- pred show_changes(list(statinfo)::in, list(statinfo)::in,
    io::di, io::uo) is det.

show_changes(Old, New, !IO) :-
    P = (pred(Pair::in, !.IO::di, !:IO::uo) is det :-
        Pair0 = list.det_index0(Pair, 0),
        Pair1 = list.det_index0(Pair, 1),
        show_statinfo(Pair0, !IO),
        show_statinfo(Pair1, !IO),
        io.nl(!IO)
    ),
    list.foldl(P, list.chunk(list.zip(Old, New), 2), !IO).

:- pred show_statinfo(statinfo::in, io::di, io::uo) is det.

show_statinfo(S, !IO) :-
    io.format("%s %s %s %s\n",
        [s(si_file(S)),
         s(string(si_atime(S))),
         s(string(si_mtime(S))),
         s(string(si_ctime(S)))
        ], !IO).

%----------------------------------------------------------------------------%

    % Given a list of files, return a list stat structures.
    %
:- pred statfiles(list(string)::in, maybe(list(statinfo))::out,
    io::di, io::uo) is det.

statfiles(Files, Out, !IO) :-
    P = (pred(F::in, !.OK::in, !:OK::out, !.IO::di, !:IO::uo) is det :-
        filestat(F, Res, !IO),
        (
            Res = io.ok(Stat),
            cons(Stat, !OK)
        ;
            Res = io.error(Error),
            io.format("File error: %s: %s\n",
                [s(io.error_message(Error)), s(F)], !IO)
        )
    ),
    list.foldl2(P, Files, [], Stats, !IO),
    ( if list.same_length(Files, Stats) then
        Out = yes(Stats)
    else
        Out = no
    ).

%----------------------------------------------------------------------------%
%
% FFI "C" Routines to stat() a file and usleep() for a while.
%
%----------------------------------------------------------------------------%

    % A Statinfo to return something about our file.
    %
:- type statinfo
    --->    statinfo(
                si_file::string,
                si_size::int,
                si_atime::date,
                si_mtime::date,
                si_ctime::date
            ).

:- pragma foreign_decl("C", "
    #include <sys/stat.h>
    #include <stdio.h>
    #include <time.h>
    #include <unistd.h>
").

%----------------------------------------------------------------------------%

    % Create an io.res error response.
    %
:- func make_error(int::in, string::in) = (io.res::out) is det.

make_error(E, Why) = Out :-
    ( if E = 0 then
        Out = ok
    else
        Out = io.error(io.make_io_error(Why))
    ).

:- pragma foreign_export("C", make_error(in, in) = (out), "makeError").

%----------------------------------------------------------------------------%

    % Convert a bunch of integers to a calendar.date value.
    %
:- func to_date(int::in, int::in, int::in, int::in, int::in, int::in)
    = (date::out) is det.

to_date(Yr, Mo, Dy, Hr, Mi, Se) = Date :-
    Month = calendar.det_int_to_month(Mo),
    Date = calendar.det_init_date(Yr, Month, Dy, Hr, Mi, Se, 0).

:- pragma foreign_export("C", to_date(in, in, in, in, in, in)
    = (out), "toDate").


%----------------------------------------------------------------------------%

    % Make the io.res(statinfo) response given the values from the
    % stat structure for the file.
    %
:- func make_response(string::in, int::in, date::in, date::in, date::in)
    = (io.res(statinfo)::out) is det.

make_response(File, Size, ATime, MTime, CTime)
    = ok(statinfo(File, Size, ATime, MTime, CTime)).

:- pragma foreign_export("C", make_response(in, in, in, in, in)
    = (out), "statResponse").

%----------------------------------------------------------------------------%

    % The wrapper around "C" stat() to get file information.
    %
:- pred filestat(string::in, io.res(statinfo)::out, io::di, io::uo) is det.

:- pragma foreign_proc(
    "C", filestat(File::in, Error::out, _IO0::di, _IO::uo),
    [   promise_pure
    ,   thread_safe
    ,   will_not_throw_exception
    ,   will_not_modify_trail
    ,   does_not_affect_liveness
    ,   tabled_for_io
    ],
    "
    struct stat buffer;
    int         status;
    status = stat(File, &buffer);

    if (status) {
        Error = makeError(errno, strerror(errno));
    }
    else {
        struct tm atime, mtime, ctime;
        localtime_r(&buffer.st_atime, &atime);
        localtime_r(&buffer.st_mtime, &mtime);
        localtime_r(&buffer.st_ctime, &ctime);

        Error = statResponse(
            File,
            buffer.st_size,
            toDate(
                atime.tm_year, atime.tm_mon, atime.tm_mday,
                atime.tm_hour, atime.tm_min, atime.tm_sec
            ),
            toDate(
                mtime.tm_year, mtime.tm_mon, mtime.tm_mday,
                mtime.tm_hour, mtime.tm_min, mtime.tm_sec
            ),
            toDate(
                ctime.tm_year, ctime.tm_mon, ctime.tm_mday,
                ctime.tm_hour, ctime.tm_min, ctime.tm_sec
            )
        );
    }
    "
).

%----------------------------------------------------------------------------%

    % Sleep for N uSeconds.
    % This is used to give the CPU a break between rounds of the scanning
    % loop whilst looking for changes.
    %
:- pred nanosleep(int::in, io::di, io::uo) is det.

:- pragma foreign_proc(
    "C", nanosleep(NanoSecs::in, _IO0::di, _IO::uo),
    [   promise_pure
    ,   thread_safe
    ,   will_not_throw_exception
    ,   will_not_modify_trail
    ,   does_not_affect_liveness
    ,   tabled_for_io
    ],
    "
    int secs = NanoSecs / 1000000;
    int nsecs = NanoSecs - ( secs * 1000000 );
    struct timespec speci = { secs, nsecs };
    nanosleep(&speci, NULL);
    "
).

%----------------------------------------------------------------------------%
:- end_module rr.
%----------------------------------------------------------------------------%
