-ifdef(debug).
-define(rdbg(Term), error_logger:error_msg(Term)).
-else.
-define(rdbg(_Term), void).
-endif.

-define(idbg(FmtStr, Err), error_logger:info_msg("~p (line ~p, pid ~p): " FmtStr "~n", [?MODULE, ?LINE, self() | Err])).
