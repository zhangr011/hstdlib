-ifndef(HTTP_CLIENT_HRL).
-define(HTTP_CLIENT_HRL,true).

-define(HTTP_CLIENT_WORKER_NUMBER, 10).
-define(HTTP_CLIENT_TIMEOUT, 10000).
-define(HTTP_CLIENT_OPTIONS, [{max_sessions, 100}, {max_pipeline_size, 10}]).

-endif.
