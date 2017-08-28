-module(sup).
-behavior(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    DataPath = truly:config(data_path, "/root/data/caller-id.csv"),

    Db = {db, {db, start_link, [DataPath]},
        transient, 1000, worker, [db]},

    {ok, {{one_for_one, 10, 30}, [Db]}}.